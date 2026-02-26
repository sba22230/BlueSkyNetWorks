v <- R.Version()
if (v$major == '4' && v$minor != '0.2') {
  library(bskyr)
  library(furrr)
  library(rgexf)
  library(statnet)
  wrkrs <- max(1, floor(availableCores(constraints = "connections-16") * 0.3))
} else {
  wrkrs = 10
}
library(arrow)
library(DT)
library(DBI)
library(dplyr)
library(ggraph)
library(igraph)
library(intergraph)
library(lubridate)
library(odbc)
library(purrr)
library(readr)
library(retry)
library(RevoScaleR) # RevoScaleR provides RxSqlServerData, rxDataStep, etc.
library(sna)
library(stringi)
library(stringr)
library(tibble)
library(tidyr)
library(visNetwork)

safe_chr <- function(x, ...) {
  val <- purrr::pluck(x, ..., .default = NA_character_)
  if (is.null(val)) NA_character_ else as.character(val)
}
safe_int <- function(x, ...) {
  val <- purrr::pluck(x, ..., .default = NA_integer_)
  if (is.null(val)) NA_integer_ else as.integer(val)
}

# Deduplicate by URI
dedup_posts <- function(posts) {
  posts %>%
    tibble::as_tibble() %>%
    distinct(uri, .keep_all = TRUE)
}
if (v$os != "linux-gnu") {
  # ---------------------------
  # Configuration: SQL Server
  # ---------------------------
  # Edit these values for your environment
  sql_server <- "localhost" # e.g., "localhost\\SQLEXPRESS" or
  # "sqlserver.domain.com"
  database <- "BlueSkyNet"
  use_trusted_connection <- TRUE # set FALSE if using SQL auth
  # shareDir must be accessible by SQL Server compute context
  shareDir <- paste("H:\\AllShare\\", Sys.getenv("USERNAME"), sep = "")
  # change to a path accessible by SQL Server machine
  #sql_user <- "your_sql_user"
  # only used if not using trusted connection
  #sql_password <- "your_password"
  # only used if not using trusted connection
  orgicc <- rxGetComputeContext()
  rxOptions(numCoresToUse = wrkrs)
  rxSetComputeContext("localpar")

  if (use_trusted_connection) {
    odbc_con <- dbConnect(
      odbc::odbc(),
      Driver = "SQL Server",
      Server = sql_server,
      Database = database,
      Trusted_Connection = "Yes"
    )

    connStr <- paste0(
      "Driver={SQL Server};Server=",
      sql_server,
      ";Database=",
      database,
      ";Trusted_Connection=Yes;"
    )
  } else {
    odbc_con <- dbConnect(
      odbc::odbc(),
      Driver = "SQL Server",
      Server = sql_server,
      Database = database,
      UID = sql_user,
      PWD = sql_password
    )

    connStr <- paste0(
      # nolint: object_name_linter.
      "Driver={SQL Server};Server=",
      sql_server,
      ";Database=",
      database,
      ";Uid=",
      sql_user,
      ";Pwd=",
      sql_password,
      ";"
    )
  }
  sql_cc <- RxInSqlServer(
    connectionString = connStr,
    numTasks = wrkrs,
    autoCleanup = TRUE,
    shareDir = shareDir
  )
  # Helper to create RxSqlServerData objects
  rx_sql_table <- function(table_name, connectionString = connStr) {
    RxSqlServerData(
      table = table_name,
      connectionString = connectionString,
      stringsAsFactors = FALSE
    )
  }

  # ODBC connection to read and write objects into SQL
  ds_Graphs <- RxOdbcData(table = "Graph_objects", connectionString = connStr)

  # SQL DDL code to create table if it does not exist
  ddl <- paste(
    " create table [",
    ds_Graphs@table,
    "] (",
    "     [id] varchar(200) not null, ",
    "     [value] varbinary(max), ",
    "     constraint unique_id unique (id))",
    sep = ""
  )
  if (!rxSqlServerTableExists(ds_Graphs@table, ds_Graphs@connectionString)) {
    rxOpen(ds_Graphs, "w")
    rxExecuteSQLDDL(ds_Graphs, ddl)
  }
}
# Single page search with retry
search_page <- function(query, cursor = NULL, limit = 300) {
  retry(
    {
      bs_search_posts(
        query = query,
        limit = limit,
        cursor = cursor,
        clean = FALSE,
        auth = bs_auth
      )
    },
    when = "error",
    max_tries = 5,
    interval = runif(1, 1.0, 2.0)
  )
}

get_posts_from_resp <- function(resp) {
  if (is.list(resp) && !is.null(resp$posts)) {
    resp$posts
  } else if (
    is.list(resp) &&
      length(resp) > 0 &&
      all(vapply(resp, function(x) is.list(x) && !is.null(x$posts), logical(1)))
  ) {
    flatten(map(resp, "posts"))
  } else if (
    is.list(resp) &&
      length(resp) > 0 &&
      all(vapply(resp, function(x) !is.null(x$uri), logical(1)))
  ) {
    resp
  } else {
    list()
  }
}

# Deep pagination with "until" anchoring by last indexedAt
deep_search_posts <- function(
  query,
  hard_limit = 50000,
  chunk_limit = 100,
  checkpoint_path = "data/speirgorm_posts.parquet"
) {
  all_rows <- list()
  cursor <- NULL
  until <- NULL
  fetched <- 0
  iter <- 0
  # âœ… Load existing checkpoint if available
  if (file.exists(checkpoint_path)) {
    checkpoint <- tryCatch(
      arrow::read_parquet(checkpoint_path),
      error = function(e) {
        message("Failed to read checkpoint: ", e$message)
        NULL
      }
    )
    if (!is.null(checkpoint) && nrow(checkpoint) > 0) {
      all_rows <- list(checkpoint)
      fetched <- nrow(checkpoint)
      # anchor forward: only query posts newer than the latest indexedAt
      latest <- max(as.POSIXct(checkpoint$indexedAt, tz = "UTC"), na.rm = TRUE)
      until <- NULL
      since <- format(latest, "%Y-%m-%dT%H:%M:%SZ")
      message("Resuming from checkpoint, last post at: ", since)
      query <- paste0(query, " since:", since)
    }
  }

  while (fetched < hard_limit) {
    iter <- iter + 1
    q <- if (!is.null(until)) paste0(query, " until:", until) else query

    resp <- search_page(q, cursor = cursor, limit = chunk_limit)
    posts <- get_posts_from_resp(resp)

    if (length(posts) == 0) {
      # No posts: attempt to push further back if we have an anchor
      if (!is.null(until)) {
        # Break if repeated emptiness
        message("No more posts for query window: ", q)
        break
      } else {
        message("No posts returned; stopping.")
        break
      }
    }

    df <- tibble(
      uri = map_chr(posts, ~ safe_chr(.x, "uri")),
      indexedAt = map_chr(posts, ~ safe_chr(.x, "indexedAt")),
      author_handle = map_chr(posts, ~ safe_chr(.x, "author", "handle")),
      text = map_chr(posts, ~ safe_chr(.x, "record", "text")),
      like_count = map_int(posts, ~ safe_int(.x, "likeCount")),
      bookmark_count = map_int(posts, ~ safe_int(.x, "bookmarkCount")),
      reply_count = map_int(posts, ~ safe_int(.x, "replyCount")),
      repost_count = map_int(posts, ~ safe_int(.x, "repostCount")),
      reply_parent_uri = map_chr(
        posts,
        ~ safe_chr(.x, "record", "reply", "parent", "uri")
      )
    )

    all_rows <- append(all_rows, list(df))
    fetched <- nrow(bind_rows(all_rows))
    message(sprintf("[Iter %d] Fetched %d total rows", iter, fetched))

    # Persist checkpoint every ~1k rows
    if (fetched %% 1000 < chunk_limit) {
      out <- bind_rows(all_rows) %>% distinct(uri, .keep_all = TRUE)
      tryCatch(
        {
          arrow::write_parquet(out, checkpoint_path)
          message("Checkpointed: ", checkpoint_path)
        },
        error = function(e) {
          message("Checkpoint failed: ", e$message)
        }
      )
    }

    # Update cursor and potential "until" anchor
    cursor <- resp$cursor %||% NULL

    if (is.null(cursor)) {
      # If the server ended this window,
      # push the anchor back using the oldest indexedAt
      # Convert indexedAt to POSIXct for proper comparison
      indexedAt_posix <- suppressWarnings(as.POSIXct(
        df$indexedAt,
        format = "%Y-%m-%dT%H:%M:%OSZ",
        tz = "UTC"
      ))
      if (all(is.na(indexedAt_posix))) {
        message("All indexedAt values are NA after conversion; stopping.")
        break
      }
      oldest <- df$indexedAt[which.min(indexedAt_posix)]
      if (!is.na(oldest) && !identical(oldest, until)) {
        until <- oldest
        # reset cursor to start a new anchored window
        cursor <- NULL
        message("Advancing until anchor to: ", until)
      } else {
        message("No cursor and no new anchor; stopping.")
        break
      }
    }

    # Gentle pacing
    Sys.sleep(runif(1, 0.5, 1.5))
  }

  bind_rows(all_rows) %>% distinct(uri, .keep_all = TRUE)
}

get_reposts_df <- function(uri) {
  Sys.sleep(runif(1, 0.2, 1.1))
  reposts <- tryCatch(
    {
      retry(
        bs_get_reposts(uri, limit = 500, auth = bs_auth, clean = TRUE),
        when = "error",
        max_tries = 4,
        interval = runif(1, 0.8, 1.8)
      )
    },
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("400", msg)) {
        message("Skipping invalid/missing repost: ", uri, " (Bad Request)")
        return(tibble(
          original_uri = uri,
          handle = character(),
          uri = character()
        ))
      } else if (grepl("502", msg)) {
        message("Temporary gateway error for ", uri, "; will retry later.")
        return(tibble(
          original_uri = uri,
          handle = character(),
          uri = character()
        ))
      } else {
        message("Failed to get reposts for ", uri, ": ", msg)
        return(tibble(
          original_uri = uri,
          handle = character(),
          uri = character()
        ))
      }
    }
  )
  if (
    is.null(reposts) || !inherits(reposts, "data.frame") || nrow(reposts) == 0
  ) {
    return(tibble(original_uri = uri, handle = character(), uri = character()))
  }
  reposts$original_uri <- uri
  tibble::as_tibble(reposts)
}

get_thread_df <- function(uri) {
  Sys.sleep(runif(1, 0.2, 0.7))
  thread <- tryCatch(
    {
      retry(
        bs_get_post_thread(uri, auth = bs_auth, clean = FALSE),
        when = "error",
        max_tries = 4,
        interval = runif(1, 0.8, 1.8)
      )
    },
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("400", msg)) {
        message("Skipping invalid/missing thread: ", uri, " (Bad Request)")
        return(tibble(
          original_uri = uri,
          author = character(),
          text = character(),
          uri = character()
        ))
      } else if (grepl("502", msg)) {
        message("Temporary gateway error for ", uri, "; will retry later.")
        return(tibble(
          original_uri = uri,
          author = character(),
          text = character(),
          uri = character()
        ))
      } else {
        message("Failed to get thread for ", uri, ": ", msg)
        return(tibble(
          original_uri = uri,
          author = character(),
          text = character(),
          uri = character()
        ))
      }
    }
  )
  if (is.null(thread) || length(thread) == 0) {
    return(tibble(
      original_uri = uri,
      author = character(),
      text = character(),
      uri = character()
    ))
  }
  tibble(
    original_uri = uri,
    author = map_chr(thread, ~ safe_chr(.x, "post", "author", "handle")),
    text = map_chr(thread, ~ safe_chr(.x, "post", "record", "text")),
    uri = map_chr(thread, ~ safe_chr(.x, "post", "uri"))
  )
}


# Chunking helper
chunk_vec <- function(x, size) split(x, ceiling(seq_along(x) / size))

hydrate_in_batches <- function(posts_df, batch_size = 400, tag = "speirgorm") {
  uris_reposts <- posts_df %>%
    filter(repost_count > 0) %>%
    pull(uri) %>%
    unique()
  uris_threads <- posts_df %>%
    filter(reply_count > 0) %>%
    pull(uri) %>%
    unique()

  rep_chunks <- chunk_vec(uris_reposts, batch_size)
  thr_chunks <- chunk_vec(uris_threads, batch_size)

  all_reposts <- list()
  all_threads <- list()

  # Load existing partial results if resuming
  reposts_checkpoint_path <- sprintf("data/%s_hydrated_reposts.parquet", tag)
  threads_checkpoint_path <- sprintf("data/%s_hydrated_threads.parquet", tag)

  if (file.exists(reposts_checkpoint_path)) {
    all_reposts <- list(arrow::read_parquet(reposts_checkpoint_path))
    message("Loaded existing reposts checkpoint: ", reposts_checkpoint_path)
  }
  if (file.exists(threads_checkpoint_path)) {
    all_threads <- list(arrow::read_parquet(threads_checkpoint_path))
    message("Loaded existing threads checkpoint: ", threads_checkpoint_path)
  }

  for (i in seq_along(rep_chunks)) {
    message(sprintf(
      "Hydrating reposts batch %d/%d (%d uris)",
      i,
      length(rep_chunks),
      length(rep_chunks[[i]])
    ))
    part <- future_map_dfr(
      rep_chunks[[i]],
      get_reposts_df,
      .progress = TRUE,
      .options = furrr_options(seed = 22230)
    )
    all_reposts <- append(all_reposts, list(part))

    # Write batch CSV
    tryCatch(
      {
        readr::write_csv(
          part,
          sprintf("data/reposts_batch_%s_%03d.csv", tag, i)
        )
      },
      error = function(e) message("Failed to write reposts batch: ", e$message)
    )

    # Checkpoint combined reposts (for resume capability)
    combined <- bind_rows(all_reposts) %>%
      distinct(original_uri, handle, uri, .keep_all = TRUE)
    tryCatch(
      {
        arrow::write_parquet(combined, reposts_checkpoint_path)
      },
      error = function(e) message("Failed to checkpoint reposts: ", e$message)
    )

    Sys.sleep(runif(1, 3, 6))
  }

  for (i in seq_along(thr_chunks)) {
    message(sprintf(
      "Hydrating threads batch %d/%d (%d uris)",
      i,
      length(thr_chunks),
      length(thr_chunks[[i]])
    ))
    part <- future_map_dfr(
      thr_chunks[[i]],
      get_thread_df,
      .progress = TRUE,
      .options = furrr_options(seed = 22230)
    )
    all_threads <- append(all_threads, list(part))

    # Write batch CSV
    tryCatch(
      {
        readr::write_csv(
          part,
          sprintf("data/threads_batch_%s_%03d.csv", tag, i)
        )
      },
      error = function(e) message("Failed to write threads batch: ", e$message)
    )

    # Checkpoint combined threads (for resume capability)
    combined <- bind_rows(all_threads) %>%
      distinct(original_uri, author, uri, .keep_all = TRUE)
    tryCatch(
      {
        arrow::write_parquet(combined, threads_checkpoint_path)
      },
      error = function(e) message("Failed to checkpoint threads: ", e$message)
    )

    Sys.sleep(runif(1, 3, 6))
  }

  reposts_df <- bind_rows(all_reposts) %>%
    distinct(original_uri, handle, uri, .keep_all = TRUE)
  threads_df <- bind_rows(all_threads) %>%
    distinct(original_uri, author, uri, .keep_all = TRUE)

  list(reposts_df = reposts_df, threads_df = threads_df)
}

# Step 0 - setup functions
save_graph_svg <- function(
  plot_or_expr,
  filename = "plot.svg",
  folder = "images",
  width = 16,
  height = 12
) {
  # Ensure folder exists
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }

  filepath <- file.path(folder, filename)

  # Case 1: ggplot/ggraph object
  if (inherits(plot_or_expr, "ggplot")) {
    ggplot2::ggsave(
      filename = filepath,
      plot = plot_or_expr,
      device = "svg",
      width = width,
      height = height,
      units = "in"
    )
    message("Saved ggplot/ggraph SVG to: ", filepath)
    return(invisible(filepath))
  }

  # Case 2: Base R / igraph / sna / network plotting expression
  svg(filepath, width = width, height = height)

  # Evaluate the expression
  if (is.expression(plot_or_expr) || is.call(plot_or_expr)) {
    eval(plot_or_expr)
  } else if (is.function(plot_or_expr)) {
    plot_or_expr()
  } else {
    stop("Input must be a ggplot object or a plotting expression/function.")
  }

  dev.off()
  message("Saved base/igraph/sna/network SVG to: ", filepath)
  invisible(filepath)
}

normalize_handle <- function(h) {
  if (length(h) > 1) {
    # Vectorized handling: replace NA values with NA_character_ safely
    h[is.na(h)] <- NA_character_
    return(h)
  }
  if (is.na(h)) {
    return(NA_character_)
  }
  h
}

# helper: parse rgba(), rgb(), hex and named colors into r,g,b,a
parse_color_to_rgba <- function(cols, fallback = c(170, 170, 170, 1)) {
  res <- lapply(cols, function(cl) {
    if (is.na(cl) || cl == "") {
      return(fallback)
    }
    # rgba(r,g,b,a)
    m <- regmatches(
      cl,
      regexec(
        "^\\s*rgba\\s*\\(\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*,\\s*(0|1|0?\\.\\d+)\\s*\\)\\s*$",
        cl,
        perl = TRUE
      )
    )[[1]]
    if (length(m) > 0) {
      return(c(
        as.integer(m[2]),
        as.integer(m[3]),
        as.integer(m[4]),
        as.numeric(m[5])
      ))
    }
    # rgb(r,g,b)
    m2 <- regmatches(
      cl,
      regexec(
        "^\\s*rgb\\s*\\(\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*\\)\\s*$",
        cl,
        perl = TRUE
      )
    )[[1]]
    if (length(m2) > 0) {
      return(c(as.integer(m2[2]), as.integer(m2[3]), as.integer(m2[4]), 1))
    }
    # try col2rgb for hex/named
    rgb_mat <- tryCatch(col2rgb(cl), error = function(e) NULL)
    if (!is.null(rgb_mat)) {
      return(c(
        as.integer(rgb_mat[1, 1]),
        as.integer(rgb_mat[2, 1]),
        as.integer(rgb_mat[3, 1]),
        1
      ))
    }
    fallback
  })
  mat <- do.call(rbind, res)
  colnames(mat) <- c("r", "g", "b", "a")
  as.data.frame(mat)
}

# parallel single-item parser (returns named numeric vector r,g,b,a)
parse_color_single <- function(
  cl,
  fallback = c(r = 170, g = 170, b = 170, a = 1)
) {
  if (is.na(cl) || cl == "") {
    return(fallback)
  }
  m <- regmatches(
    cl,
    regexec(
      "^\\s*rgba\\s*\\(\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*,\\s*(0|1|0?\\.\\d+)\\s*\\)\\s*$",
      cl,
      perl = TRUE
    )
  )[[1]]
  if (length(m) > 0) {
    return(c(
      r = as.integer(m[2]),
      g = as.integer(m[3]),
      b = as.integer(m[4]),
      a = as.numeric(m[5])
    ))
  }
  m2 <- regmatches(
    cl,
    regexec(
      "^\\s*rgb\\s*\\(\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*\\)\\s*$",
      cl,
      perl = TRUE
    )
  )[[1]]
  if (length(m2) > 0) {
    return(c(
      r = as.integer(m2[2]),
      g = as.integer(m2[3]),
      b = as.integer(m2[4]),
      a = 1
    ))
  }
  rgb_mat <- tryCatch(col2rgb(cl), error = function(e) NULL)
  if (!is.null(rgb_mat)) {
    return(c(
      r = as.integer(rgb_mat[1, 1]),
      g = as.integer(rgb_mat[2, 1]),
      b = as.integer(rgb_mat[3, 1]),
      a = 1
    ))
  }
  fallback
}

sanitize_xml <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&apos;", x, fixed = TRUE)
  x
}

# safe rescale helper
safe_rescale <- function(x) {
  if (all(is.na(x))) {
    return(rep(0, length(x)))
  }
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) {
    return(rep(0, length(x)))
  }
  scales::rescale(x, to = c(0, 1), from = rng)
}

layout_exec <- function(
  edges_data,
  nodes_data,
  graph = NULL,
  layout_type,
  ...
) {
  library(igraph)

  # make absolutely sure this is a single scalar
  layout_type <- as.character(layout_type)[1]
  # If a graph is supplied, use it directly
  if (inherits(graph, "igraph")) {
    g <- graph
    nodes_df <- as.data.frame(vertex_attr(g))
  } else {
    edges_df <- rxImport(edges_data)
    nodes_df <- rxImport(nodes_data)

    g <- graph_from_data_frame(
      d = edges_df,
      vertices = nodes_df,
      directed = FALSE
    )
  }
  net_size = vcount(g)

  coords <- switch(
    layout_type,
    "drl" = layout_with_drl(
      g,
      use.seed = TRUE,
      seed = matrix(runif(vcount(g) * 2), ncol = 2)
    ),
    "drl_fast" = layout_with_drl(g, options = list(simmer.attraction = 0)),
    "fr" = layout_with_fr(
      g,
      dim = 3,
      niter = net_size * 3,
      start.temp = sqrt(net_size),
      weights = E(g)$weight
    ),
    "graphopt" = layout_with_graphopt(
      g,
      start = matrix(runif(vcount(g) * 2), ncol = 2),
      niter = 400
    ),
    "lgl" = layout_with_lgl(
      g,
      maxiter = net_size * 3,
      maxdelta = vcount(g) * 2
    ),
    "kk" = layout_with_kk(
      g,
      coords = matrix(runif(vcount(g) * 2), ncol = 2),
      maxiter = net_size * 3,
      weights = E(g)$weight
    ),
    "mds" = layout_with_mds(g, dim = 4),
    "nicely" = layout_nicely(g, dim = 3),
    "tree" = layout_as_tree(
      g,
      root = which(nodes_df$total_degree == max(nodes_df$total_degree)),
      rootlevel = numeric(),
      mode = "out"
    ),
    stop(paste("Unknown layout type:", layout_type))
  )

  df <- as.data.frame(coords)
  names(df) <- c("x", "y")
  df$name <- V(g)$name
  base::attr(df, "layout_type") <- layout_type
  df
}
