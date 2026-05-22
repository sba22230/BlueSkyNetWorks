v <- R.Version()

#### Lbrary handling code ####
list.of.packages <- c(
  'aricode',
  'arrow',
  'DT',
  'DBI',
  'dplyr',
  'future',
  'ggraph',
  'ggrepel',
  'ggplot2',
  'igraph',
  'intergraph',
  'lubridate',
  'odbc',
  'purrr',
  'readr',
  'retry',
  'RevoScaleR',
  'sna',
  'stringi',
  'stringr',
  'tibble',
  'tidyr',
  'topicmodels',
  'visNetwork'
)
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[, 'Package'])
]
if (length(new.packages)) {
  install.packages(new.packages, dependencies = TRUE)
}
if (v$major == '4') {
  library(bskyr)
  library(furrr)
  library(patchwork)
  library(rgexf)
  library(statnet)
  library(tidytext)
  wrkrs <- max(1, floor(availableCores(constraints = "connections-16") * 0.7))
}
library(aricode)
library(arrow)
library(DT)
library(DBI)
library(dplyr)
library(future)
library(ggraph)
library(ggrepel)
library(ggplot2)
library(igraph)
library(intergraph)
library(lubridate)
library(odbc)
library(purrr)
library(readr)
library(retry)
library(RevoScaleR) # RevoScaleR provides RxSqlServerData, rxDataStep, etc.
library(scales)
library(sna)
library(stringi)
library(stringr)
library(tibble)
library(tidyr)
library(topicmodels)
library(visNetwork)

#### Ble Sky Functions ####
orig_plan <- future::plan()
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
  #  Load existing checkpoint if available
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
plan(multisession, workers = wrkrs)
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

#### SQL Functions ####
sql_server_available <- function() {
  tryCatch(
    {
      con <- dbConnect(
        odbc(),
        Driver = "SQL Server",
        Server = "localhost",
        Database = "master",
        Trusted_Connection = "Yes",
        Port = 1433
      )
      dbDisconnect(con)
      TRUE
    },
    error = function(e) FALSE
  )
}

if (v$os != "linux-gnu" && sql_server_available()) {
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

#### Graph saving functions ####
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
  directed = FALSE,
  seed = NULL,
  community_separation = 5, # <--- NEW: controls spacing between communities
  ...
) {
  library(igraph)

  #-------------------------------
  # 1. Input validation
  #-------------------------------
  if (!is.character(layout_type) || length(layout_type) != 1) {
    stop("layout_type must be a single character string.")
  }

  valid_layouts <- c(
    "drl",
    "drl_fast",
    "fr",
    "graphopt",
    "lgl",
    "kk",
    "mds",
    "nicely",
    "tree"
  )

  if (!layout_type %in% valid_layouts) {
    stop(
      "Unknown layout_type: ",
      layout_type,
      ". Valid options: ",
      paste(valid_layouts, collapse = ", ")
    )
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  #-------------------------------
  # 2. Build graph
  #-------------------------------
  if (inherits(graph, "igraph")) {
    graph_obj <- graph
    nodes_df <- as.data.frame(vertex_attr(graph_obj))
  } else {
    edges_df <- tryCatch(rxImport(edges_data), error = function(e) {
      stop("Failed to import edges_data: ", e$message)
    })
    nodes_df <- tryCatch(rxImport(nodes_data), error = function(e) {
      stop("Failed to import nodes_data: ", e$message)
    })

    if (nrow(edges_df) == 0 || nrow(nodes_df) == 0) {
      stop("Edges or nodes data is empty.")
    }

    graph_obj <- igraph::graph_from_data_frame(
      d = edges_df,
      vertices = nodes_df,
      directed = directed
    )
  }

  net_size <- vcount(graph_obj)
  if (net_size == 0) {
    stop("Graph has no vertices.")
  }

  #-------------------------------
  # 3. Ensure total_degree for tree layout
  #-------------------------------
  if (layout_type == "tree" && !"total_degree" %in% names(nodes_df)) {
    nodes_df$total_degree <- degree(graph_obj, mode = "total")
  }

  #-------------------------------
  # 4. Extract weights
  #-------------------------------
  edge_weights <- if ("weight" %in% edge_attr_names(graph_obj)) {
    E(graph_obj)$weight
  } else {
    NULL
  }

  #-------------------------------
  # 5. Precompute PCA-based coordinates
  #-------------------------------
  precompute_layout <- function(nodes_df, community_separation) {
    numeric_cols <- c(
      "posts_authored",
      "total_likes_on_posts",
      "betweenness_norm",
      "closeness_norm",
      "eigenvector_centrality",
      "authority_norm",
      "local_clustering",
      "total_degree",
      "community"
    )

    df <- nodes_df[, intersect(numeric_cols, names(nodes_df)), drop = FALSE]
    df[is.na(df)] <- 0

    # PCA → 2D
    pca <- prcomp(df, scale. = TRUE)
    coords <- pca$x[, 1:2, drop = FALSE]
    colnames(coords) <- c("x_init", "y_init")

    # Optional: separate communities in space
    if ("community" %in% names(nodes_df)) {
      comm <- as.numeric(as.factor(nodes_df$community))
      coords[, 1] <- coords[, 1] + comm * community_separation
    }

    coords
  }

  pre_coords <- precompute_layout(nodes_df, community_separation)

  # NEW: Helper for random community-clustered coordinates
  precompute_random_community_coords <- function(nodes_df, net_size) {
    if (!"community" %in% names(nodes_df)) {
      # Fallback to fully random if no community info
      warning("No 'community' column found; using fully random coordinates.")
      return(matrix(
        runif(net_size * 2),
        ncol = 2,
        dimnames = list(NULL, c("x_init", "y_init"))
      ))
    }

    comms <- as.factor(nodes_df$community)
    unique_comms <- levels(comms)
    n_comms <- length(unique_comms)

    # Generate random centers for each community (spread them out)
    comm_centers <- matrix(runif(n_comms * 2, -10, 10), ncol = 2)
    # Centers in a rough square

    # For each node, add small random noise around its community's center
    coords <- matrix(NA, nrow = nrow(nodes_df), ncol = 2)
    for (i in seq_along(unique_comms)) {
      comm <- unique_comms[i]
      idx <- which(comms == comm)
      noise <- matrix(rnorm(length(idx) * 2, mean = 0, sd = 1), ncol = 2)
      # Gaussian noise
      coords[idx, ] <- matrix(
        rep(comm_centers[i, ], each = length(idx)),
        ncol = 2
      ) +
        noise
    }

    colnames(coords) <- c("x_init", "y_init")
    coords
  }

  rand_coords <- precompute_random_community_coords(
    nodes_df,
    net_size
  )
  #-------------------------------
  # 6. Compute layout
  #-------------------------------
  coords <- tryCatch(
    {
      switch(
        layout_type,

        "drl" = layout_with_drl(
          graph_obj,
          use.seed = TRUE,
          seed = pre_coords
        ),

        "drl_fast" = layout_with_drl(
          graph_obj,
          options = list(simmer.attraction = 0)
        ),

        "fr" = layout_with_fr(
          graph_obj,
          dim = 2,
          niter = list(...)$niter %||% (net_size / 3),
          start.temp = sqrt(net_size),
          weights = edge_weights,
          coords = rand_coords
        ),

        "graphopt" = layout_with_graphopt(
          graph_obj,
          start = pre_coords,
          charge = 0.05,
          mass = 30,
          niter = 222,
          tol = 1e-3
        ),

        "lgl" = layout_with_lgl(
          graph_obj,
          maxiter = min(100, list(...)$maxiter %||% 100),
          maxdelta = list(...)$maxdelta %||% net_size,
          area = net_size^2,
          coolexp = 1.2,
          root = which(nodes_df$total_degree == max(nodes_df$total_degree))
        ),

        "kk" = layout_with_kk(
          graph_obj,
          coords = rand_coords,
          maxiter = list(...)$maxiter %||% (net_size / 3),
          weights = edge_weights
        ),

        "mds" = layout_with_mds(graph_obj, dim = 2),

        "nicely" = layout_nicely(graph_obj, dim = 2),

        "tree" = layout_as_tree(
          graph_obj,
          root = which(nodes_df$total_degree == max(nodes_df$total_degree)),
          rootlevel = numeric(),
          mode = "out"
        ),

        stop("Unexpected layout_type (should not reach here)")
      )
    },
    error = function(e) {
      warning(
        "Layout computation failed for ",
        layout_type,
        ": ",
        e$message,
        ". Falling back to 'nicely'."
      )
      layout_nicely(graph_obj, dim = 2)
    }
  )

  #-------------------------------
  # 7. Prepare output
  #-------------------------------
  result_df <- as.data.frame(coords)
  colnames(result_df) <- if (ncol(result_df) >= 2) c("x", "y") else "x"
  result_df$name <- V(graph_obj)$name

  attr(result_df, "layout_type") <- layout_type
  attr(result_df, "net_size") <- net_size

  result_df
}

#### NLP helpers ####
plot_word_comparison_date <- function(graph, community_id) {
  # ---- 1. Extract posts from edges ----
  posts <- cbind(E(graph)$text, E(graph)$created_at) %>%
    as.data.frame() %>%
    mutate(V2 = ymd(V2))
  colnames(posts) <- c("text", "created_at")

  # ---- 2. Tidy text ----
  tidy_posts <- posts %>%
    filter(!str_detect(text, "^RT")) %>%
    mutate(text = str_replace_all(text, replace_reg, "")) %>%
    unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
    filter(
      !word %in% stop_words$word,
      !word %in% str_remove_all(stop_words$word, "'"),
      str_detect(word, "[a-z]")
    )
  word_time <- tidy_posts %>%
    group_by(word) %>%
    summarise(median_time = median(created_at))

  # ---- 3. Compute counts + Dirichlet log-odds ----
  counts_long <- tidy_posts %>% count(word, name = "n")
  N <- sum(counts_long$n)
  alpha <- 0.01

  counts <- counts_long %>%
    mutate(
      log_odds = log((n + alpha) / (N - n + alpha))
    )

  # ---- 4. Compute frequencies ----
  frequency <- tidy_posts %>%
    count(word, name = "n") %>%
    mutate(freq = n / sum(n))

  frequency <- frequency %>%
    left_join(global_freq, by = "word")

  # ---- 5. Join log-odds onto frequency ----
  frequency <- frequency %>%
    left_join(counts %>% select(word, log_odds), by = "word")

  frequency <- frequency %>%
    left_join(word_time, by = "word")

  # ---- 6. Identify distinctive + common words ----
  top_words <- frequency %>% slice_max(log_odds, n = 15)

  common_words <- frequency %>%
    mutate(diff = abs(freq - global_freq)) %>%
    slice_min(diff, n = 5)

  # ---- 7. Community metadata ----
  n_nodes <- vcount(graph)
  n_edges <- ecount(graph)
  avg_degree <- mean(degree(graph))
  density <- edge_density(graph)

  title_text <- paste0(
    "Community ",
    community_id,
    " — ",
    n_nodes,
    " nodes, ",
    n_edges,
    " edges\n",
    "Avg degree: ",
    round(avg_degree, 2),
    " | Density: ",
    round(density, 4)
  )

  # ---- 8. Build the plot ----
  p <- ggplot(frequency, aes(x = median_time, y = log_odds)) +
    geom_point(alpha = 0.4, size = 2, color = "#1f77b4") +
    geom_density_2d(color = "grey85", alpha = 0.3) +
    geom_text_repel(
      data = top_words,
      aes(label = word),
      size = 4,
      fontface = "bold",
      color = "#1f78b4"
    ) +
    geom_text_repel(
      data = common_words,
      aes(label = word),
      size = 3,
      fontface = "bold",
      color = "#33a02c"
    ) +
    scale_x_date(date_labels = "%Y-%m-%d") +
    #scale_x_log10(labels = percent_format()) +
    #scale_y_log10(labels = percent_format()) +
    labs(
      x = "Median posting time",
      y = "Distinctiveness (Dirichlet log-odds)",
      title = title_text,
      subtitle = "Distinctive (blue bold) and common (green bold) words"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10)
    )

  return(p)
}

ViewPostsByDate <- function(filter, comm_1, comm_2) {
  posts <- posts %>%
    filter(community == comm_1 | community == comm_2)

  ggplot(posts, aes(x = created_at, fill = community)) +
    geom_bar(position = "identity", show.legend = FALSE) +
    facet_wrap(~community, ncol = 1)
}

ViewCommunityContrastedByWords <- function(
  totals,
  counts,
  tidy_posts,
  comm_1,
  comm_2
) {
  N1 <- totals$N[totals$community == comm_1]
  N2 <- totals$N[totals$community == comm_2]
  alpha <- 0.01

  # 1. counts for the two communities
  counts_sub <- counts %>%
    select(
      word,
      count_1 = all_of(as.character(comm_1)),
      count_2 = all_of(as.character(comm_2))
    ) %>%
    mutate(
      count_1 = replace_na(count_1, 0),
      count_2 = replace_na(count_2, 0)
    )

  # 2. log-odds
  counts_sub <- counts_sub %>%
    mutate(
      log_odds_1 = log((count_1 + alpha) / (N1 - count_1 + alpha)),
      log_odds_2 = log((count_2 + alpha) / (N2 - count_2 + alpha)),
      log_odds = log_odds_1 - log_odds_2
    )

  # 3. frequencies
  freq <- tidy_posts %>%
    filter(community %in% c(comm_1, comm_2)) %>%
    count(community, word, name = "n") %>%
    left_join(
      tidy_posts %>%
        filter(community %in% c(comm_1, comm_2)) %>%
        count(community, name = "total"),
      by = "community"
    ) %>%
    mutate(freq = n / total) %>%
    select(community, word, freq) %>%
    pivot_wider(
      names_from = community,
      values_from = freq,
      values_fill = 0
    )

  # derive the actual column names, e.g. "26", "22"
  col_1 <- as.character(comm_1)
  col_2 <- as.character(comm_2)

  df <- freq %>%
    rename(
      freq_1 = all_of(col_1),
      freq_2 = all_of(col_2)
    ) %>%
    left_join(counts_sub %>% select(word, log_odds), by = "word") %>%
    mutate(
      freq_1 = ifelse(freq_1 == 0, 1e-6, freq_1),
      freq_2 = ifelse(freq_2 == 0, 1e-6, freq_2),
      dominant = case_when(
        freq_1 > freq_2 ~ "1",
        freq_2 > freq_1 ~ "2",
        TRUE ~ "equal"
      )
    )

  top_1 <- df %>% slice_max(log_odds, n = 10)
  top_2 <- df %>% slice_min(log_odds, n = 10)
  top20 <- bind_rows(top_1, top_2)

  common_words <- df %>%
    mutate(log_ratio = abs(log10(freq_1) - log10(freq_2))) %>%
    slice_min(log_ratio, n = 10)
  plot_df <- df %>%
    semi_join(top20, by = "word") %>%
    bind_rows(common_words) %>%
    distinct(word, .keep_all = TRUE)

  ggplot(plot_df, aes(x = freq_1, y = freq_2)) +
    geom_point(aes(color = dominant), alpha = 0.4, size = 2) +
    geom_density_2d(color = "grey70", alpha = 0.3) +
    geom_text_repel(
      data = top20,
      aes(label = word),
      size = 4,
      fontface = "bold",
      color = "black",
      max.overlaps = Inf
    ) +
    geom_text_repel(
      data = common_words,
      aes(label = word),
      size = 3,
      color = "grey20",
      fontface = "italic",
      max.overlaps = Inf
    ) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_manual(
      values = c("1" = "#1f77b4", "2" = "#d62728", "equal" = "grey40"),
      name = "More frequent in"
    ) +
    geom_abline(color = "red", linetype = "dashed") +
    labs(
      x = paste("Frequency in", comm_1),
      y = paste("Frequency in", comm_2),
      title = paste(
        "Word Frequency Comparison Between Communities",
        comm_1,
        "and",
        comm_2
      ),
      subtitle = "Log-odds distinctive words (bold) and common words (italic)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

plan(orig_plan)
gc()
