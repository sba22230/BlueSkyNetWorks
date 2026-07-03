#' Compute Graph Layout Coordinates
#'
#' This function calculates node coordinates for graph visualization using various igraph layout algorithms.
#' It supports both pre-built igraph objects and data frames for edges/nodes.
#'
#' @param edges_data Data source for edges (e.g., RxSqlServerData or data frame).
#' @param nodes_data Data source for nodes (e.g., RxSqlServerData or data frame).
#' @param graph Optional pre-built igraph object. If provided, edges_data and nodes_data are ignored.
#' @param layout_type Character string specifying the layout algorithm (e.g., "fr", "kk").
#' @param directed Logical; whether the graph is directed (default: FALSE).
#' @param seed Optional integer seed for reproducible random layouts.
#' @param ... Additional arguments passed to layout functions (e.g., niter, weights).
#' @return A data frame with columns 'x', 'y' (and optionally 'z'), 'name', and an attribute 'layout_type'.
layout_exec_1 <- function(
  edges_data,
  nodes_data,
  graph = NULL,
  layout_type,
  directed = FALSE,
  seed = NULL,
  ...
) {
  library(igraph)
  # Input validation
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

  # Set seed for reproducibility if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Handle graph input
  if (inherits(graph, "igraph")) {
    graph_obj <- graph
    nodes_df <- as.data.frame(vertex_attr(graph_obj))
  } else {
    # Import data (assumes RevoScaleR context)
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

  # Ensure total_degree for tree layout
  if (layout_type == "tree" && !"total_degree" %in% names(nodes_df)) {
    nodes_df$total_degree <- degree(graph_obj, mode = "total")
  }

  # Extract weights if available, else NULL
  edge_weights <- if ("weight" %in% edge_attr_names(graph_obj)) {
    E(graph_obj)$weight
  } else {
    NULL
  }

  # Compute layout with error handling
  coords <- tryCatch(
    {
      switch(
        layout_type,
        "drl" = layout_with_drl(
          graph_obj,
          use.seed = TRUE,
          seed = matrix(runif(net_size * 2), ncol = 2)
        ),
        "drl_fast" = layout_with_drl(
          graph_obj,
          options = list(simmer.attraction = 0)
        ),
        "fr" = layout_with_fr(
          graph_obj,
          dim = 2, # Force 2D for consistency
          niter = list(...)$niter %||% (net_size / 3),
          start.temp = sqrt(net_size),
          weights = edge_weights
        ),
        "graphopt" = layout_with_graphopt(
          graph_obj,
          start = matrix(runif(net_size * 2), ncol = 2),
          charge = 0.05,
          mass = 30,
          niter = 222,
          tol = 1e-3
        ),
        "lgl" = layout_with_lgl(
          graph_obj,
          maxiter = min(100, list(...)$maxiter %||% 100),
          maxdelta = list(...)$maxdelta %||% net_size,
          area = net_size^2, # Explicit default
          coolexp = 1.2, # Slightly faster cooling
          root = if (!is.null(list(...)$root)) list(...)$root else NULL # Optional root
        ),
        "kk" = layout_with_kk(
          graph_obj,
          coords = matrix(runif(net_size * 2), ncol = 2),
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

  # Prepare output data frame
  result_df <- as.data.frame(coords)
  colnames(result_df) <- if (ncol(result_df) >= 2) c("x", "y") else c("x") # Handle potential 1D
  result_df$name <- V(graph_obj)$name
  attr(result_df, "layout_type") <- layout_type
  attr(result_df, "net_size") <- net_size # Optional metadata

  result_df
}

g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

library(tictoc)
tic()
res <- rxExec(
  layout_exec_1,
  graph = g,
  directed = TRUE,
  seed = 22230,
  layout_type = rxElemArg(c(
    "graphopt",
    "drl",
    "drl_fast",
    "fr",
    #,"graphopt"
    #,"lgl"
    "kk"
  )),
  execObjects = c("connStr", "layout_exec")
)
toc()
