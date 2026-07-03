rxSetComputeContext(sql_cc)

layout_exec <- function(edges_data, nodes_data, layout_type, ...) {
  library(igraph)

  # make absolutely sure this is a single scalar
  layout_type <- as.character(layout_type)[1]

  edges_df <- rxImport(edges_data)
  nodes_df <- rxImport(nodes_data)

  g <- graph_from_data_frame(
    d = edges_df,
    vertices = nodes_df,
    directed = FALSE
  )

  coords <- switch(
    layout_type,
    "drl" = layout_with_drl(g, ...),
    "drl_fast" = layout_with_drl(g, options = list(simmer.attraction = 0)),
    "graphopt" = layout_with_graphopt(g, niter = 400),
    "lgl" = layout_with_lgl(g),
    "kk" = layout_with_kk(g),
    "tree" = layout_as_tree(
      g,
      root = which(nodes$total_degree == max(nodes$total_degree))
    ),
    stop(paste("Unknown layout type:", layout_type))
  )

  df <- as.data.frame(coords)
  names(df) <- c("x", "y")
  df$name <- V(g)$name
  df
}
tic()
res <- rxExec(
  layout_exec,
  edges_data = edges_rx,
  nodes_data = nodes_rx,
  layout_type = rxElemArg(c(
    "drl",
    "drl_fast",
    "graphopt",
    "lgl",
    "kk",
    "tree"
  )),
  execObjects = c("connStr", "layout_exec")
)
toc()
# res is a list of 3 data.frames in the same order as layout_type
coords_drl <- res[[1]]
coords_drl_fast <- res[[2]]
coords_graphopt <- res[[3]]
