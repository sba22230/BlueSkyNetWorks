precompute_layout <- function(nodes_df, community_separation = 10) {
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

  # Optional: separate communities
  if ("community" %in% names(nodes_df)) {
    comm <- as.numeric(as.factor(nodes_df$community))
    coords[, 1] <- coords[, 1] + comm * community_separation
  }

  coords
}

coords <- precompute_layout(nodes_df)

cols <- as.factor(nodes_df$community)

plot(
  coords[, 1],
  coords[, 2],
  pch = 19,
  col = cols,
  xlab = "PCA X (with community offset)",
  ylab = "PCA Y",
  main = "Precomputed Layout by Community"
)
legend(
  "topright",
  legend = levels(cols),
  col = 1:length(levels(cols)),
  pch = 19
)

prcomp(nodes_df[, numeric_cols], scale. = TRUE)
vsize <- V(g)$pagerank + 1
main_title <- paste0(
  "Main Graph ",
  " ” ",
  vcount(g),
  " nodes\n(",
  "PCA-based layout",
  ")"
)
plot.igraph(
  g,
  layout = coords,
  main = main_title,
  vertex.size = vsize,
  vertex.label.cex = 0.2,
  edge.arrow.size = 0.3,
  vertex.color = cols
)
