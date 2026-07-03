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

pre_coords <- precompute_layout(nodes_df)
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
  comm_centers <- matrix(runif(n_comms * 2, -10, 10), ncol = 2) # Centers in a rough square

  # For each node, add small random noise around its community's center
  coords <- matrix(NA, nrow = nrow(nodes_df), ncol = 2)
  for (i in seq_along(unique_comms)) {
    comm <- unique_comms[i]
    idx <- which(comms == comm)
    noise <- matrix(rnorm(length(idx) * 2, mean = 0, sd = 1), ncol = 2) # Gaussian noise
    coords[idx, ] <- matrix(
      rep(comm_centers[i, ], each = length(idx)),
      ncol = 2
    ) +
      noise
  }

  colnames(coords) <- c("x_init", "y_init")
  coords
}

coords <- precompute_random_community_coords(
  nodes_df,
  vcount(g)
)


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
