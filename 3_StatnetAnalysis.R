source("0_functions.R")

# Step 1: Load data
posts_df <- read_parquet("data/speirgorm_network.parquet")
reposts_df <- read_parquet("data/speirgorm_reposts.parquet")
# edges <- read_parquet("graphs/speirgorm_edges.parquet")
# nodes <- read_parquet("graphs/speirgorm_nodes.parquet")
# --- Optional: sample reposted posts and filter reposts_df -----------------
# Filter posts that have been reposted, sample them reproducibly, then
# keep only repost records for those sampled original posts. Adjust
# `sample_n_posts` as desired (or set to NULL to keep all reposts).

set.seed(22230)

num_posts <- nrow(posts_df)
# if tthe num_posts is set, then we sample that many posts
# else we keep all reposts
if (!is.null(num_posts) && nrow(posts_df) > num_posts) {
  n_iter <- num_posts / 2
  sampled_posts <- posts_df %>%
    dplyr::sample_n(num_posts) # or sample_frac(0.1)
  head(sampled_posts, 5)
  edges <- sampled_posts %>%
    dplyr::filter(!is.na(RepostedBy)) %>%
    dplyr::transmute(
      from = RepostedBy,
      to = PostedBy,
      repost_uri = Post,
      created_at = PostedOn,
      like_count,
      reply_count,
      bookmark_count,
      repost_count,
      text,
      reposts_received = repost_count, # or your own metric
      edgeStarts = PostedOn,
      edgeEnds = PostedOn # or NA if you prefer open-ended edges
    )

  # 3A. Posts authored
  posts_authored <- sampled_posts %>%
    dplyr::group_by(PostedBy) %>%
    dplyr::summarise(
      earliestPost = min(PostedOn, na.rm = TRUE),
      latestPost = max(PostedOn, na.rm = TRUE),
      posts_authored = dplyr::n(),
      total_likes_on_posts = sum(like_count, na.rm = TRUE),
      total_replies_on_posts = sum(reply_count, na.rm = TRUE),
      total_bookmarks_on_posts = sum(bookmark_count, na.rm = TRUE)
    ) %>%
    dplyr::rename(name = PostedBy)

  # 3B. Reposts made
  reposts_made <- sampled_posts %>%
    dplyr::filter(!is.na(RepostedBy)) %>%
    dplyr::count(RepostedBy, name = "reposts_made") %>%
    dplyr::rename(name = RepostedBy)

  # 3C. Reposts received
  reposts_received <- sampled_posts %>%
    dplyr::filter(!is.na(RepostedBy)) %>%
    dplyr::count(PostedBy, name = "reposts_received") %>%
    dplyr::rename(name = PostedBy)

  # 3D. Combine all node attributes
  nodes <- posts_authored %>%
    dplyr::left_join(reposts_made, by = "name") %>%
    dplyr::left_join(reposts_received, by = "name") %>%
    dplyr::mutate(
      reposts_made = tidyr::replace_na(reposts_made, 0),
      reposts_received = tidyr::replace_na(reposts_received, 0)
    )

  nodes <- nodes %>%
    dplyr::mutate(
      start = earliestPost,
      end = latestPost
    )

  nodes_set <- nodes$name

  edges <- edges %>%
    dplyr::filter(from %in% nodes_set, to %in% nodes_set)

  cat(
    "Sampled",
    num_posts,
    "reposted posts; resulting reposts count:",
    nrow(reposts_df),
    "\n"
  )
} else {
  n_iter <- num_posts / 2
  edges <- read_parquet("graphs/speirgorm_edges.parquet")
  nodes <- read_parquet("graphs/speirgorm_nodes.parquet")
  cat("Keeping all reposts; total reposts count:", nrow(edges), "\n")
}


cat("Nodes count:", nrow(nodes), "\n")
cat("Edges count:", nrow(edges), "\n")

g2 <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
coords <- layout_with_drl(g2)
V(g2)$x <- coords[, 1]
V(g2)$y <- coords[, 2]

summary_df <- tibble(
  network_size = vcount(g2),
  edge_count = ecount(g2),
  dyad_count = gsize(g2),
  density = edge_density(g2),
  mutual_pairs = dyad_census(g2)$mut,
  asymetric_pairs = dyad_census(g2)$asym,
  isolated_nodes = dyad_census(g2)$null,
  diameter = diameter(g2, directed = TRUE, weights = NA),
  avg_path_length = mean_distance(g2, directed = TRUE),
  neighbours_average = neighbors(g2, V(g2), mode = "all"),
  reciprocity_default = reciprocity(g2, mode = "default"),
  reciprocity_ratio = reciprocity(g2, mode = "ratio"),
  average_in_degree = mean(igraph::degree(g2, mode = "in")),
  average_out_degree = mean(igraph::degree(g2, mode = "out")),
  most_replied_to = names(which.max(igraph::degree(g2, mode = "in"))),
  most_active_replier = names(which.max(igraph::degree(g2, mode = "out")))
)

print(summary_df)
coords2 <- layout_with_drl(
  g2,
  use.seed = FALSE,
  seed = matrix(runif(vcount(g2) * 2), ncol = 2),
  options = list(init.iterations = n_iter)
) # heavy step, do once
V(g2)$x <- coords2[, 1]
V(g2)$y <- coords2[, 2]
g3 <- ggraph(g2, layout = "manual", x = V(g2)$x, y = V(g2)$y) +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = total_likes_on_posts, color = reposts_received)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_graph()
cat("Final graph summary:\n")
print(summary(g2))

# Start of Statnet analysis
# Convert igraph to statnet
library(network)
bluSkynet <- asNetwork(g2)
summary(bluSkynet)

bluSkynet %v% "vertex.names"
# bluSkynet[,]
list.edge.attributes(bluSkynet)
bluSkynet %e% "edgeStarts"
#as.sociomatrix.sna(bluSkynet, "created_at")

network.dyadcount(bluSkynet)
network.edgecount(bluSkynet)
network.size(bluSkynet)
degree(bluSkynet)

ideg <- degree(bluSkynet, cmode = "indegree")
odeg <- degree(bluSkynet, cmode = "outdegree")

save_graph_svg(
  plot_or_expr = function() {
    plot(ideg, odeg, type = "n", xlab = "Incoming", ylab = "Outgoing")
    abline(0, 1, lty = 3)
    text(
      jitter(ideg),
      jitter(odeg),
      labels = network.vertex.names(bluSkynet),
      cex = 0.5,
      col = 2
    )
  },
  filename = "degree_scatter.svg"
)

save_graph_svg(
  plot_or_expr = function() {
    hist(ideg, xlab = "Indegree", main = "Indgree Distribution", prob = TRUE)
  },
  filename = "indegree_dist.svg"
)

save_graph_svg(
  plot_or_expr = function() {
    hist(odeg, xlab = "Outdegree", main = "Outdgree Distribution", prob = TRUE)
  },
  filename = "outdegree_dist.svg"
)

bet <- betweenness(bluSkynet, gmode = "graph") #  how often shortest paths pass through something
bet

gplot(bluSkynet, vertex.cex = sqrt(bet) / 25, gmode = "graph")

clo <- closeness(bluSkynet, cmode = "suminvundir")
clo

cen <- centralization(bluSkynet, degree, cmode = "indegree")
ceneig <- centralization(bluSkynet, evcent)

gden(bluSkynet)
grecip(bluSkynet)
grecip(bluSkynet, measure = "edgewise")
