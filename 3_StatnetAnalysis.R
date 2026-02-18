# source("0_functions.R")

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
num_posts <- 5000
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

g4_sample <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
ig_cen <- igraph::dyad_census(g4_sample)


ig_summary_df <- tibble(
  method = "igraph",
  network_size = vcount(g4_sample),
  edge_count = ecount(g4_sample),
  dyad_count = sum(ig_cen$mut, ig_cen$asym, ig_cen$null),
  density = edge_density(g4_sample),
  mutual_pairs = ig_cen$mut,
  asymetric_pairs = ig_cen$asym,
  isolated_nodes = ig_cen$null,
  diameter = diameter(g4_sample, directed = TRUE, weights = NA),
  avg_path_length = mean_distance(g4_sample, directed = TRUE),
  neighbours_average = mean(neighbors(g4_sample, V(g4_sample), mode = "all")),
  reciprocity_default = reciprocity(g4_sample, mode = "default"),
  reciprocity_ratio = reciprocity(g4_sample, mode = "ratio"),
  average_in_degree = mean(igraph::degree(g4_sample, mode = "in")),
  average_out_degree = mean(igraph::degree(g4_sample, mode = "out")),
  most_replied_to = names(which.max(igraph::degree(g4_sample, mode = "in"))),
  most_active_replier = names(which.max(igraph::degree(
    g4_sample,
    mode = "out"
  )))
)

str(ig_summary_df)

cat("Final graph summary:\n")
print(summary(g4_sample))
rxWriteObject(
  ds_Graphs,
  paste0("g4_sampled_Graph - igraph - no posts: num_posts ", num_posts),
  g4_sample,
  overwrite = TRUE
)
# Start of Statnet analysis
# Convert igraph to statnet
library(network)
bluSkynet <- asNetwork(g4_sample)
summary(bluSkynet)
rxWriteObject(
  ds_Graphs,
  paste0("bluSkynet_Graph - network - no posts: num_posts ", num_posts),
  bluSkynet,
  overwrite = TRUE
)

bluSkynet %v% "vertex.names"
# bluSkynet[,]
list.edge.attributes(bluSkynet)
bluSkynet %e% "edgeStarts"
#as.sociomatrix.sna(bluSkynet, "created_at")

bsN_degree <- degree(bluSkynet)

bsN_ideg <- degree(bluSkynet, cmode = "indegree")
bsN_odeg <- degree(bluSkynet, cmode = "outdegree")

save_graph_svg(
  plot_or_expr = function() {
    plot(bsN_ideg, bsN_odeg, type = "n", xlab = "Incoming", ylab = "Outgoing")
    abline(0, 1, lty = 3)
    text(
      jitter(bsN_ideg),
      jitter(bsN_odeg),
      labels = network.vertex.names(bluSkynet),
      cex = 0.5,
      col = 2
    )
  },
  filename = "degree_scatter.svg"
)

save_graph_svg(
  plot_or_expr = function() {
    hist(
      bsN_ideg,
      xlab = "Indegree",
      main = "Indgree Distribution",
      prob = TRUE
    )
  },
  filename = "indegree_dist.svg"
)

save_graph_svg(
  plot_or_expr = function() {
    hist(
      bsN_odeg,
      xlab = "Outdegree",
      main = "Outdgree Distribution",
      prob = TRUE
    )
  },
  filename = "outdegree_dist.svg"
)

bsN_dyadcensus <- sna::dyad.census(bluSkynet)
#bsN_dyadcount <- network.dyadcount(bluSkynet)
bsN_dyadcount <- sum(
  bsN_dyadcensus[[1]],
  bsN_dyadcensus[[2]],
  bsN_dyadcensus[[3]]
)
bsN_edgecount <- network.edgecount(bluSkynet)
bsN_netsize <- network.size(bluSkynet)
bsN_cen <- centralization(bluSkynet, degree, cmode = "indegree")
bsN_ceneig <- centralization(bluSkynet, evcent)
bsN_gden <- gden(bluSkynet)
bsN_grecip <- grecip(bluSkynet, measure = "dyadic")
bsN_grecip_edgewise <- grecip(bluSkynet, measure = "edgewise")
dist_matrix <- geodist(bluSkynet)$gdist
bsN_diameter <- max(dist_matrix[is.finite(dist_matrix)])


bsN_summary_df <- tibble(
  method = "network/sna",
  network_size = bsN_netsize,
  edge_count = bsN_edgecount,
  dyad_count = bsN_dyadcount,
  density = bsN_gden,
  mutual_pairs = bsN_dyadcensus[[1]],
  asymetric_pairs = bsN_dyadcensus[[2]],
  isolated_nodes = bsN_dyadcensus[[3]],
  diameter = bsN_diameter,
  avg_path_length = mean(dist_matrix[is.finite(dist_matrix)]),
  neighbours_average = mean(neighbors(g2, V(g2), mode = "all")),
  reciprocity_default = bsN_grecip_edgewise,
  reciprocity_ratio = bsN_grecip,
  average_in_degree = mean(bsN_ideg),
  average_out_degree = mean(bsN_odeg),
  most_replied_to = network.vertex.names(bluSkynet)[which.max(bsN_ideg)],
  most_active_replier = network.vertex.names(bluSkynet)[which.max(bsN_odeg)]
)

str(bsN_summary_df)

comparison_table <- bind_rows(bsN_summary_df, ig_summary_df) %>%
  mutate(across(-method, as.character)) %>% # ensure all values are same type
  pivot_longer(
    cols = -method,
    names_to = "metric",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = method,
    values_from = value
  )

datatable(comparison_table)
# very slow -
#bsN_bet <- betweenness(bluSkynet, gmode = "graph") #  how often shortest paths pass through something
#bsN_bet

#sN_clo <- closeness(bluSkynet, cmode = "suminvundir")
#bsN_clo
