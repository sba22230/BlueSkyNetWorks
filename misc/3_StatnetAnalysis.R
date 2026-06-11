# source("0_functions.R")

# Step 1: Load data
edges_df <- read_parquet("Report/graphs/speirgorm_edges.parquet")
nodes_df <- read_parquet("Report/graphs/speirgorm_nodes.parquet")
# edges <- read_parquet("Report/graphs/speirgorm_edges.parquet")
# nodes <- read_parquet("Report/graphs/speirgorm_nodes.parquet")
# --- Optional: sample reposted posts and filter reposts_df -----------------
# Filter posts that have been reposted, sample them reproducibly, then
# keep only repost records for those sampled original posts. Adjust
# `sample_n_posts` as desired (or set to NULL to keep all reposts).

set.seed(22230)

num_posts <- nrow(edges_df)
num_posts <- 5000
# if tthe num_posts is set, then we sample that many posts
# else we keep all reposts
if (!is.null(num_posts) && nrow(edges_df) > num_posts) {
  # 1. Sample edges from edges_df
  sampled_edges <- edges_df %>% dplyr::sample_n(num_posts)
  # 2. Identify nodes appearing in sampled edges
  nodes_set <- union(sampled_edges$from, sampled_edges$to)
  # 3. Filter nodes_df to keep only relevant nodes
  sampled_nodes <- nodes_df %>% dplyr::filter(name %in% nodes_set)
  # 4. Assign to your expected output variables
  edges <- sampled_edges
  nodes <- sampled_nodes
  cat("Sampled", num_posts, "edges; resulting nodes:", nrow(nodes), "\n")
} else {
  n_iter <- num_posts / 2
  edges <- read_parquet("Report/graphs/speirgorm_edges.parquet")
  nodes <- read_parquet("Report/graphs/speirgorm_nodes.parquet")
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

degree <- degree(bluSkynet)

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
    hist(
      ideg,
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
      odeg,
      xlab = "Outdegree",
      main = "Outdgree Distribution",
      prob = TRUE
    )
  },
  filename = "outdegree_dist.svg"
)

dyadcensus <- sna::dyad.census(bluSkynet)
#dyadcount <- network.dyadcount(bluSkynet)
dyadcount <- sum(
  dyadcensus[[1]],
  dyadcensus[[2]],
  dyadcensus[[3]]
)
edgecount <- network.edgecount(bluSkynet)
netsize <- network.size(bluSkynet)
cen <- centralization(bluSkynet, degree, cmode = "indegree")
ceneig <- centralization(bluSkynet, evcent)
gden <- gden(bluSkynet)
grecip <- grecip(bluSkynet, measure = "dyadic")
grecip_edgewise <- grecip(bluSkynet, measure = "edgewise")
dist_matrix <- geodist(bluSkynet)$gdist
diameter <- max(dist_matrix[is.finite(dist_matrix)])


summary_df <- tibble(
  method = "network/sna",
  network_size = netsize,
  edge_count = edgecount,
  dyad_count = dyadcount,
  density = gden,
  mutual_pairs = dyadcensus[[1]],
  asymetric_pairs = dyadcensus[[2]],
  isolated_nodes = dyadcensus[[3]],
  diameter = diameter,
  avg_path_length = mean(dist_matrix[is.finite(dist_matrix)]),
  neighbours_average = mean(neighbors(g2, V(g2), mode = "all")),
  reciprocity_default = grecip_edgewise,
  reciprocity_ratio = grecip,
  average_in_degree = mean(ideg),
  average_out_degree = mean(odeg),
  most_replied_to = network.vertex.names(bluSkynet)[which.max(ideg)],
  most_active_replier = network.vertex.names(bluSkynet)[which.max(odeg)]
)

str(summary_df)

comparison_table <- bind_rows(summary_df, ig_summary_df) %>%
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
#bet <- betweenness(bluSkynet, gmode = "graph") #  how often shortest paths pass through something
#bet

#sN_clo <- closeness(bluSkynet, cmode = "suminvundir")
#clo
