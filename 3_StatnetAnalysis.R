

source("0_functions.R")

# Step 1: Load data
posts_df <- read_parquet("data/speirgorm_posts.parquet")
reposts_df <- read_parquet("data/speirgorm_reposts.parquet")
edges <- read.csv("graphs/speirgorm_edges.csv")
nodes <- read.csv("graphs/speirgorm_nodes.csv")
# --- Optional: sample reposted posts and filter reposts_df -----------------
# Filter posts that have been reposted, sample them reproducibly, then
# keep only repost records for those sampled original posts. Adjust
# `sample_n_posts` as desired (or set to NULL to keep all reposts).
set.seed(22230)
sample_n_posts <- 1000
nodes_org <- nodes
edges_org <- edges
# # change to desired sample size; set NULL to skip sampling# Posts that have one or more reposts according to metadata
# reposted_posts <- posts_df |> filter(repost_count > 0, !is.na(uri), !is.na(repost_count))
#
if (!is.null(sample_n_posts) && nrow(edges) > 0) {
  
  edges_filtered <- edges |>
    dplyr::left_join(
      nodes |> dplyr::select(name, repost_count),
      by = c("from" = "name")
    ) |>
    dplyr::filter(repost_count > 0)
  
  sampled_posts <- edges_filtered |>
    slice_sample(n = min(sample_n_posts, nrow(edges_filtered)))
  
} else {
  sampled_posts <- posts
}

edges <- sampled_posts

#
#
# Filter edges to only include reposts of the sampled original posts
# posts_df_org <- posts_df
# posts_df <- sampled_posts
# reposts_df_org <- reposts_df
# reposts_df <- reposts_df_sampled
#
# # Step 2: Build edge list (who reposted whom)
# edges <- reposts_df |>
#   transmute(from = handle, to = original_uri) |>
#   distinct()
#
# cat("Edges count:", nrow(edges), "\n")
#
# # Step 3: Build node list (unique actors and posts)
nodes <- tibble(name = unique(c(edges$from, edges$to)))
cat("Nodes count:", nrow(nodes), "\n")
#
# # Step 4: Enrich edges with author info
# edges <- reposts_df |>
#   left_join(
#     posts_df |> select(uri, author_handle),
#     by = c("original_uri" = "uri")
#   ) |>
#   transmute(
#     from = handle,
#     to = author_handle,
#     repost_uri = uri,
#     created_at = created_at
#   ) |>
#   filter(!is.na(from) & !is.na(to)) |>
#   distinct()
# cat("Enriched edges count:", nrow(edges), "\n")
#
# # Step 5: Enrich nodes with metadata
metadata <- bind_rows(
  reposts_df |> select(name = handle, display_name, did),
  posts_df   |> select(name = author_handle, repost_count, like_count)
) |>
  distinct(name, .keep_all = TRUE)

nodes <- nodes |> 
  left_join(metadata, by = "name")

#
# # Add repost counts
# nodes <- nodes |>
#   mutate(repost_count = table(edges$to)[name] |> as.integer())
# cat("Enriched nodes count:", nrow(nodes), "\n")

# Step 6: Plot enriched network with ggraph
g2 <- graph_from_data_frame(d = edges,
                            vertices = nodes,
                            directed = TRUE)

summary_df <- tibble(
  total_users = vcount(g2),
  total_replies = ecount(g2),
  density = edge_density(g2),
  most_replied_to = names(which.max(igraph::degree(g2, mode = "in"))),
  most_active_replier = names(which.max(igraph::degree(g2, mode = "out")))
)

print(summary_df)
coords2 <- layout_with_drl(g2) # heavy step, do once
V(g2)$x <- coords2[, 1]
V(g2)$y <- coords2[, 2]
g3 <- ggraph(g2, layout = "manual", x = V(g2)$x, y = V(g2)$y) +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = like_count, color = repost_count)) +
  geom_node_text(aes(label = did), repel = TRUE) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_void()
cat("Final graph summary:\n")
print(summary(g2))
plot(g3)
save_graph_svg(g3, "g3_with_drl.svg")

# Start of Statnet analysis
# Convert igraph to statnet
library(network)
bluSkynet <- asNetwork(g2)
summary(bluSkynet)

bluSkynet %v% "vertex.names"
bluSkynet[, ]
list.edge.attributes(bluSkynet)
bluSkynet %e% "created_at"
#as.sociomatrix.sna(bluSkynet, "created_at")
save_graph_svg(
  plot_or_expr = function() {
sna::gplot(bluSkynet) },
    filename = "1_bluSkynet_sna.svg"
)
save_graph_svg(
  plot_or_expr = function() {
sna::gplot(bluSkynet, displaylabels = T, mode = "target") # very slow
  },
filename = "2_bluSkynet_sna.svg"
)

save_graph_svg(
  plot_or_expr = function() {
plot(bluSkynet, displaylabels = F)
  },
filename = "3_bluSkynet_sna.svg"
)

save_graph_svg(
  plot_or_expr = function() {
gplot(bluSkynet,
      label.cex - 0.2,
      label.col = "blue",
      displaylabels = FALSE) # graph is very messy with labels
  },
filename = "4_bluSkynet_sna.svg"
)

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
  jitter(ideg), jitter(odeg),
  labels = network.vertex.names(bluSkynet),
  cex = 0.5, col = 2
)
  },
filename = "degree_scatter.svg"
)

save_graph_svg(
  plot_or_expr = function() {
hist(ideg, xlab = "Indegree", 
     main = "Indgree Distribution", prob = TRUE)
  },
filename = "indegree_dist.svg"
)
save_graph_svg(
  plot_or_expr = function() {
hist(odeg, xlab = "Outdegree", 
     main = "Outdgree Distribution", prob = TRUE)
  },
filename = "outdegree_dist.svg"
)

save_graph_svg(
  plot_or_expr = function() {
gplot(bluSkynet, vertex.cex = (ideg+odeg)^0.5,
      vertex.sides = 50, label.cex = 0.4,
      vertex.col = rgb(odeg/max(odeg), 0, ideg/max(ideg)),
      displaylabels = TRUE, displayisolates = FALSE)
  },
filename = "5_bluSkynet.svg"
)
bet <- betweenness(bluSkynet, gmode = "graph")
bet
gplot(bluSkynet, vertex.cex = sqrt(bet)/25,
      gmode = "graph")

clo <- closeness(bluSkynet, cmode = "suminvundir")
clo

cen <- centralization(bluSkynet, degree, cmode = "indegree")
ceneig <- centralization(bluSkynet, evcent)

gden(bluSkynet)
grecip(bluSkynet)
grecip(bluSkynet, measure = "edgewise")

# Graph Layouts

library(graphlayouts)
# Nicely
sg0 <- ggraph(g2, layout = "nicely") +
  geom_edge_link(width = 0.2, colour = "grey") +
  geom_node_point(aes(size = like_count, color = repost_count)) +
  geom_node_text(aes(label = display_name), repel = TRUE) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_void()
save_graph_svg(sg0, "graphLayout_1.svg")
# Stress
sg <- ggraph(g2, layout = "stress") +
  geom_edge_link(width = 0.2, colour = "grey") +
  geom_node_point(aes(size = like_count, color = repost_count)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_graph()
plot(sg)
save_graph_svg(sg, "graphLayout_2.svg")

# Stress Majorization
sg1 <- ggraph(g2, layout = "stress", bbox = 15) +
  geom_node_point(aes(size = like_count, color = repost_count)) +
  geom_node_text(aes(label = display_name), repel = TRUE) +
  geom_edge_link(width = 0.2, colour = "grey") +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_graph()
plot(sg1)
save_graph_svg(sg1, "graphLayout_3.svg")
# tsna to go here 