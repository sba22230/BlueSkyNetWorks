source("0_functions.R")

# Step 1: Load data
posts_df <- read_parquet("data/speirgorm_network.parquet")
# reposts_df <- read_parquet("data/speirgorm_reposts.parquet")
# edges <- read_parquet("graphs/speirgorm_edges.parquet")
# nodes <- read_parquet("graphs/speirgorm_nodes.parquet")
# --- Optional: sample reposted posts and filter reposts_df -----------------
# Filter posts that have been reposted, sample them reproducibly, then
# keep only repost records for those sampled original posts. Adjust
# `sample_n_posts` as desired (or set to NULL to keep all reposts).

set.seed(22230)

num_posts <- 600
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

g1 <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
coords <- layout_with_drl(g1)
V(g1)$x <- coords[, 1]
V(g1)$y <- coords[, 2]

ggraph(g1, layout = "manual", x = V(g1)$x, y = V(g1)$y) +
  geom_edge_parallel0(
    aes(width = repost_count, colour = like_count),
    edge_alpha = 1,
    arrow = arrow(
      angle = 30,
      length = unit(0.15, "inches"),
      ends = "last",
      type = "closed"
    )
  ) +
  scale_edge_colour_gradient(low = "#87CEFF", high = "#27408B") +
  scale_edge_width(range = c(0.3, 1.2)) +
  geom_node_point(
    aes(fill = posts_authored, size = reposts_received),
    colour = "#000000",
    shape = 21,
    stroke = 0.3
  ) +
  scale_fill_gradient(low = "#87CEFF", high = "#27408B") +
  scale_size(range = c(3, 8)) +
  theme_graph() +
  theme(legend.position = "top")


summary_df <- tibble(
  total_users = vcount(g2),
  total_replies = ecount(g2),
  density = edge_density(g2),
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
plot(g3)
save_graph_svg(g3, "g3_with_drl.svg")

# Start of Statnet analysis
# Convert igraph to statnet
library(network)
bluSkynet <- asNetwork(g2)
summary(bluSkynet)

bluSkynet %v% "vertex.names"
bluSkynet[,]
list.edge.attributes(bluSkynet)
bluSkynet %e% "edgeStarts"
#as.sociomatrix.sna(bluSkynet, "created_at")

save_graph_svg(
  plot_or_expr = function() {
    sna::gplot(bluSkynet)
  },
  filename = "1_bluSkynet_sna.svg"
)

save_graph_svg(
  plot_or_expr = function() {
    sna::gplot(
      bluSkynet,
      mode = "fruchtermanreingold",
      layout.par = list(niter = n_iter)
    ) # very slow
  },
  filename = "2_bluSkynet_sna.svg"
)

save_graph_svg(
  plot_or_expr = function() {
    plot(bluSkynet, displaylabels = FALSE)
  },
  filename = "3_bluSkynet_sna.svg"
)

save_graph_svg(
  plot_or_expr = function() {
    gplot(bluSkynet, label.cex - 0.2, label.col = "blue", displaylabels = FALSE)
    # graph is very messy with labels
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

save_graph_svg(
  plot_or_expr = function() {
    gplot(
      bluSkynet,
      vertex.cex = (ideg + odeg)^0.5,
      vertex.sides = 50,
      label.cex = 0.4,
      vertex.col = rgb(odeg / max(odeg), 0, ideg / max(ideg)),
      displaylabels = TRUE,
      displayisolates = FALSE
    )
  },
  filename = "5_bluSkynet.svg"
)

bet <- betweenness(bluSkynet, gmode = "graph")
bet

gplot(bluSkynet, vertex.cex = sqrt(bet) / 25, gmode = "graph")

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
  geom_node_point(aes(size = total_likes_on_posts, color = reposts_received)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_graph()
save_graph_svg(sg0, "graphLayout_1.svg")
# Stress
sg <- ggraph(g2, layout = "stress") +
  geom_edge_link(width = 0.2, colour = "grey") +
  geom_node_point(aes(size = total_likes_on_posts, color = reposts_received)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_graph()
plot(sg)
save_graph_svg(sg, "graphLayout_2.svg")

# Stress Majorization
sg1 <- ggraph(g2, layout = "stress", bbox = 15) +
  geom_node_point(aes(size = total_likes_on_posts, color = reposts_received)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  geom_edge_link(width = 0.2, colour = "grey") +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_graph()
plot(sg1)
save_graph_svg(sg1, "graphLayout_3.svg")

# tsna to go here
library(ndtv)
library(networkDynamic)
library(tsna)
# nodes$name is your vertex key
n <- nrow(nodes)

name_to_id <- setNames(seq_len(n), nodes$name)
stopifnot(!any(is.na(name_to_id[nodes$name])))
edges2 <- edges %>%
  dplyr::mutate(
    from_id = name_to_id[from],
    to_id = name_to_id[to]
  )
stopifnot(!any(is.na(edges2$from_id)))
stopifnot(!any(is.na(edges2$to_id)))
edges2 <- edges2 %>%
  dplyr::mutate(
    edgeStarts = edgeStarts,
    edgeEnds = edgeEnds # or NA if open-ended
  )
library(network)
library(networkDynamic)

bluSkynet2 <- network.initialize(n, directed = TRUE)
bluSkynet2 %v% "name" <- nodes$name

add.edges(
  bluSkynet2,
  tail = edges2$from_id,
  head = edges2$to_id
)

network.edgecount(bluSkynet2) # should equal nrow(edges2)
vertex_spells <- data.frame(
  vertex.id = as.integer(name_to_id[nodes$name]),
  onset = as.POSIXct(nodes$earliestPost),
  terminus = as.POSIXct(nodes$latestPost),
  stringsAsFactors = FALSE
)

# strip to a plain data.frame with simple rownames
attributes(vertex_spells) <- list(
  names = names(vertex_spells),
  class = "data.frame",
  row.names = seq_len(nrow(vertex_spells))
)
edge_spells <- data.frame(
  edge.id = seq_len(nrow(edges2)),
  onset = as.POSIXct(edges2$edgeStarts),
  terminus = as.POSIXct(edges2$edgeEnds),
  stringsAsFactors = FALSE
)

attributes(edge_spells) <- list(
  names = names(edge_spells),
  class = "data.frame",
  row.names = seq_len(nrow(edge_spells))
)
dynNet <- networkDynamic(
  base.net = bluSkynet2,
  vertex.spells = vertex_spells,
  edge.spells = edge_spells
)
str(vertex_spells)
str(edge_spells)
network.size(bluSkynet2)
network.edgecount(bluSkynet2)
