# source("3_StatnetAnalysis.R")

g2 <- rxReadObject(
  ds_Graphs,
  "g4_sampled_Graph - igraph - no posts: num_posts 5000"
)

# Network is already created in 3_StatnetAnalysis.R as 'g4_sample'

coords <- layout_with_drl(g2)
V(g2)$x <- coords[, 1]
V(g2)$y <- coords[, 2]

ggraph(g2, layout = "manual", x = V(g2)$x, y = V(g2)$y) +
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

plot(g3)
save_graph_svg(g3, "g3_with_drl.svg")

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

gplot(bluSkynet, vertex.cex = sqrt(bet) / 25, gmode = "graph")
