plot_word_comparison_date_plot <- function(graph, community_id) {
  data <- plot_word_comparison_date(graph, community_id)

  with(data, {
    plot(
      top_words$log_odds,
      top_words$freq,
      main = title_text,
      xlab = "Log odds",
      ylab = "Frequency",
      pch = 19,
      col = "steelblue"
    )
  })
}


save_graph_svg(
  expression({
    par(mfrow = c(1, 2))

    ## Panel 1: igraph
    plot.igraph(
      sub_g,
      layout = layout_nicely(sub_g, dim = 2),
      main = main_title,
      vertex.size = vsize,
      vertex.label.cex = 0.2,
      edge.arrow.size = 0.3,
      vertex.color = vcol
    )

    ## Panel 2: word comparison
    plot_data <- plot_word_comparison_date(sub_g, ym)

    x_main <- as.numeric(plot_data$frequency$median_time)

    plot(
      x_main,
      plot_data$frequency$log_odds,
      pch = 16,
      col = rgb(0.12, 0.47, 0.71, 0.4),
      xlab = "Median posting time",
      ylab = "Distinctiveness (Dirichlet log-odds)",
      main = plot_data$title_text,
      sub = "Distinctive (blue bold) and common (green bold) words",
      xaxt = "n" # suppress default x-axis
    )

    # Proper date labels
    axis(
      1,
      at = as.numeric(plot_data$frequency$median_time),
      labels = format(plot_data$frequency$median_time, "%b %d"),
      las = 2
    )

    # Helper for jittering Date values
    jitter_date <- function(x, amount = 0.2) jitter(as.numeric(x), amount)

    # Label distinctive words
    text(
      jitter_date(plot_data$top_words$median_time, amount = 0.05),
      jitter(plot_data$top_words$log_odds, amount = 0.07),
      labels = plot_data$top_words$word,
      cex = 0.7,
      col = "royalblue",
      pos = 3
    )

    # Label common words
    text(
      jitter_date(plot_data$common_words$median_time, amount = 0.1),
      jitter(plot_data$common_words$log_odds, amount = 0.1),
      labels = plot_data$common_words$word,
      cex = 0.6,
      col = "forestgreen",
      pos = 1
    )
  }),
  filename = "test_subgraph_code.svg"
)


f <- tempfile(fileext = ".svg")
svg(f, width = 10, height = 8)
layout(grid_matrix)

  plot(1:10, main = "plot1 (left column - Full height)")
  plot(1:15, main = "plot 2 (right column - row 1)")
  plot(2:20, main = "plot 3 (right column - row 2)")
  plot(10:1, main = "plot 4 (right column - row 3)")

dev.off()
