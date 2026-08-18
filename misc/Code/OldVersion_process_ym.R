# Worker function: processes one year-month slice
process_ym <- function(
  ym,
  year_month,
  g,
  pal,
  ds_Graphs,
  save_graph_svg,
  save_network_svg,
  plot_word_comparison_date,
  render_word_comparison_date
) {
  library(dplyr)
  library(ggrepel)
  library(stringr)
  library(tidytext)
  library(lubridate)
  library(igraph)
  library(magrittr)

  eids <- which(year_month == ym)
  if (length(eids) == 0) {
    return(NULL)
  }

  sub_g <- igraph::subgraph.edges(g, eids, delete.vertices = TRUE)
  sub_net <- asNetwork(sub_g)

  cat(
    "Created subgraph for",
    ym,
    "with",
    vcount(sub_g),
    "nodes and",
    ecount(sub_g),
    "edges\n"
  )

  main_title <- paste0("Sub Graph ", vcount(sub_g), " nodes\n(", ym, ")")

  # Precompute vertex aesthetics (bug fix: subg -> sub_g)
  vsize <- if (!is.null(V(sub_g)$pagerank)) {
    V(sub_g)$pagerank + 1
  } else {
    rep(6, vcount(sub_g))
  }
  vcol <- if (!is.null(V(sub_g)$kcore)) {
    pal[as.integer(V(sub_g)$kcore) + 1]
  } else {
    "steelblue"
  }

  out_file <- paste0("SubGraph_", ym, ".svg")
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
    folder = "images",
    filename = out_file
  )

  # Each worker writes its own keys — no contention
  rxWriteObject(
    ds_Graphs,
    paste0("subgraph_igraph_", ym),
    sub_g,
    overwrite = TRUE
  )
  rxWriteObject(
    ds_Graphs,
    paste0("subgraph_statnet_", ym),
    sub_net,
    overwrite = TRUE
  )

  list(ym = ym, sub_g = sub_g, sub_net = sub_net)
}
