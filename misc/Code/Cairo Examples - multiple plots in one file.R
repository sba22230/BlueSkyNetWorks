# Install required packages if missing
pkgs <- c("Cairo", "ggplot2", "igraph", "gridExtra", "grid")
to_install <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(Cairo)
library(ggplot2)
library(igraph)
library(gridExtra)
library(grid)

# --- 1. Create a ggplot2 plot ---
df <- data.frame(
  x = 1:10,
  y = (1:10) + rnorm(10)
)

p1 <- ggplot(df, aes(x, y)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("ggplot2: Scatter with Regression") +
  theme_minimal()

# --- 2. Create an igraph plot and capture as a grid object ---
g <- make_ring(8) %>%
  set_vertex_attr("color", value = rainbow(8)) %>%
  set_vertex_attr("size", value = 20)

# Function to convert igraph plot to a grid object
igraphGrob <- function(graph) {
  # Capture igraph plot into a temporary file
  tmp <- tempfile(fileext = ".png")
  png(tmp, width = 800, height = 800)
  plot(graph, vertex.label = NA)
  dev.off()
  
  # Read back as rasterGrob
  img <- png::readPNG(tmp)
  unlink(tmp)
  rasterGrob(img, interpolate = TRUE)
}

p2 <- igraphGrob(g)

# --- 3. Output both plots into one SVG using Cairo ---
CairoSVG("ggplot_and_igraph.svg", width = 12, height = 6)

grid.arrange(p1, p2, ncol = 2)

dev.off()

cat("SVG file 'ggplot_and_igraph.svg' created in working directory.\n")


library(Cairo)

# Multiple pages in one PDF
CairoPDF("multi_page.pdf", onefile = TRUE)
plot(1:5, main = "Page 1")
plot(5:1, main = "Page 2")
dev.off()  # PDF will have 2 pages

# Single page PDF
CairoPDF("single_page.pdf", onefile = FALSE)
plot(1:5, main = "Only Page")
plot(5:1, main = "This won't be a new page")
dev.off()  # PDF will have only the first plot


library(igraph)
library(grid)
library(png)

# Function to capture igraph plot as a grid object
igraph_to_grob <- function(graph, main_title, vsize, vcol) {
  # Create a temporary PNG file
  tmp <- tempfile(fileext = ".png")
  
  # Render igraph plot to PNG (off-screen)
  png(tmp, width = 800, height = 800, res = 150)
  par(mar = c(1, 1, 2, 1))  # margins
  plot.igraph(
    graph,
    layout = layout_nicely(graph, dim = 2),
    main = main_title,
    vertex.size = vsize,
    vertex.label.cex = 0.2,
    edge.arrow.size = 0.3,
    vertex.color = vcol
  )
  dev.off()
  
  # Read PNG back into R and wrap as a grob
  img <- png::readPNG(tmp)
  unlink(tmp)  # clean up temp file
  rasterGrob(img, interpolate = TRUE)
}


