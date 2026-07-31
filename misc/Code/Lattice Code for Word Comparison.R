library(lattice)

xyplot(
  frequency[[cols[2]]] ~ frequency[[cols[1]]],
  scales = list(
    x = list(log = 10),
    y = list(log = 10)
  ),
  xlab = paste0("Frequency by ", name1),
  ylab = paste0("Frequency by ", name2),
  panel = function(x, y, ...) {
    
    # jitter for drawing only
    xj <- jitter(x, amount = 0.25)
    yj <- jitter(y, amount = 0.25)
    
    panel.points(
      xj, yj,
      col = rgb(0, 0, 0, 0.1),
      pch = 16,
      cex = 1.5
    )
    
    panel.text(
      xj, yj,
      labels = frequency$word,
      pos = 3,
      cex = 0.7
    )
    
    panel.abline(a = 0, b = 1, col = "red", lty = 2)
  }
)
