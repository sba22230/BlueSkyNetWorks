library(igraph)

# Get node degrees
deg <- degree(g)

# Count frequency of each degree
deg_freq <- table(deg)

# Convert to dataframe
df <- data.frame(
  degree = as.numeric(names(deg_freq)),
  frequency = as.numeric(deg_freq)
)

# Remove degree 0 if desired
df <- subset(df, degree > 0)

# Plot
plot(
  log2(df$degree),
  log2(df$frequency),
  pch = 1,
  col = "blue",
  xlab = expression(logDegree),
  ylab = expression(logFrequency)
)

dd <- degree_distribution(g)


df <- data.frame(
  degree = seq_along(dd) - 1,
  frequency = dd * vcount(g)
)

df <- subset(df,
             degree > 0 &
               frequency > 0)

plot(
  log2(df$degree),
  log2(df$frequency),
  pch = 1,
  col = "blue",
  xlab = expression(logDegree),
  ylab = expression(logFrequency)
)
deg_pos <- deg[deg > 0]

library(poweRlaw)

m <- displ$new(deg_pos)

pl <- displ$new(deg_pos)

est <- estimate_xmin(pl)
pl$setXmin(est)
pl$setPars(estimate_pars(pl))

bs <- bootstrap_p(pl, no_of_sims = 100)

bs$p

summary(deg_pos)

length(deg_pos)

range(deg_pos)

quantile(
  deg_pos,
  probs = c(.5, .9, .95, .99, .999)
)
length(unique(deg_pos))

ln <- dislnorm$new(deg_pos)

pars <- estimate_pars(ln)

pars
plot(
  sort(deg_pos),
  log = "xy"
)
hist(log10(deg_pos), breaks = 100)
est <- estimate_xmin(pl)
est

alpha <- estimate_pars(pl)
alpha

pl$setXmin(est)
pl$setPars(alpha)
plot(pl)
lines(pl, col = "red", lwd = 2)



