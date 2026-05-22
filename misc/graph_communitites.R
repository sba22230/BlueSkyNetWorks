# communities experiments

source("0_functions.R")

#Functions

# ============================================
# SILHOUETTE COEFFICIENT
# ============================================

silhouette_coefficient <- function(graph, membership, weights = NULL) {
  d <- igraph::distances(graph, weights = weights)
  communities <- unique(membership)
  s <- numeric(igraph::vcount(graph))
  
  for (node in seq_len(igraph::vcount(graph))) {
    current <- membership[node]
    
    same <- which(membership == current)
    if (length(same) > 1) {
      a_vals <- d[node, same[same != node]]
      a <- mean(a_vals[is.finite(a_vals)], na.rm = TRUE)
      if (is.nan(a)) a <- 0
    } else {
      a <- 0
    }
    
    b <- Inf
    for (comm in communities[communities != current]) {
      other <- which(membership == comm)
      b_vals <- d[node, other]
      b_mean <- mean(b_vals[is.finite(b_vals)], na.rm = TRUE)
      if (!is.nan(b_mean)) b <- min(b, b_mean)
    }
    
    if (!is.finite(b)) b <- 0
    den <- max(a, b)
    s[node] <- if (den == 0) 0 else (b - a) / den
  }
  
  mean(s, na.rm = TRUE)
}

silhouette_coefficient_batched <- function(
    graph, membership, weights = NULL,
    mode = c("out", "in", "all"),
    batch_size = 256
) {
  mode <- match.arg(mode)
  
  n <- igraph::vcount(graph)
  if (length(membership) != n) stop("membership must have length vcount(graph).")
  
  f <- as.factor(membership)
  grp <- as.integer(f)
  K <- nlevels(f)
  
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Please install the Matrix package.")
  }
  
  # n x K sparse membership indicator
  M <- Matrix::sparseMatrix(i = seq_len(n), j = grp, x = 1, dims = c(n, K))
  sizes <- as.numeric(Matrix::colSums(M))
  
  # row-wise min ignoring NA (fast if matrixStats is installed)
  row_mins <- if (requireNamespace("matrixStats", quietly = TRUE)) {
    function(x) matrixStats::rowMins(x, na.rm = TRUE)
  } else {
    function(x) apply(x, 1, function(v) {
      v <- v[is.finite(v)]
      if (length(v) == 0) NA_real_ else min(v)
    })
  }
  
  s <- numeric(n)
  
  starts <- seq.int(1L, n, by = batch_size)
  for (st in starts) {
    en <- min(n, st + batch_size - 1L)
    src <- st:en
    b <- length(src)
    
    # Distances from this batch of sources to all vertices
    D <- igraph::distances(
      graph,
      v = src,
      to = igraph::V(graph),
      weights = weights,
      mode = mode,
      algorithm = "dijkstra"
    )
    
    finite <- is.finite(D)
    D0 <- D
    D0[!finite] <- 0
    
    # Aggregate sums and counts to clusters
    sum_to <- D0 %*% M
    cnt_to <- (finite * 1) %*% M
    
    grp_block <- grp[src]
    
    # Self distances are column == vertex id (since to = V(graph) is 1..n)
    self_dist <- D[cbind(seq_len(b), src)]
    self_ok <- is.finite(self_dist)
    
    own_sum <- sum_to[cbind(seq_len(b), grp_block)] - ifelse(self_ok, self_dist, 0)
    own_cnt <- cnt_to[cbind(seq_len(b), grp_block)] - as.integer(self_ok)
    
    # a(i)
    a <- numeric(b)
    ok_a <- (sizes[grp_block] > 1) & (own_cnt > 0)
    a[ok_a] <- own_sum[ok_a] / own_cnt[ok_a]
    a[!is.finite(a)] <- 0
    
    # b(i): min mean distance to other clusters
    means <- sum_to / pmax(cnt_to, 1)
    means[cnt_to == 0] <- NA_real_
    means[cbind(seq_len(b), grp_block)] <- NA_real_
    
    bval <- row_mins(as.matrix(means))
    bval[!is.finite(bval)] <- 0
    
    den <- pmax(a, bval)
    s[src] <- ifelse(den == 0, 0, (bval - a) / den)
  }
  
  mean(s, na.rm = TRUE)
}
# ============================================
# INTERNAL DENSITY
# ============================================

internal_density <- function(graph, membership) {
  communities <- unique(membership)
  density_scores <- numeric(length(communities))
  
  for (i in seq_along(communities)) {
    members <- which(membership == communities[i])
    
    if (length(members) < 2) {
      density_scores[i] <- NA
      next
    }
    
    induced_subgraph <- igraph::induced_subgraph(graph, members)
    internal_edges <- igraph::ecount(induced_subgraph)
    n <- length(members)
    
    possible_edges <- if (igraph::is.directed(graph)) n * (n - 1) else (n * (n - 1)) / 2
    
    density_scores[i] <- internal_edges / possible_edges
  }
  
  mean(density_scores, na.rm = TRUE)
}

# Conductance (lower is better, measures edge flow out of communities)
conductance <- function(graph, membership) {
  communities <- unique(membership)
  cond_scores <- numeric(length(communities))
  
  for (i in seq_along(communities)) {
    members <- which(membership == communities[i])
    induced_subgraph <- induced_subgraph(graph, members)
    internal_edges <- ecount(induced_subgraph)
    total_degree <- sum(degree(graph, members))
    
    if (total_degree > 0) {
      cond_scores[i] <- (total_degree - 2 * internal_edges) / total_degree
    }
  }
  mean(cond_scores, na.rm = TRUE)
}

# ============================================
# 1. RUN COMMUNITY DETECTION ALGORITHMS
# ============================================

# Walktrap
community_walktrap <- cluster_walktrap(g, weights = E(g)$weight, steps = 6, merges = TRUE, modularity = TRUE, membership = TRUE)
membership_walktrap <- membership(community_walktrap)

# Louvain — requires undirected graph; collapse mutual edges into one
g_undir <- as_undirected(g, mode = 'collapse')
community_louvain <- cluster_louvain(g_undir, E(g_undir)$weight,resolution = 0.1)
membership_louvain <- membership(community_louvain)

# Leiden — supports directed graphs natively

community_leiden  <- cluster_leiden(
  as_undirected(g),
  objective_function = 'modularity',
  n_iterations = 45,
  initial_membership = nodes$community , resolution = 1
)

membership_leiden <- membership(community_leiden)

# ============================================
# 2. CALCULATE QUALITY METRICS
# ============================================

# Modularity (higher is better, range: -0.5 to 1)
mod_walktrap <- modularity(g, membership_walktrap)
mod_louvain  <- modularity(g_undir, membership_louvain)  # use undirected graph to match Louvain input
mod_leiden   <- modularity(as_undirected(g), membership_leiden)

# Conductance
cond_walktrap <- conductance(g, membership_walktrap)
cond_louvain  <- conductance(g_undir, membership_louvain)
cond_leiden   <- conductance(as_undirected(g), membership_leiden)



# ============================================
# ADD TO RESULTS
# ============================================

# Calculate new metrics
dens_walktrap <- internal_density(g, membership_walktrap)
dens_louvain <- internal_density(g, membership_louvain)
dens_leiden <- internal_density(g, membership_leiden)

sil_walktrap <- silhouette_coefficient_batched(g, membership_walktrap, mode = 'all', batch_size = 128)
sil_louvain <- silhouette_coefficient_batched(g, membership_louvain, mode = 'all', batch_size = 128)
sil_leiden <- silhouette_coefficient_batched(g, membership_leiden, mode = 'all', batch_size = 128)

# Enhanced results table
results <- data.frame(
  Algorithm = c('Walktrap', 'Louvain', 'Leiden'),
  Modularity = c(mod_walktrap, mod_louvain, mod_leiden),
  Conductance = c(cond_walktrap, cond_louvain, cond_leiden),
  Internal_Density = c(dens_walktrap, dens_louvain, dens_leiden),
  Silhouette = c(sil_walktrap, sil_louvain, sil_leiden),
  Num_Communities = c(
    length(unique(membership_walktrap)),
    length(unique(membership_louvain)),
    length(unique(membership_leiden))
  )
)

print(results)
datatable(results)
# --- Your results data frame should look like this:
# results <- data.frame(
#   Algorithm = c("Walktrap","Louvain","Leiden"),
#   Modularity = c(0.3997713, 0.4537625, 0.1181706),
#   Conductance = c(0.9806384, 0.2740777, 0.07993219),
#   Internal_Density = c(0.3386445, 0.1689237, 0.5713206),
#   Silhouette = c(-0.1482751, -0.5408794, -0.6103554),
#   Num_Communities = c(5468, 44, 24)
# )

rank_results <- function(results,
                         weights = c(Modularity = 0.25,
                                     Conductance = 0.25,
                                     Internal_Density = 0.25,
                                     Silhouette = 0.25),
                         ties.method = "average") {
  
  ranked <- results
  
  # Per-metric ranks (1 = best)
  ranked$Rank_Modularity       <- rank(-ranked$Modularity, ties.method = ties.method)
  ranked$Rank_Conductance      <- rank( ranked$Conductance, ties.method = ties.method) # lower is better
  ranked$Rank_Internal_Density <- rank(-ranked$Internal_Density, ties.method = ties.method)
  ranked$Rank_Silhouette       <- rank(-ranked$Silhouette, ties.method = ties.method) # higher (less negative) is better
  
  # Ensure weights are in the right order and sum to 1
  weights <- weights[c("Modularity","Conductance","Internal_Density","Silhouette")]
  weights <- weights / sum(weights)
  
  # Weighted total rank score (lower is better)
  ranked$Total_Rank_Score <- weights["Modularity"]       * ranked$Rank_Modularity +
    weights["Conductance"]      * ranked$Rank_Conductance +
    weights["Internal_Density"] * ranked$Rank_Internal_Density +
    weights["Silhouette"]       * ranked$Rank_Silhouette
  
  # Final rank based on total score
  ranked$Final_Rank <- rank(ranked$Total_Rank_Score, ties.method = ties.method)
  
  # Sort best to worst
  ranked <- ranked[order(ranked$Final_Rank, ranked$Total_Rank_Score), ]
  
  ranked
}

# --- Example usage (equal weights):
ranked_table <- rank_results(results)
datatable(ranked_table)

# --- Example usage (if you want to emphasise separation & boundaries more):
ranked_table_weighted <- rank_results(
  results,
  weights = c(Modularity = 0.20, Conductance = 0.35, Internal_Density = 0.15, Silhouette = 0.30)
)
datatable(ranked_table_weighted)
