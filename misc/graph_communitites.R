# =============================================================================
# COMMUNITY DETECTION EXPERIMENTS
# =============================================================================
# Purpose: Identify clusters of users (communities) in the BlueSky interaction
# network and evaluate which detection algorithm produces the most coherent,
# well-separated communities. Communities here represent groups of users who
# interact with each other significantly more than with the rest of the network.
#
# This script:
#   1. Defines helper functions for quality metrics not built into igraph
#   2. Runs three community detection algorithms on the graph
#   3. Scores each algorithm across four complementary quality metrics
#   4. Ranks algorithms to identify the best-fit community structure
# =============================================================================

source("0_functions.R")

# Load the most recent N posts to build the interaction graph.
# Increasing num_posts gives a denser, more representative graph but is slower.
num_posts <- 15000
g <- get_graph_data(num_posts)


# =============================================================================
# HELPER FUNCTIONS: QUALITY METRICS
# =============================================================================
# igraph provides modularity out of the box, but silhouette, internal density,
# and conductance require custom implementations. These four metrics together
# give a rounded picture of community quality — no single metric is sufficient
# on its own.


# -----------------------------------------------------------------------------
# SILHOUETTE COEFFICIENT (reference / small-graph implementation)
# -----------------------------------------------------------------------------
# The silhouette coefficient measures how similar a node is to its own community
# relative to the nearest neighbouring community. It ranges from -1 to +1:
#   +1  → node is well inside its community (tight cluster, good separation)
#    0  → node sits on the boundary between two communities
#   -1  → node would better fit in a neighbouring community (likely mis-assigned)
#
# We compute a(i) = mean shortest-path distance to all other nodes in the same
# community, and b(i) = minimum mean distance to nodes in any other community.
# s(i) = (b(i) - a(i)) / max(a(i), b(i))
#
# NOTE: This naive implementation computes the full n×n distance matrix and is
# only practical on small graphs (<~2 000 nodes). Use the batched version below
# for larger networks.
silhouette_coefficient <- function(graph, membership, weights = NULL) {
  d <- igraph::distances(graph, weights = weights)
  communities <- unique(membership)
  s <- numeric(igraph::vcount(graph))

  for (node in seq_len(igraph::vcount(graph))) {
    current <- membership[node]

    same <- which(membership == current)
    if (length(same) > 1) {
      # a(i): mean intra-community distance (excluding self)
      a_vals <- d[node, same[same != node]]
      a <- mean(a_vals[is.finite(a_vals)], na.rm = TRUE)
      if (is.nan(a)) a <- 0
    } else {
      # Singleton community — no intra-community distance to compute
      a <- 0
    }

    # b(i): minimum mean distance across all other communities
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

  # Return the mean silhouette score across all nodes; higher is better
  mean(s, na.rm = TRUE)
}


# -----------------------------------------------------------------------------
# SILHOUETTE COEFFICIENT — BATCHED (production implementation)
# -----------------------------------------------------------------------------
# Functionally identical to silhouette_coefficient() above, but processes nodes
# in batches (default 256) to avoid materialising the full n×n distance matrix
# in memory. For a 15 000-post graph this is essential — the naive version would
# require ~1.7 GB just for the distance matrix.
#
# Key optimisations vs. the naive version:
#   - Sparse membership indicator matrix (Matrix package) allows cluster sums
#     to be computed with a single matrix multiply instead of per-community loops
#   - matrixStats::rowMins() replaces an apply() loop for finding b(i)
#   - Dijkstra's algorithm is explicitly requested (efficient on sparse graphs)
silhouette_coefficient_batched <- function(
    graph, membership, weights = NULL,
    mode = c("out", "in", "all"),
    batch_size = 256
) {
  mode <- match.arg(mode)

  n <- igraph::vcount(graph)
  if (length(membership) != n) stop("membership must have length vcount(graph).")

  # Convert membership labels to a compact integer range 1..K
  f <- as.factor(membership)
  grp <- as.integer(f)
  K <- nlevels(f)

  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Please install the Matrix package.")
  }

  # Build an n×K binary indicator matrix: M[i,k] = 1 if node i is in cluster k.
  # Stored as a sparse matrix to keep memory usage low.
  M <- Matrix::sparseMatrix(i = seq_len(n), j = grp, x = 1, dims = c(n, K))
  sizes <- as.numeric(Matrix::colSums(M))   # number of nodes per cluster

  # Use matrixStats for fast row-wise minimums if available; fall back to apply()
  row_mins <- if (requireNamespace("matrixStats", quietly = TRUE)) {
    function(x) matrixStats::rowMins(x, na.rm = TRUE)
  } else {
    function(x) apply(x, 1, function(v) {
      v <- v[is.finite(v)]
      if (length(v) == 0) NA_real_ else min(v)
    })
  }

  s <- numeric(n)

  # Process nodes in batches to cap peak memory at O(batch_size × n)
  starts <- seq.int(1L, n, by = batch_size)
  for (st in starts) {
    en <- min(n, st + batch_size - 1L)
    src <- st:en
    b <- length(src)

    # Compute shortest-path distances from this batch to all other nodes
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
    D0[!finite] <- 0   # treat Inf distances as 0 for summation; excluded via counts

    # Aggregate total reachable distance and reachable-node count per cluster
    sum_to <- D0 %*% M    # batch_size × K matrix of distance sums per cluster
    cnt_to <- (finite * 1) %*% M   # corresponding reachable-node counts

    grp_block <- grp[src]   # cluster assignment for each node in this batch

    # Self-distances (node to itself) are always 0 and must be excluded from a(i)
    self_dist <- D[cbind(seq_len(b), src)]
    self_ok <- is.finite(self_dist)

    own_sum <- sum_to[cbind(seq_len(b), grp_block)] - ifelse(self_ok, self_dist, 0)
    own_cnt <- cnt_to[cbind(seq_len(b), grp_block)] - as.integer(self_ok)

    # a(i): mean intra-community distance
    a <- numeric(b)
    ok_a <- (sizes[grp_block] > 1) & (own_cnt > 0)
    a[ok_a] <- own_sum[ok_a] / own_cnt[ok_a]
    a[!is.finite(a)] <- 0

    # b(i): mean distance to each other cluster; take the minimum across clusters
    means <- sum_to / pmax(cnt_to, 1)
    means[cnt_to == 0] <- NA_real_        # no reachable nodes → undefined
    means[cbind(seq_len(b), grp_block)] <- NA_real_   # mask own cluster

    bval <- row_mins(as.matrix(means))
    bval[!is.finite(bval)] <- 0

    den <- pmax(a, bval)
    s[src] <- ifelse(den == 0, 0, (bval - a) / den)
  }

  # Return mean silhouette score; range [-1, 1], higher is better
  mean(s, na.rm = TRUE)
}


# -----------------------------------------------------------------------------
# INTERNAL DENSITY
# -----------------------------------------------------------------------------
# Internal density measures the fraction of possible edges that actually exist
# within each community (i.e., the density of the induced subgraph). A value of
# 1 means every pair of members is connected; 0 means no internal connections.
#
# We want this to be HIGH — a good community should be densely connected
# internally. We average across communities, ignoring singletons (NA).
internal_density <- function(graph, membership) {
  communities <- unique(membership)
  density_scores <- numeric(length(communities))

  for (i in seq_along(communities)) {
    members <- which(membership == communities[i])

    # Cannot compute density for a single node — skip it
    if (length(members) < 2) {
      density_scores[i] <- NA
      next
    }

    induced_subgraph <- igraph::induced_subgraph(graph, members)
    internal_edges <- igraph::ecount(induced_subgraph)
    n <- length(members)

    # Maximum possible edges differs for directed vs. undirected graphs
    possible_edges <- if (igraph::is.directed(graph)) n * (n - 1) else (n * (n - 1)) / 2

    density_scores[i] <- internal_edges / possible_edges
  }

  # Mean across communities; higher is better
  mean(density_scores, na.rm = TRUE)
}


# -----------------------------------------------------------------------------
# CONDUCTANCE
# -----------------------------------------------------------------------------
# Conductance measures the fraction of a community's total edge volume that
# crosses its boundary into the rest of the graph. Lower conductance means
# fewer edges escape the community — i.e., the community is well-contained.
#
# Formula for community C:
#   conductance(C) = cut(C) / vol(C)
# where cut(C) = edges leaving C, and vol(C) = sum of degrees in C.
# Here cut = total_degree - 2 × internal_edges (undirected interpretation).
#
# We want this to be LOW — communities should be relatively self-contained
# rather than leaking connections out to the wider network.
conductance <- function(graph, membership) {
  communities <- unique(membership)
  cond_scores <- numeric(length(communities))

  for (i in seq_along(communities)) {
    members <- which(membership == communities[i])
    induced_subgraph <- induced_subgraph(graph, members)
    internal_edges <- ecount(induced_subgraph)
    total_degree <- sum(degree(graph, members))

    if (total_degree > 0) {
      # Edges leaving the community = total degree − 2 × internal edges
      # (each internal edge contributes 2 to total degree)
      cond_scores[i] <- (total_degree - 2 * internal_edges) / total_degree
    }
    # If total_degree == 0 (isolated cluster), conductance stays 0 (perfectly contained)
  }

  # Mean across communities; lower is better
  mean(cond_scores, na.rm = TRUE)
}


# =============================================================================
# 1. RUN COMMUNITY DETECTION ALGORITHMS
# =============================================================================
# Three algorithms are compared because each makes different structural
# assumptions and has different performance / resolution trade-offs:
#
#   Walktrap  — random-walk based; works well on directed graphs and tends to
#               find many small, tight communities. Good at capturing fine-grained
#               sub-groups but can over-fragment the network.
#
#   Louvain   — greedy modularity optimisation; fast and scales to large graphs.
#               Requires an undirected graph, so mutual edges are collapsed first.
#               Typically finds fewer, larger communities.
#
#   Leiden    — improved version of Louvain that guarantees communities are
#               well-connected internally (Louvain can produce disconnected ones).
#               The n_iterations parameter controls how thoroughly it refines.
# =============================================================================

# --- Walktrap ---
# steps = 6 gives a balance between short (local) and long (global) random walks.
# Longer walks merge more communities; shorter walks fragment more.
community_walktrap <- cluster_walktrap(g, weights = E(g)$weight, steps = 6, merges = TRUE, modularity = TRUE, membership = TRUE)
membership_walktrap <- membership(community_walktrap)

# --- Louvain ---
# Louvain requires an undirected graph; 'collapse' mode merges reciprocal edges
# into a single undirected edge, preserving the combined weight.
# resolution = 1 is the standard modularity resolution (higher → smaller communities).
g_undir <- as_undirected(g, mode = 'collapse')
community_louvain <- cluster_louvain(g_undir, E(g_undir)$weight, resolution = 1)
membership_louvain <- membership(community_louvain)

# --- Leiden ---
# Uses the pre-computed V(g)$community as a warm start (initial_membership) to
# speed up convergence; n_iterations = 45 allows thorough refinement.
# Converted to undirected here to use modularity as the objective function
# (CPM objective would support directed graphs natively).
community_leiden <- cluster_leiden(
  as_undirected(g),
  objective_function = 'modularity',
  n_iterations = 45,
  initial_membership = V(g)$community,
  resolution = 1
)
membership_leiden <- membership(community_leiden)


# =============================================================================
# 2. CALCULATE QUALITY METRICS
# =============================================================================
# Each metric captures a different dimension of community quality. Together they
# help identify which algorithm best reflects genuine network structure:
#
#   Modularity      — how much denser are intra-community edges vs. a random
#                     graph? Range [-0.5, 1]; higher is better. Standard baseline
#                     metric, but known to have a resolution limit (tends to miss
#                     small communities and merge large ones).
#
#   Conductance     — fraction of edge flow escaping each community. Lower means
#                     communities are more self-contained. Particularly informative
#                     for flow-based interpretations (e.g., information spread).
#
#   Internal Density — fraction of internal edges present vs. possible. Higher
#                     means tighter, more cohesive communities. Useful sanity-check
#                     that communities aren't just large, sparse blobs.
#
#   Silhouette      — graph-distance analogue of the classic clustering metric.
#                     Captures both cohesion and separation simultaneously.
#                     Range [-1, 1]; higher (less negative) is better. Negative
#                     values indicate many nodes are closer to other communities
#                     than their own — a sign of poor fit.
# =============================================================================

# Modularity: measures strength of community structure vs. a random null model
# (higher is better; range -0.5 to 1)
mod_walktrap <- modularity(g, membership_walktrap)
mod_louvain  <- modularity(g_undir, membership_louvain)  # must use the undirected graph Louvain ran on
mod_leiden   <- modularity(as_undirected(g), membership_leiden)

# Conductance: fraction of edge flow leaving each community (lower is better)
cond_walktrap <- conductance(g, membership_walktrap)
cond_louvain  <- conductance(g_undir, membership_louvain)
cond_leiden   <- conductance(as_undirected(g), membership_leiden)

# Internal density: how tightly connected each community is internally (higher is better)
dens_walktrap <- internal_density(g, membership_walktrap)
dens_louvain  <- internal_density(g, membership_louvain)
dens_leiden   <- internal_density(g, membership_leiden)

# Silhouette: node-level cohesion vs. separation, aggregated to algorithm level
# (higher / less negative is better; batched version used for memory efficiency)
sil_walktrap <- silhouette_coefficient_batched(g, membership_walktrap, mode = 'all', batch_size = 128)
sil_louvain  <- silhouette_coefficient_batched(g, membership_louvain,  mode = 'all', batch_size = 128)
sil_leiden   <- silhouette_coefficient_batched(g, membership_leiden,   mode = 'all', batch_size = 128)


# =============================================================================
# 3. COMPILE RESULTS
# =============================================================================

results <- data.frame(
  Algorithm        = c('Walktrap', 'Louvain', 'Leiden'),
  Modularity       = c(mod_walktrap,  mod_louvain,  mod_leiden),
  Conductance      = c(cond_walktrap, cond_louvain, cond_leiden),
  Internal_Density = c(dens_walktrap, dens_louvain, dens_leiden),
  Silhouette       = c(sil_walktrap,  sil_louvain,  sil_leiden),
  Num_Communities  = c(
    length(unique(membership_walktrap)),
    length(unique(membership_louvain)),
    length(unique(membership_leiden))
  )
)

print(results)
datatable(results)

# Reference output from a prior run (useful for sanity-checking after parameter changes):
# results <- data.frame(
#   Algorithm        = c("Walktrap","Louvain","Leiden"),
#   Modularity       = c(0.3997713, 0.4537625, 0.1181706),
#   Conductance      = c(0.9806384, 0.2740777, 0.07993219),
#   Internal_Density = c(0.3386445, 0.1689237, 0.5713206),
#   Silhouette       = c(-0.1482751, -0.5408794, -0.6103554),
#   Num_Communities  = c(5468, 44, 24)
# )


# =============================================================================
# 4. RANK ALGORITHMS
# =============================================================================
# Because the four metrics are on different scales and point in different
# directions (some higher-is-better, some lower-is-better), we convert each to
# a per-metric rank (1 = best across algorithms) and then compute a weighted
# average rank score. The algorithm with the lowest total score wins.
#
# The equal-weight run treats all metrics as equally important. The
# weighted run emphasises Conductance and Silhouette — prioritising community
# separation and boundary quality over raw internal density. Adjust weights to
# match the downstream use case (e.g., for propagation modelling, conductance
# matters more; for user-segmentation, silhouette may be more relevant).
# =============================================================================

rank_results <- function(results,
                         weights = c(Modularity       = 0.25,
                                     Conductance      = 0.25,
                                     Internal_Density = 0.25,
                                     Silhouette       = 0.25),
                         ties.method = "average") {

  ranked <- results

  # Assign per-metric ranks: rank() returns 1 for the best value
  # Negate metrics where higher is better so rank() still gives 1 to the best
  ranked$Rank_Modularity       <- rank(-ranked$Modularity,        ties.method = ties.method)
  ranked$Rank_Conductance      <- rank( ranked$Conductance,        ties.method = ties.method)  # lower is better
  ranked$Rank_Internal_Density <- rank(-ranked$Internal_Density,  ties.method = ties.method)
  ranked$Rank_Silhouette       <- rank(-ranked$Silhouette,         ties.method = ties.method)  # higher (less negative) is better

  # Normalise weights to sum to 1 in case the caller passes unnormalised values
  weights <- weights[c("Modularity", "Conductance", "Internal_Density", "Silhouette")]
  weights <- weights / sum(weights)

  # Weighted composite rank score — lower total score → better overall algorithm
  ranked$Total_Rank_Score <-
    weights["Modularity"]       * ranked$Rank_Modularity +
    weights["Conductance"]      * ranked$Rank_Conductance +
    weights["Internal_Density"] * ranked$Rank_Internal_Density +
    weights["Silhouette"]       * ranked$Rank_Silhouette

  # Translate composite score into a final ordinal rank (1 = best overall)
  ranked$Final_Rank <- rank(ranked$Total_Rank_Score, ties.method = ties.method)

  ranked <- ranked[order(ranked$Final_Rank, ranked$Total_Rank_Score), ]
  ranked
}

# Equal-weight ranking: all four metrics contribute equally
ranked_table <- rank_results(results)
datatable(ranked_table)

# Emphasis on separation and boundary quality (higher Conductance + Silhouette weights).
# Use this variant when the goal is to find communities that are meaningfully
# distinct from each other (e.g., for targeted content or audience segmentation).
ranked_table_weighted <- rank_results(
  results,
  weights = c(Modularity = 0.20, Conductance = 0.35, Internal_Density = 0.15, Silhouette = 0.30)
)
datatable(ranked_table_weighted)


# =============================================================================
# 5. PERSIST RESULTS
# =============================================================================
# Append to CSV so successive runs can be compared over time (e.g., as the
# network grows). col.names is only written on the first run.

output_path <- "misc/ranked_table_weighted.csv"
write.table(
  ranked_table_weighted,
  file      = output_path,
  sep       = ",",
  col.names = !file.exists(output_path),  # write header only if file is new
  row.names = FALSE,
  append    = TRUE
)
