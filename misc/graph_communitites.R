# communities experiments

source("0_functions.R")

#Functions


# ============================================
# SILHOUETTE COEFFICIENT
# ============================================

silhouette_coefficient <- function(graph, membership) {
  # Calculate shortest path distances
  distances <- shortest.paths(graph)
  communities <- unique(membership)
  silhouette_scores <- numeric(vcount(graph))
  
  for (node in seq_len(vcount(graph))) {
    current_community <- membership[node]
    
    # Average distance to nodes in own community
    same_community <- which(membership == current_community)
    if (length(same_community) > 1) {
      a <- mean(distances[node, same_community[-which(same_community == node)]])
    } else {
      a <- 0
    }
    
    # Minimum average distance to other communities
    b <- Inf
    for (comm in communities[communities != current_community]) {
      other_community <- which(membership == comm)
      avg_dist <- mean(distances[node, other_community])
      b <- min(b, avg_dist)
    }
    
    if (b == Inf) b <- 0
    
    # Calculate silhouette value
    silhouette_scores[node] <- (b - a) / max(a, b)
  }
  
  mean(silhouette_scores, na.rm = TRUE)
}

# ============================================
# INTERNAL DENSITY
# ============================================

internal_density <- function(graph, membership) {
  communities <- unique(membership)
  density_scores <- numeric(length(communities))
  
  for (i in seq_along(communities)) {
    members <- which(membership == communities[i])
    
    # Skip single-node communities
    if (length(members) < 2) {
      density_scores[i] <- NA
      next
    }
    
    induced_subgraph <- induced_subgraph(graph, members)
    internal_edges <- ecount(induced_subgraph)
    possible_edges <- (length(members) * (length(members) - 1)) / 2
    
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
community_walktrap <- cluster_walktrap(g, steps = 4)
membership_walktrap <- membership(community_walktrap)

# Louvain — requires undirected graph; collapse mutual edges into one
g_undir <- as.undirected(g, mode = "collapse")
community_louvain <- cluster_louvain(g_undir)
membership_louvain <- membership(community_louvain)

# Leiden — supports directed graphs natively
community_leiden <- cluster_leiden(g_undir, resolution = 0.1)
membership_leiden <- membership(community_leiden)

# ============================================
# 2. CALCULATE QUALITY METRICS
# ============================================

# Modularity (higher is better, range: -0.5 to 1)
mod_walktrap <- modularity(g, membership_walktrap)
mod_louvain  <- modularity(g_undir, membership_louvain)  # use undirected graph to match Louvain input
mod_leiden   <- modularity(g, membership_leiden)

# Conductance
cond_walktrap <- conductance(g, membership_walktrap)
cond_louvain  <- conductance(g_undir, membership_louvain)
cond_leiden   <- conductance(g, membership_leiden)



# ============================================
# ADD TO RESULTS
# ============================================

# Calculate new metrics
dens_walktrap <- internal_density(g, membership_walktrap)
dens_louvain <- internal_density(g, membership_louvain)
dens_leiden <- internal_density(g, membership_leiden)

sil_walktrap <- silhouette_coefficient(g, membership_walktrap)
sil_louvain <- silhouette_coefficient(g, membership_louvain)
sil_leiden <- silhouette_coefficient(g, membership_leiden)

# Enhanced results table
results <- data.frame(
  Algorithm = c("Walktrap", "Louvain", "Leiden"),
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

