# `isolated_count` Analysis: Why It's Always Zero

## Root Cause

The `isolated_count` metric is **structurally impossible** in the current data pipeline.

### Definition (Line 172, 4_visualisation.R)
```r
isolated_count = sum(isolated, na.rm = TRUE),
```

Where `isolated` is defined (Line 108):
```r
vis_nodes$isolated <- vis_nodes$degree == 0
```

### Why It's Always Zero

1. **Graph construction** (`0_functions.R:593-625`):
   - Nodes are filtered to **only include nodes that appear in edges**:
     ```r
     nodes_set <- union(sampled_edges$from, sampled_edges$to)
     sampled_nodes <- nodes_df %>% dplyr::filter(name %in% nodes_set)
     ```
   
2. **Consequence**: Every node in the final graph has **at least one edge** by definition (in or out).

3. **Result**: `degree == 0` is impossible, so `isolated_count` is always 0.

---

## What "Isolated" Actually Means

In this repost network, there are no truly isolated nodes (degree 0). Instead, "peripheral" or "weakly connected" nodes could be:

1. **Low-degree nodes** — barely mentioned, rarely repost
2. **Disconnected from main component** — in a small cluster, not connected to the giant component
3. **Periphery of community** — low position in community hierarchy

---

## Better Metrics for Peripheral Nodes

Replace the current `isolated_count` logic with:

### Option 1: Low-Degree Threshold (Simplest)
```r
community_analysis <- vis_nodes |>
  group_by(community) |>
  summarise(
    size = n(),
    # Count nodes below 10th percentile of degree
    degree_p10 = quantile(degree, 0.1),
    low_degree_count = sum(degree <= degree_p10),
    low_degree_pct = low_degree_count / size,
    
    active_members = size - low_degree_count,
    avg_degree = mean(degree, na.rm = TRUE),
    # ... rest of metrics
  )
```

Then update the classification:
```r
community_type = case_when(
  low_degree_pct >= 0.5 ~ "Peripheral",  # 50%+ low-degree nodes
  size >= 50 & avg_degree > med_degree * 1.5 ~ "Core Hub",
  # ...
)
```

### Option 2: Connectivity Components (More Sophisticated)
```r
# Find disconnected subcomponents within each community
component_analysis <- function(g_subset) {
  comps <- igraph::components(g_subset)
  n_components <- comps$no
  main_component_size <- max(comps$csize)
  peripheral_nodes <- sum(comps$csize < main_component_size)
  return(list(
    n_components = n_components,
    largest_component_pct = main_component_size / vcount(g_subset),
    peripheral_count = peripheral_nodes
  ))
}
```

### Option 3: Betweenness Centrality (Advanced)
```r
# Nodes with zero betweenness sit on no shortest paths
community_analysis <- vis_nodes |>
  group_by(community) |>
  summarise(
    size = n(),
    isolated_count = sum(betweenness == 0 | is.na(betweenness)),
    active_members = size - isolated_count,
    # ...
  )
```

---

## Recommended Fix

Use **Option 1 (Low-Degree Threshold)** because it:
- ✅ Reflects the actual network structure
- ✅ Easy to interpret and validate
- ✅ Can tune the threshold (10th percentile or other)
- ✅ Works with existing degree data already computed

### Implementation

```r
# In 4_visualisation.R, replace line 172:

community_analysis <- vis_nodes |>
  group_by(community) |>
  summarise(
    size = n(),
    
    # Use degree percentile instead of degree == 0
    degree_p10 = quantile(degree, 0.10, na.rm = TRUE),
    low_degree_count = sum(degree <= degree_p10, na.rm = TRUE),
    low_degree_pct = low_degree_count / size,
    
    # For backward compatibility, map to active_members
    # (not a true measure of isolation, but more meaningful than current)
    isolated_count = low_degree_count,
    active_members = size - low_degree_count,
    
    avg_degree = mean(degree, na.rm = TRUE),
    max_degree = max(degree, na.rm = TRUE),
    median_degree = median(degree, na.rm = TRUE),
    
    avg_repost_count = mean(value, na.rm = TRUE),
    max_repost_count = max(value, na.rm = TRUE),
    
    earliest_first_seen = min(earliestPost, na.rm = TRUE),
    latest_last_seen = max(latestPost, na.rm = TRUE),
    
    top_member = paste(
      head(name[order(desc(degree))], 3),
      collapse = ", "
    ),
    
    .groups = "drop"
  ) |>
  arrange(desc(size))
```

Then update the classification logic:
```r
med_degree <- median(community_analysis$avg_degree)
q75_repost <- quantile(community_analysis$avg_repost_count, 0.75)

community_labels <- community_analysis |>
  mutate(
    community_type = case_when(
      # Peripheral: 50%+ of nodes below 10th percentile degree
      low_degree_pct >= 0.5 ~ "Peripheral",
      
      # Large, highly connected communities (hubs)
      size >= 50 & avg_degree > med_degree * 1.5 ~ "Core Hub",
      
      # Large but not highly connected
      size >= 50 ~ "Large Community",
      
      # Medium-sized, moderately connected
      size >= 20 & avg_degree >= med_degree ~ "Active Circle",
      
      # Smaller, tightly knit groups
      size >= 10 & avg_degree > med_degree ~ "Tight Cluster",
      
      # Small but highly engaged
      size < 10 & avg_repost_count > q75_repost ~ "Engaged Micro-Group",
      
      # Catchall
      TRUE ~ "Discussion Group"
    ),
    
    label = sprintf(
      "Community %d: %s\n(%d members, %.0f%% low-degree)",
      community,
      community_type,
      size,
      low_degree_pct * 100
    )
  )
```

---

## Verification

After applying the fix:

```r
# Check the distribution
community_analysis |>
  select(community, size, isolated_count, low_degree_pct) |>
  head(10)

# Should see:
# - isolated_count > 0 for most communities
# - low_degree_pct varies (0-1.0)
# - At least some communities with low_degree_pct >= 0.5
```
