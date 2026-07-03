#!/usr/bin/env Rscript
# Test script to verify isolated_count fix

source('0_functions.R')

cat('\n=== Verifying isolated_count fix ===\n')

# Simulate the old (broken) definition
old_isolated <- sum(vis_nodes$degree == 0, na.rm = TRUE)
cat('Old isolated_count (degree == 0):', old_isolated, '\n')

# Compute the new (fixed) definition
degree_p10 <- quantile(vis_nodes$degree, 0.10, na.rm = TRUE)
new_isolated <- sum(vis_nodes$degree <= degree_p10, na.rm = TRUE)
cat('New isolated_count (degree <= p10):', new_isolated, '\n')
cat('10th percentile degree threshold:', degree_p10, '\n')
cat(
  'Percentage of nodes below threshold:',
  100 * new_isolated / nrow(vis_nodes),
  '%\n'
)

# Show the distribution
cat('\n=== Degree distribution in top 5 communities ===\n')
top_comms <- unique(vis_nodes$community)[1:5]
for (comm in top_comms) {
  nodes_in_comm <- filter(vis_nodes, community == comm)
  isolated_in_comm <- sum(nodes_in_comm$degree <= degree_p10, na.rm = TRUE)
  pct <- 100 * isolated_in_comm / nrow(nodes_in_comm)
  cat(sprintf(
    'Community %d: %d/%d nodes low-degree (%.1f%%)\n',
    comm,
    isolated_in_comm,
    nrow(nodes_in_comm),
    pct
  ))
}
