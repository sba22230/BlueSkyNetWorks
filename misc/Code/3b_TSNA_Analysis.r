source("3_StatnetAnalysis.R")

# tsna to go here
library(ndtv)
library(networkDynamic)
library(tsna)
# nodes$name is your vertex key
n <- nrow(nodes)

name_to_id <- setNames(seq_len(n), nodes$name)
stopifnot(!any(is.na(name_to_id[nodes$name])))
edges2 <- edges %>%
  dplyr::mutate(
    from_id = name_to_id[from],
    to_id = name_to_id[to]
  )
stopifnot(!any(is.na(edges2$from_id)))
stopifnot(!any(is.na(edges2$to_id)))
edges2 <- edges2 %>%
  dplyr::mutate(
    edgeStarts = edgeStarts,
    edgeEnds = edgeEnds # or NA if open-ended
  )
library(network)
library(networkDynamic)

bluSkynet2 <- network.initialize(n, directed = TRUE)
bluSkynet2 %v% "name" <- nodes$name

add.edges(
  bluSkynet2,
  tail = edges2$from_id,
  head = edges2$to_id
)

network.edgecount(bluSkynet2) # should equal nrow(edges2)
vertex_spells <- data.frame(
  vertex.id = as.integer(name_to_id[nodes$name]),
  onset = as.POSIXct(nodes$earliestPost),
  terminus = as.POSIXct(nodes$latestPost),
  stringsAsFactors = FALSE
)

# strip to a plain data.frame with simple rownames
attributes(vertex_spells) <- list(
  names = names(vertex_spells),
  class = "data.frame",
  row.names = seq_len(nrow(vertex_spells))
)
edge_spells <- data.frame(
  edge.id = seq_len(nrow(edges2)),
  onset = as.POSIXct(edges2$edgeStarts),
  terminus = as.POSIXct(edges2$edgeEnds),
  stringsAsFactors = FALSE
)

attributes(edge_spells) <- list(
  names = names(edge_spells),
  class = "data.frame",
  row.names = seq_len(nrow(edge_spells))
)
dynNet <- networkDynamic(
  base.net = bluSkynet2,
  vertex.spells = vertex_spells,
  edge.spells = edge_spells
)
str(vertex_spells)
str(edge_spells)
network.size(bluSkynet2)
network.edgecount(bluSkynet2)
