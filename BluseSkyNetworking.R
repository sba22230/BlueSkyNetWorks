library(bskyr)
library(furrr)
library(dplyr)
library(lubridate)
library(igraph)
library(stringr)
library(stringi)
library(tidyr)
library(purrr)
library(tibble)
library(ggraph)
library(visNetwork)
library(retry)
library(rgexf)


## Authenticate with BlueSky
bs_user <- bs_get_user()
bs_pass <- bs_get_pass()
bs_auth <- bs_auth(bs_user, bs_pass, save_auth = TRUE)

# Safe extractor helpers (avoid errors if fields are missing)
safe_chr <- function(x, ...) {
  val <- purrr::pluck(x, ..., .default = NA_character_)
  if (is.null(val)) NA_character_ else as.character(val)
}
safe_int <- function(x, ...) {
  val <- purrr::pluck(x, ..., .default = NA_integer_)
  if (is.null(val)) NA_integer_ else as.integer(val)
}

# Step 1: Define helper functions to fetch reposts and threads
get_reposts_df <- function(uri) {
  Sys.sleep(runif(1, 0.1, 2.9)) # jittered pause
  # Wrap bs_get_reposts in retry, return directly
  lmt = round(runif(1, 500, 1000))
  reposts <- retry(
    bs_get_reposts(uri, limit = lmt, auth = bs_auth, clean = TRUE),
    when = "error",
    max_tries = 3,
    interval = runif(1, 0.5, 1.5)
  )
  #If it's NULL or not a data frame, return an empty tibble with consistent
  #columns
  if (
    is.null(reposts) || !inherits(reposts, "data.frame") || nrow(reposts) == 0
  ) {
    # nolint: line_length_linter.
    return(tibble(
      original_uri = character(),
      handle = character(),
      uri = character()
    ))
  }
  # Add original_uri column
  reposts$original_uri <- uri
  reposts
}

get_thread_df <- function(uri) {
  Sys.sleep(runif(1, 0.2, 0.5)) # jittered pause
  thread <- retry(
    bs_get_post_thread(uri, auth = bs_auth, clean = FALSE),
    when = "error",
    max_tries = 3,
    interval = runif(1, 0.5, 1.5)
  )

  if (is.null(thread) || length(thread) == 0) {
    return(tibble(
      original_uri = character(),
      author = character(),
      text = character(),
      uri = character()
    ))
  }
  tibble(
    original_uri = uri,
    author = map_chr(
      thread,
      ~ pluck(.x, "post", "author", "handle", .default = NA)
    ), # nolint: line_length_linter.
    text = map_chr(
      thread,
      ~ pluck(.x, "post", "record", "text", .default = NA)
    ), # nolint: line_length_linter.
    uri = map_chr(thread, ~ pluck(.x, "post", "uri", .default = NA))
  )
}


get_posts <- function(resp) {
  if (is.list(resp) && !is.null(resp$posts)) {
    # Case: resp itself has posts
    posts <- resp$posts
  } else if (
    is.list(resp) &&
      length(resp) > 0 &&
      all(vapply(resp, function(x) is.list(x) && !is.null(x$posts), logical(1)))
  ) {
    # nolint: line_length_linter.
    # Case: resp is a list of objects, each with posts
    posts <- flatten(map(resp, "posts"))
  } else if (
    is.list(resp) &&
      length(resp) > 0 &&
      all(vapply(resp, function(x) !is.null(x$uri), logical(1)))
  ) {
    # Case: resp is already a list of posts
    posts <- resp
  } else {
    stop("Unexpected structure returned by bs_search_posts(); inspect 'resp'")
  }

  posts
}

# Step 2: Search posts containing "Speirgorm"
resp <- bs_search_posts("Speirgorm", clean = FALSE, limit = 15000)

# Flatten all posts from the response
posts <- get_posts(resp)

# Step 4: Extract key fields from posts
post_uri <- map_chr(posts, ~ safe_chr(.x, "uri"))
post_reply_parent_uri <- map_chr(
  posts,
  ~ safe_chr(.x, "record", "reply", "parent", "uri")
) # nolint: line_length_linter.

# Step 5: Build posts_df with metadata
post_author <- map_chr(posts, ~ safe_chr(.x, "author", "handle"))
post_created <- map_chr(posts, ~ safe_chr(.x, "indexedAt"))
post_text <- map_chr(posts, ~ safe_chr(.x, "record", "text"))
post_liked_cnt <- map_int(posts, ~ safe_int(.x, "likeCount"))
post_bookmrk_cnt <- map_int(posts, ~ safe_int(.x, "bookmarkCount"))
post_rply_cnt <- map_int(posts, ~ safe_int(.x, "replyCount"))
post_rpst_count <- map_int(posts, ~ safe_int(.x, "repostCount"))

posts_df <- tibble(
  uri = post_uri,
  author_handle = post_author,
  indexedAt = post_created,
  text = post_text,
  like_count = post_liked_cnt,
  bookmark_count = post_bookmrk_cnt,
  reply_count = post_rply_cnt,
  repost_count = post_rpst_count,
  reply_parent_uri = post_reply_parent_uri
)

# Debug: inspect first few posts
print(head(posts_df))
cat("Posts dataframe rows:", nrow(posts_df), "\n")

# Step 6: Build reposts and threads data frames
wrkrs = availableCores(constraints = "connections-16") * .3
plan(multisession, workers = wrkrs) # adjust workers to your CPU/network capacity

reposts_df <- posts_df |>
  filter(repost_count > 0) |>
  pull(uri) |>
  future_map_dfr(
    get_reposts_df,
    .progress = TRUE,
    .options = furrr_options(seed = 22230)
  )

# set the system to sleep
Sys.sleep(runif(1, 10, 30))
threads_df <- posts_df |>
  filter(reply_count > 0) |>
  pull(uri) |>
  future_map_dfr(
    get_thread_df,
    .progress = TRUE,
    .options = furrr_options(seed = 22230)
  )

#reposts_df <- post_uri |> map_dfr(get_reposts_df) # nolint
#threads_df <- post_uri |> map_dfr(get_thread_df) # nolint
cat("Reposts dataframe rows:", nrow(reposts_df), "\n")
cat("Threads dataframe rows:", nrow(threads_df), "\n")
plan(sequential) # reset back to normal, shuts down workers

# Debug: check reposts_df structure
print(head(reposts_df))

# Step 7: Build edge list (who reposted whom)
edges <- reposts_df |>
  transmute(from = handle, to = original_uri) |>
  distinct()
cat("Edges count:", nrow(edges), "\n")

# Debug: inspect edges
print(head(edges))

# Step 8: Build node list (unique actors and posts)
nodes <- tibble(name = unique(c(edges$from, edges$to)))
cat("Nodes count:", nrow(nodes), "\n")

# Debug: inspect nodes
print(head(nodes))

# Step 9: Build igraph object and plot basic network
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
cat("Graph summary:\n")
print(summary(g))

ggraph(g, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE)

# Step 10: Enrich edges with author info
edges <- reposts_df |>
  left_join(
    posts_df |> select(uri, author_handle),
    by = c("original_uri" = "uri")
  ) |>
  transmute(
    from = handle,
    to = author_handle,
    repost_uri = uri,
    created_at = created_at
  ) |>
  filter(!is.na(from) & !is.na(to)) |>
  distinct()
cat("Enriched edges count:", nrow(edges), "\n")

# Debug: enriched edges
print(head(edges))

# Step 11: Enrich nodes with metadata
nodes <- bind_rows(
  reposts_df |> select(name = handle, display_name, avatar, did),
  posts_df |> select(name = author_handle, text)
) |>
  distinct(name, .keep_all = TRUE)

# Add repost counts
nodes <- nodes |>
  mutate(repost_count = table(edges$to)[name] |> as.integer())
cat("Enriched nodes count:", nrow(nodes), "\n")

# Debug: enriched nodes
print(head(nodes))

# Step 12: Plot enriched network with ggraph
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
ggraph(g, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = repost_count, color = repost_count)) +
  geom_node_text(aes(label = display_name), repel = TRUE) +
  scale_size_continuous(range = c(3, 12)) +
  scale_color_gradient(low = "lightblue", high = "red") +
  theme_void()
cat("Final graph summary:\n")
print(summary(g))
write_graph(g, "bluesky enriched Speirgorm Network.graphml", format = "graphml")

# Step 13: Interactive visualization with visNetwork
vis_nodes <- nodes |>
  mutate(
    id = name,
    label = ifelse(
      is.na(display_name) | display_name == "",
      name,
      display_name
    ), # nolint: line_length_linter.
    title = paste0("Name: ", name, "\nText: ", text), # hover tooltip
    value = ifelse(is.na(repost_count), 0, repost_count), # size by repost_count # nolint: line_length_linter.
    group = name
  ) |>
  distinct(id, .keep_all = TRUE)

vis_edges <- edges |>
  transmute(
    from = from,
    to = to,
    arrows = "from" # add arrow pointing to the reposter node
  )

# Build igraph object from edges
g <- graph_from_data_frame(vis_edges, vertices = vis_nodes, directed = TRUE)

# Compute degree for each node
deg <- degree(g, mode = "all")

# Add degree to vis_nodes
vis_nodes <- vis_nodes |>
  mutate(degree = deg[id])

# Sort IDs by degree (descending)
sorted_ids <- vis_nodes |>
  arrange(desc(degree)) |>
  pull(id)

# --- Precompute layout coordinates with igraph ---
comps <- components(g)
layouts <- lapply(unique(comps$membership), function(comp_id) {
  subg <- induced_subgraph(g, which(comps$membership == comp_id))
  if (vcount(subg) > 100) {
    layout_with_lgl(subg) # large component
  } else {
    layout_with_kk(subg) # smaller components
  }
})
coords <- matrix(NA, nrow = vcount(g), ncol = 2)
offset <- 0

for (comp_id in unique(comps$membership)) {
  sub_nodes <- which(comps$membership == comp_id)
  sub_coords <- layouts[[which(unique(comps$membership) == comp_id)]]

  # Apply offset so components don’t overlap
  coords[sub_nodes, ] <- sub_coords + offset
  offset <- offset + max(sub_coords[, 1]) + 10 # shift horizontally
}
vis_nodes$x <- coords[, 1]
vis_nodes$y <- coords[, 2]

top_nodes <- vis_nodes |> arrange(desc(degree)) |> slice(1:50) |> pull(id)
vis_nodes$label <- ifelse(vis_nodes$id %in% top_nodes, vis_nodes$label, NA)

# Community detection
comm <- cluster_walktrap(g) # works on directed graphs

# Add community membership to nodes
vis_nodes$community <- comm$membership

# add isolation tag
vis_nodes$isolated <- vis_nodes$degree == 0

# --- Use precomputed layout in visNetwork ---
visNetwork(vis_nodes, vis_edges, width = "1040px", height = "800px") |>
  visOptions(
    highlightNearest = TRUE,
    nodesIdSelection = list(values = sorted_ids)
  ) |>
  visEdges(arrows = "to") |>
  visGroups(groupname = unique(vis_nodes$group)) |>
  visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) |>
  visPhysics(enabled = TRUE)

# Export the Visnetwork into GEXF format and visualise it again - check performance of Gephi
# Nodes table
ge_nodes <- data.frame(
  id = vis_nodes$id,
  label = ifelse(is.na(vis_nodes$label), vis_nodes$id, vis_nodes$label),
  x = vis_nodes$x,
  y = vis_nodes$y
)

# Node attributes (include everything else, including x/y coords, degree, community, isolated)
ge_nodesAtt <- vis_nodes %>%
  transmute(
    degree = ifelse(is.na(degree), 0L, as.integer(degree)),
    repost_count = ifelse(is.na(repost_count), 0L, as.integer(repost_count)),
    community = as.integer(community),
    isolated = ifelse(isolated, "TRUE", "FALSE")
  )

# Edges table
ge_edges <- data.frame(
  source = vis_edges$from,
  target = vis_edges$to
)

# Edge attributes (optional)
ge_edgesAtt <- vis_edges |>
  select(-from, -to) |>
  mutate(across(
    where(is.character),
    ~ stri_replace_all_regex(., "[&<>]", " ")
  )) |>
  mutate(across(where(is.character), ~ stri_trans_general(., "Latin-ASCII")))

#ge_nodesAtt <- ge_nodesAtt %>% mutate(across(where(is.character), ~ replace_na(., "")))
#ge_nodesAtt <- ge_nodesAtt %>% mutate(across(where(is.character), ~ gsub("[\r\n\t]", " ", .)))
#ge_nodesAtt <- ge_nodesAtt %>% mutate(repost_count = ifelse(is.na(repost_count), 0L, repost_count))
ge_edgesAtt <- ge_edgesAtt %>%
  mutate(across(where(is.character), ~ replace_na(., "")))

# Create a palette with enough distinct colours
palette <- grDevices::rainbow(length(unique(vis_nodes$community)))

# Map each node's community to a colour
comm_colors <- data.frame(
  r = col2rgb(palette[vis_nodes$community])[1, ],
  g = col2rgb(palette[vis_nodes$community])[2, ],
  b = col2rgb(palette[vis_nodes$community])[3, ],
  a = rep(1, nrow(vis_nodes))
)

# Create GEXF object
gexf_obj <- write.gexf(
  nodes = ge_nodes[, c("id", "label")], # only 2 columns
  edges = ge_edges,
  nodesAtt = ge_nodesAtt,
  edgesAtt = ge_edgesAtt,
  nodesVizAtt = list(
    position = data.frame(
      x = vis_nodes$x,
      y = vis_nodes$y,
      z = rep(0, nrow(vis_nodes)) # optional, Gephi expects 3D coords
    ),
    size = vis_nodes$value,
    color = comm_colors
  ),
  defaultedgetype = "directed"
)

# Save to file
print(gexf_obj, file = "visnetwork_export.gexf")
plot(gexf_obj)


cat("Interactive visualization ready with precomputed layout.\n")
top_authors_reposted <- posts_df |>
  group_by(author_handle) |>
  summarise(total_reposts = sum(repost_count, na.rm = TRUE)) |>
  arrange(desc(total_reposts)) |>
  slice(1:10)

top_reposters <- reposts_df |>
  group_by(handle, display_name) |> # group by both
  summarise(total_reposts_made = n(), .groups = "drop") |> # count reposts
  arrange(desc(total_reposts_made)) |> # sort descending
  slice(1:10) # top 10

library(DT)

datatable(top_authors_reposted, caption = "Top 10 Authors by Reposts Received")
datatable(top_reposters, caption = "Top 10 Accounts by Reposts Made")

# End of script
