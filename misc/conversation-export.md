
In a network graph, **degree** refers to the number of edges (connections) a node has — essentially, how many other nodes it's directly connected to.

- **`avg_degree`** — The mean degree across all nodes. Gives a sense of the typical connectedness in the network. A high average suggests the network is dense; a low one suggests it's sparse.

- **`max_degree`** — The single highest degree in the network, i.e., the most-connected node (often called a "hub"). Useful for spotting influential accounts that are central to the conversation or information flow.

- **`median_degree`** — The middle value when all degrees are sorted. In social networks, degree distributions are typically right-skewed (a few highly-connected hubs, many loosely-connected nodes), so the median is usually much lower than the mean. If `avg_degree >> median_degree`, that's a sign of hub dominance.

Together, these three metrics characterize the shape of connectivity in the network: the median gives the typical node experience, the average shows how hubs pull the mean up, and the max identifies the single most connected node.

<span class="suggestion">Calculate the degree distribution and plot it to see how skewed the network is.</span>

<span class="suggestion">Identify the top N nodes by degree and examine who they are in the BluesSky context.</span>

<span class="suggestion">Compare avg_degree vs. median_degree across communities to see if some communities are more hub-dominated than others.</span>

1. The top node `ciaraioch.bsky.social` has 6636 in_degree and only 76 out_degree - they are heavily replied-to.
2. `ainekeenan.bsky.social` has 3044 in_degree and 0 out_degree, with very high authority score (0.879) - they post content that others repost but they don't interact back.

3. `sharrow.bsky.social` stands out with a balanced in/out ratio (1732/1567) and the highest betweenness centrality, making them a key connector in the network.

The main findings are: `ciaraioch.bsky.social` dominates as the central hub with massive in-degree (6636) but minimal out-degree (76), `ainekeenan.bsky.social` is a pure broadcaster with 3044 in-degree and zero out-degree, and `sharrow.bsky.social` functions as a true network bridge. 

A few patterns stand out immediately. Let me look at the top nodes more cleanly and flag some things I noticed:

1. **`reposts_received == posts_authored`** for all nodes - this is suspicious and seems like a data issue. I should flag this.

2. The top nodes in the Speirgorm (Irish language) BluesSky community:

**ciaraioch.bsky.social** - #1 by total degree (6712), highest in_degree (6636), low out_degree (76), highest PageRank (0.087). This is the dominant hub. Medium authority (0.154). Very low hub score. The central figure in the network.


**drmarymcauliffe.bsky.social** - Dr. Mary McAuliffe ranks second by degree (4750) with strong incoming connections (4693) but minimal outgoing activity (57). Her PageRank and authority scores are moderate, positioning her as another significant node but less influential than ciaraioch.

**robosullivan.bsky.social** - Rob O'Sullivan is third by degree (4498) with notably higher out_degree (602) compared to the others. His authority score (0.317) and hub score (0.086) are the strongest so far, plus strong betweenness centrality, making him a crucial two-way connector in the network.

**eolai.bsky.social** - Fourth by degree (3506) with strong in_degree (3247) but low authority and hub scores, suggesting primarily a receiver of attention rather than an active connector.

**sharrow.bsky.social** - Fifth by degree (3299) with remarkably balanced in/out ratios (1732/1567) and the highest betweenness centrality among the top 8 (6.7M), plus a solid hub score (0.091). This account acts as a true bridge between different parts of the network.

**ainekeenan.bsky.social** - Aine Keenan ranks sixth (3044) with an unusual profile: zero out_degree but the highest authority score (0.879), zero betweenness, and essentially no hub score. This is a pure content creator whose posts get heavily shared but who never reposts others.

**oisinmcgann.bsky.social** - Oisin McGann is seventh (3035) with good balance and the highest hub score among the top 8 (0.150), indicating strong ability to connect different communities.

**lamhfada.bsky.social** - Eighth (2966) with the highest betweenness centrality across the entire dataset (11M), making them a critical bridge between distinct network clusters.

Now I have the full picture. Here are the key findings:

---

**Data quality flag first:** `reposts_received` and `posts_authored` are identical for every node. That's almost certainly a column aliasing bug in the pipeline — worth checking how those are derived in `get_graph_data()`.

---

**Top node profiles** — five distinct archetypes emerge:

| Handle | Pattern | Why it matters |
|---|---|---|
| `ciaraioch` | In: 6636 / Out: 76, PageRank: 0.087 | Dominant hub. Receives the most reposts, rarely reposts others. The central figure. |
| `ainekeenan` | In: 3044 / Out: **0**, Authority: **0.879**, Betweenness: 0 | Pure broadcaster. Posts content widely repposted but never engages back. Disconnected from the flow graph — hence zero betweenness. |
| `lamhfada` | Betweenness: **11.1M** (highest in network) | Critical bridge. Not the most connected, but sits on the most shortest paths — a structural broker between sub-communities. |
| `sharrow` | In: 1732 / Out: 1567 (nearly equal) | Active participant. Balanced two-way engagement and second-highest betweenness (6.7M). A connector, not just a consumer. |
| `caitrionas` / `eslsys` | In: ~1–112 / Out: 954–867 | High-volume reposters. Drive reach for others rather than generating their own audience. |

The contrast between `ciaraioch` (high PageRank, dominant hub) and `ainekeenan` (high authority, zero betweenness) is particularly interesting — both are heavily reposted, but `ainekeenan` sits entirely outside the repost reciprocity loops.

I'll suggest some questions below.


<span class="suggestion">Visualize these top nodes highlighted in the network graph to see where each archetype sits spatially.</span>


<span class="suggestion">Compare the community membership of the top nodes — do the main hubs belong to the same community or different ones?</span>





<span class="suggestion">Revisit how `isolated_count` is computed upstream — it may not reflect true graph isolation.</span>


<span class="suggestion">Rewrite the size thresholds using percentiles so the labels scale with the actual data distribution.</span>


<span class="suggestion">Plot `size` on a log scale to understand the shape of the community size distribution before choosing new cut-offs.</span>





## User

vis_nodes$isolated <- vis_nodes$degree == 0

## Assistant



<div><thinking id="thinking-1780325403329-l032t">


<span class="thinking-topic">Understanding the selected code</span>



The user has selected `vis_nodes$isolated <- vis_nodes$degree == 0` on line 85 of `4_visualisation.R`. They seem to be pointing out this line in context of our earlier discussion about `isolated_count` always being 0.

Let me look at the surrounding code to understand how `isolated_count` is computed in `community_analysis`, and how `vis_nodes$isolated` relates to it.
</thinking><span data-thinking-duration-ms="1510"></span></div>

Let me look at how `isolated_count` flows from this line into `community_analysis`:

<div><tool-call id="toolu_01TQGXNjiTHdo2DUzxJp4122"><script type="application/json">{"version":1,"name":"read","input":{"file_path":"4_visualisation.R","offset":80,"limit":90},"caption":"4_visualisation.R:80-169","captionLink":"4_visualisation.R:80"}</script></tool-call></div>

<div><tool-result call-id="toolu_01TQGXNjiTHdo2DUzxJp4122"><script type="application/json">{"version":1,"status":"success","result":{"type":"text","value":"[File: 20.8KB, 716 lines, showing lines 80-169 (truncated)]\n    80→# Add community members...[truncated, total 3457 chars]"},"resultPreview":{"type":"code","value":"[File: 20.8KB, 716 lines, showing lines 80-169 (truncated)]\n# Add community membership to nodes\n# vis_nodes$community <- comm$membership\n\n# ensure community is used as the visNetwork group and add colours\nvis_nodes$group <- as.character(vis_nodes$community)\nvis_nodes$isolated <- vis_nodes$degree == 0\n\nrxWriteObject(\n  ds_Graphs,\n  \"vis_g_Graph - igraph - community detection\",\n  vis_g,\n  overwrite = TRUE\n)\n\nedge_comm_df <- edges |>\nleft_join(nodes |> select(name, community), by = c(\"from\" = \"name\")) |>\nrename(comm_from = community) |>\nleft_join(nodes |> select(name, community), by = c(\"to\" = \"name\")) |>\nrename(comm_to = community)\n\ncomm_matrix <- edge_comm_df |>\ncount(comm_from, comm_to) |>\npivot_wider(names_from = comm_to, values_from = n, values_fill = 0)\ncomm_matrix\n\nlibrary(lattice)\n# Focus on top 20 communities by total outgoing interactions\ntop_comms <- comm_matrix |>\nmutate(total = rowSums(across(-comm_from))) |>\nslice_max(total, n = 20) |>\npull(comm_from) |>\nas.character()\n# Subset rows and matching columns, then convert to matrix\nmat <- comm_matrix |>\nfilter(comm_from %in% top_comms) |>\nselect(comm_from, any_of(top_comms)) |>\ntibble::column_to_rownames(\"comm_from\") |>\nas.matrix()\n# Log-transform (add 1 to handle zeros)\nmat_log <- log1p(mat)\nlevelplot(\nmat_log,\nxlab = \"Community (to)\",\nylab = \"Community (from)\",\nmain = \"Cross-community interactions (log scale)\",\ncol.regions = viridis::viridis(100),\nscales = list(x = list(rot = 45))\n) # this is a good view of community interaction\n\n# ========================================================================\n# COMMUNITY CHARACTERIZATION & LABELING\n# ========================================================================\n# Analyze what defines each community to create meaningful labels\n# TBD needs to use the communities already computed\n\ncat(\"\\n=== ANALYZING COMMUNITIES ===\\n\")\n\n# Get community member data\ncommunity_analysis <- vis_nodes %>%\n  group_by(community) %>%\n  summarise(\n    # Size and structure\n    size = n(),\n    isolated_count = sum(isolated, na.rm = TRUE),\n    active_members = sum(!isolated, na.rm = TRUE),\n\n    # Connectivity metrics - degree measures the number of \n    avg_degree = mean(degree, na.rm = TRUE),\n    max_degree = max(degree, na.rm = TRUE),\n    median_degree = median(degree, na.rm = TRUE),\n\n    # Engagement metrics (from nodes attributes)\n    avg_repost_count = mean(value, na.rm = TRUE),\n    max_repost_count = max(value, na.rm = TRUE),\n\n    # Temporal reach (if available)\n    earliest_first_seen = min(earliestPost, na.rm = TRUE),\n    latest_last_seen = max(latestPost, na.rm = TRUE),\n\n    # Top influencers in this community\n    top_member = paste(\n      head(name[order(desc(degree))], 3),\n      collapse = \", \"\n    ),\n\n    .groups = \"drop\"\n  ) %>%\n  arrange(desc(size))\n\ncat(\"Community Statistics:\\n\")"},"elapsedMs":189}</script></tool-result></div>



<div><thinking id="thinking-1780325410365-1aqpq">


<span class="thinking-topic">Identifying the root cause of always-zero isolated_count</span>



Now I can see the issue clearly:

Line 85: `vis_nodes$isolated <- vis_nodes$degree == 0`

Then in `community_analysis`, line 143: `isolated_count = sum(isolated, na.rm = TRUE)`

The `isolated` flag is based on `vis_nodes$degree == 0`. Let me check what `vis_nodes$degree` actually contains. Since we saw `isolated_count` is always 0 across all communities, it means `vis_nodes$degree` is never 0 for any node.
 
The problem is becoming clearer — `vis_nodes$degree` represents total degree, not just out_degree or in_degree. Since every node in the graph has at least one connection by construction (they're included because they appear in reposts), they'll all have degree >= 1. The nodes that would truly be isolated — those with degree == 0 — would only exist if `vis_nodes` contained nodes with no edges at all, but that's not how the graph is being built. the repost network, every node present has at least one connection. The `isolated_count` metric is therefore always 0 by construction, making the Peripheral classification impossible.

So the "Peripheral" classification can never actually trigger because `vis_nodes` is built directly from the edge list, which inherently excludes nodes with zero degree. I have a few options to fix this: I could redefine "isolated" to mean something more meaningful for this data like nodes with only a single connection or below-average degree, I could remove the Peripheral category entirely since it's unreachable, or I could add isolated nodes from the original dataset that never participated in any reposts. I should walk the user through this issue and what it means for their classification system.
</thinking><span data-thinking-duration-ms="15956"></span></div>

Found the root cause. `isolated_count` is always 0 because `vis_nodes` is built from the graph's edge list — by definition, every node present has at least one connection, so `degree == 0` is structurally impossible. The Peripheral condition can never fire.

There are two ways to interpret this:

**Option A — The condition is wrong.** "Peripheral" was meant to capture low-engagement nodes, but `degree == 0` is too strict. A more useful definition would be nodes with degree below some low threshold, e.g.:

```r
# In the vis_nodes setup, replace:
vis_nodes$isolated <- vis_nodes$degree == 0

# With something like:
degree_p10 <- quantile(vis_nodes$degree, 0.10)
vis_nodes$isolated <- vis_nodes$degree <= degree_p10
```

**Option B — Genuinely isolated nodes are being dropped.** If there are users in your underlying data who never made or received a repost, they won't appear in the graph at all. If you want to count them, you'd need to join `vis_nodes` back against the full user table before the `community_analysis` summarise.

Given what we saw earlier (median community size of 1, highly right-skewed), Option A is likely more useful — nodes *in* the graph but weakly connected are the real "peripheral" population.

Which interpretation fits your intent for the Peripheral category?




## User

I would like similar code that could use NRC lexicon 
community_sentiment <- tidy_posts |>
  inner_join(get_sentiments("bing"), by = "word") |>
  count(community, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(net_sentiment = positive - negative)

## Assistant



<div><thinking id="thinking-1780328802363-59xrp">


<span class="thinking-topic">Understanding NRC lexicon structure</span>



The NRC lexicon has a different structure than Bing. Bing has `word` and `sentiment` (positive/negative). NRC has `word`, `sentiment` (with values like anger, fear, anticipation, trust, surprise, sadness, joy, disgust, positive, negative), so each word can appear multiple times (once per emotion).

The user wants code analogous to the Bing community sentiment code but using NRC. I can see `nrc` is already loaded in the session. Let me write this.


Now I'm planning the approach: join the text data with the NRC lexicon, aggregate by community and emotion type, then reshape it so each of the ten emotions becomes its own column. I'll also calculate net sentiment using the positive and negative categories, similar to what was done with Bing.

Writing the NRC analysis code...
</thinking><span data-thinking-duration-ms="7865"></span></div>

The NRC lexicon has 10 categories (8 emotions + positive/negative) versus Bing's binary, so the structure is the same but you get richer columns. Since `nrc` is already in your session, use it directly:

```r
community_sentiment_nrc <- tidy_posts |>
  inner_join(nrc, by = "word") |>
  count(community, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  mutate(
    net_sentiment  = positive - negative,
    # Dominant emotion: highest count among the 8 pure emotions (exclude pos/neg)
    dominant_emotion = pmap_chr(
      pick(anger, anticipation, disgust, fear, joy, sadness, surprise, trust),
      \(...) names(which.max(c(...)))
    )
  )
```

A few notes on the differences from the Bing version:

- **No pivot needed for sentiment polarity** — NRC already has separate `positive` and `negative` rows, so `net_sentiment = positive - negative` still works the same way after the pivot.
- **`dominant_emotion`** excludes `positive` and `negative` from the `which.max()` since those overlap with the emotion columns — they're aggregate labels, not distinct feelings.
- **Word double-counting** — NRC assigns multiple emotions to the same word (e.g. "joy" and "positive"), so total word counts will exceed Bing's. Keep this in mind when comparing magnitude across lexicons; ratios within NRC are fine but raw counts aren't directly comparable to Bing.

If you want to compare the two side by side:

```r
community_sentiment |>
  select(community, net_sentiment) |>
  rename(net_bing = net_sentiment) |>
  left_join(
    community_sentiment_nrc |> select(community, net_sentiment, dominant_emotion),
    by = "community"
  ) |>
  rename(net_nrc = net_sentiment)
```

I'll suggest some questions below.


<span class="suggestion">Visualize the emotion profile per community as a stacked bar chart (community on y, emotion columns on x, faceted or colored by emotion).</span>


<span class="suggestion">Check whether `dominant_emotion` varies meaningfully across communities or is driven mostly by community size.</span>


<span class="suggestion">Cross-tabulate `dominant_emotion` against `community_type` from `community_labels` to see if community structure correlates with emotional tone.</span>





## User

Visualize the emotion profile per community as a stacked bar chart (community on y, emotion columns on x, faceted or colored by emotion).

## Assistant



<div><thinking id="thinking-1780328862706-ch80b">


<span class="thinking-topic">Understanding the data structure for NRC visualization</span>



The user wants to visualize the emotion profile per community as a stacked bar chart. I need to:
1. First run the NRC sentiment code to create `community_sentiment_nrc`
2. Then create the visualization

The `nrc` lexicon is already in the session. `tidy_posts` is also available.

Let me think about the chart:
- Community on y-axis (since there are many communities - 160)
- Emotion counts on x-axis
- Colored by emotion
- Stacked bar chart

Since there are 160 communities, I should probably filter to top N communities by size or by total word count. 

With that many communities, the chart would become too crowded to read, so I'll need to focus on the top communities. I'm planning to compute NRC sentiment scores per community, extract just the eight core emotions (excluding the aggregate positive/negative categories), filter down to the top 20 communities by word count, reshape the data into long format, and then build a horizontal stacked bar chart using ggplot2 with identity positioning.

Writing the code...
</thinking><span data-thinking-duration-ms="10300"></span></div>



<div><tool-call id="toolu_013q9qkrL6QffJ3iyT6eC4HN"><script type="application/json">{"version":1,"name":"executeCode","input":{"code":"library(dplyr)\n\n# Check how many communities have NRC matches and their relative size\ntidy...[truncated, total 233 chars]","intent":"Check community word counts to inform how many to show in the chart"},"intent":"Check community word counts to inform how many to show in the chart","inputPreview":{"type":"code","value":"library(dplyr)\n\n# Check how many communities have NRC matches and their relative size\ntidy_posts |>\n  inner_join(nrc, by = \"word\") |>\n  filter(!sentiment %in% c(\"positive\", \"negative\")) |>\n  count(community, sort = TRUE) |>\n  head(5)","language":"r"}}</script></tool-call></div>

<div><tool-result call-id="toolu_013q9qkrL6QffJ3iyT6eC4HN"><script type="application/json">{"version":1,"status":"success","result":{"type":"content","value":[{"type":"text","text":"Output:\n# A tibble: 5 × 2\n  community     n\n      <int> <int>\n1         2   418\n2         ...[truncated, total 175 chars]"}]},"resultPreview":{"type":"code","value":"# A tibble: 5 × 2\n  community     n\n      <int> <int>\n1         2   418\n2         9    61\n3         8    55\n4        22    42\n5       139    38"},"elapsedMs":476}</script></tool-result></div>



<div><thinking id="thinking-1780328880239-u0v1n">


<span class="thinking-topic">Planning the visualization approach</span>



Community 2 dominates with 418 words, then there's a big drop to community 9 with 61. There are probably many small communities. I'll filter to top 15 or so by total NRC word count and create the stacked bar chart.

Let me now compute the NRC sentiment and create the visualization. I'll use proportional fill to handle the size imbalance (community 2 is much bigger than others).
</thinking><span data-thinking-duration-ms="2046"></span></div>

Community 2 is dominant — I'll filter to the top 15 by total NRC word count and use proportional fill so community 2 doesn't visually swamp everything.

<div><tool-call id="toolu_01M4Tdc8aPcXxJVTrXNeRSXc"><script type="application/json">{"version":1,"name":"executeCode","input":{"code":"library(dplyr)\nlibrary(tidyr)\nlibrary(ggplot2)\n\n# 8 pure emotions only — exclude aggregate...[truncated, total 1732 chars]","intent":"Create stacked proportional bar chart of NRC emotion profiles by community"},"intent":"Create stacked proportional bar chart of NRC emotion profiles by community","inputPreview":{"type":"code","value":"library(dplyr)\nlibrary(tidyr)\nlibrary(ggplot2)\n\n# 8 pure emotions only — exclude aggregate positive/negative\nemotions <- c(\"anger\", \"anticipation\", \"disgust\", \"fear\",\n              \"joy\", \"sadness\", \"surprise\", \"trust\")\n\nemotion_palette <- c(\n  anger        = \"#d62728\",\n  anticipation = \"#ff7f0e\",\n  disgust      = \"#8c564b\",\n  fear         = \"#9467bd\",\n  joy          = \"#2ca02c\",\n  sadness      = \"#1f77b4\",\n  surprise     = \"#17becf\",\n  trust        = \"#bcbd22\"\n)\n\n# Build NRC community emotion profile\ncommunity_sentiment_nrc <- tidy_posts |>\n  inner_join(nrc, by = \"word\") |>\n  filter(sentiment %in% emotions) |>\n  count(community, sentiment)\n\n# Top 15 communities by total matched word count\ntop_communities <- community_sentiment_nrc |>\n  count(community, wt = n, sort = TRUE) |>\n  slice_head(n = 15) |>\n  pull(community)\n\n# Order communities by total size for the y-axis\ncommunity_order <- community_sentiment_nrc |>\n  filter(community %in% top_communities) |>\n  count(community, wt = n) |>\n  arrange(n) |>\n  pull(community)\n\ncommunity_sentiment_nrc |>\n  filter(community %in% top_communities) |>\n  mutate(\n    community = factor(community, levels = community_order),\n    sentiment = factor(sentiment, levels = emotions)\n  ) |>\n  ggplot(aes(x = n, y = community, fill = sentiment)) +\n  geom_bar(stat = \"identity\", position = \"fill\") +\n  scale_fill_manual(values = emotion_palette) +\n  scale_x_continuous(labels = scales::percent_format()) +\n  labs(\n    title = \"Emotion profile by community (NRC lexicon)\",\n    subtitle = \"Top 15 communities by matched word count — proportional share\",\n    x = \"Share of emotion words\",\n    y = \"Community\",\n    fill = \"Emotion\"\n  ) +\n  theme_minimal() +\n  theme(legend.position = \"right\")","language":"r"}}</script></tool-call></div>

<tool-image call-id="toolu_01M4Tdc8aPcXxJVTrXNeRSXc" src="00d9f4fecedac161e468e10114f7bdc3d31cc224.png" width="672" height="672"></tool-image>



<div><tool-result call-id="toolu_01M4Tdc8aPcXxJVTrXNeRSXc"><script type="application/json">{"version":1,"status":"success","result":{"type":"content","value":[{"type":"text","text":"Execution time: 4929ms"},{"type":"image-data","data":"iVBORw0KGgoAAAANSUhEUgAAAqAAAAKgCAYAAABEPM/FAABvBUlEQVR4AezBMWgd+YLn+28dFEwy2RlYxu/CbfiX4A...[truncated, total 37972 chars]","mediaType":"image/png"}]},"resultPreview":{"type":"code","value":""},"elapsedMs":6055}</script></tool-result></div>



<div><thinking id="thinking-1780328905941-09txh">


<span class="thinking-topic">Interpreting and summarizing the visualization</span>



The chart looks good! Let me analyze what it shows:

1. **Trust** (yellow-green) and **anticipation** (orange) dominate most communities, which is common in community/social networks.
2. **Community 8** has a notably higher **disgust** (brown) presence and more **fear** (purple) compared to others.
3. **Community 22** has a notably high **fear** (purple) component.
4. **Community 15** and **7** have substantial **anger** (red) at the right end.


5. **Community 3088** stands out with an unusually high **surprise** (cyan) proportion, while **sadness** (blue) and **joy** (green) appear fairly consistently across the rest of the communities.
</thinking><span data-thinking-duration-ms="4963"></span></div>

A few things stand out from the chart:

- **Trust and anticipation dominate across all communities** — a pattern typical of social networks where people share content they're invested in.
- **Community 8** has a noticeably higher disgust share than others, and elevated fear.
- **Community 22** has the largest fear proportion of the top communities.
- **Community 3088** is unusual — it shows a very high surprise share relative to the others.
- **Communities 15 and 7** carry more anger (red, right edge) than the rest.

These differences are worth treating cautiously given the small word counts for all communities except #2 — a single distinctive post or topic can shift the profile of a small community considerably.

I'll suggest some questions below.


<span class="suggestion">Join `community_labels$top_member` onto this chart to show the dominant handle next to each community ID — it will make the profiles interpretable.</span>


<span class="suggestion">Add a raw-count version of the same chart alongside the proportional one, so small communities with unusual profiles are appropriately scaled.</span>


<span class="suggestion">Investigate community 3088 and community 8 specifically — pull their top words and posts to understand what's driving the unusual emotion signatures.</span>





## User

Cross-tabulate dominant_emotion against community_type from community_labels to see if community structure correlates with emotional tone.

## Assistant



<div><thinking id="thinking-1780329060439-patxm">


<span class="thinking-topic">Checking available session variables</span>



I can see `community_sentiment_nrc`, `community_labels`, and `community_analysis` are all in the session. I need to:

1. Compute `dominant_emotion` per community from `community_sentiment_nrc`
2. Join with `community_labels` to get `community_type`
3. Cross-tabulate and visualize

The `community_sentiment_nrc` is already in the session but it may not have `dominant_emotion` yet (we created the chart inline without saving that column). Let me compute dominant_emotion and then cross-tabulate.


Now I'm deciding between a few visualization approaches—a heatmap tile plot would work well given the relatively small number of communities, or I could render a simple count table. Let me work through the computation step by step.
</thinking><span data-thinking-duration-ms="9041"></span></div>



<div><tool-call id="toolu_01DKZXwd4NNshMXFoE1Wijg6"><script type="application/json">{"version":1,"name":"executeCode","input":{"code":"library(dplyr)\nlibrary(tidyr)\nlibrary(ggplot2)\n\nemotions <- c(\"anger\", \"anticipation\", \"di...[truncated, total 859 chars]","intent":"Compute dominant emotion per community and cross-tabulate against community_type"},"intent":"Compute dominant emotion per community and cross-tabulate against community_type","inputPreview":{"type":"code","value":"library(dplyr)\nlibrary(tidyr)\nlibrary(ggplot2)\n\nemotions <- c(\"anger\", \"anticipation\", \"disgust\", \"fear\",\n              \"joy\", \"sadness\", \"surprise\", \"trust\")\n\n# Compute dominant emotion per community\ndominant_by_community <- tidy_posts |>\n  inner_join(nrc, by = \"word\") |>\n  filter(sentiment %in% emotions) |>\n  count(community, sentiment) |>\n  slice_max(n, n = 1, by = community, with_ties = FALSE) |>\n  rename(dominant_emotion = sentiment, emotion_count = n)\n\n# Join with community_labels for community_type\ncross_tab <- dominant_by_community |>\n  inner_join(community_labels |> select(community, community_type, size),\n             by = \"community\")\n\n# Count of communities per combination\ncross_tab |>\n  count(community_type, dominant_emotion) |>\n  pivot_wider(names_from = dominant_emotion, values_from = n, values_fill = 0) |>\n  arrange(community_type)","language":"r"}}</script></tool-call></div>

<div><tool-result call-id="toolu_01DKZXwd4NNshMXFoE1Wijg6"><script type="application/json">{"version":1,"status":"success","result":{"type":"content","value":[{"type":"text","text":"Output:\n# A tibble: 5 × 6\n  community_type      anticipation trust anger  fear sadness\n  <...[truncated, total 476 chars]"}]},"resultPreview":{"type":"code","value":"# A tibble: 5 × 6\n  community_type      anticipation trust anger  fear sadness\n  <chr>                      <int> <int> <int> <int>   <int>\n1 Active Circle                  2     1     0     0       0\n2 Core Hub                       1     0     0     0       0\n3 Discussion Group              22     5     2     1       1\n4 Engaged Micro-Group           10     0     1     3       1\n5 Tight Cluster                  2     1     0     1       0"},"elapsedMs":905}</script></tool-result></div>



<div><thinking id="thinking-1780329076909-gtrrz">


<span class="thinking-topic">Assessing the cross-tabulation results</span>



Good results. The cross-tabulation shows:
- Anticipation dominates across all community types
- Engaged Micro-Group has a higher proportion of fear (3/15 = 20%) compared to other types
- Trust is more prevalent in Discussion Group and Active Circle
- Fear appears in Engaged Micro-Group and Tight Cluster but not Active Circle or Core Hub

Now let me visualize this. A heatmap tile plot would work well, or a grouped bar chart. Given the small counts, I'll use a tile/heatmap with counts labeled inside.


I'm noticing that Large Community didn't show up in the results, which is odd since community 2 has 417 members and should qualify. Let me verify whether it's classified as Core Hub or Large Community based on its average degree against the median threshold.
</thinking><span data-thinking-duration-ms="8622"></span></div>

Good counts. Now visualize it as a tile heatmap — clearer than a bar chart given the sparse cell structure:

<div><tool-call id="toolu_01KXZ9YJoFDNhA64srdSnjx5"><script type="application/json">{"version":1,"name":"executeCode","input":{"code":"cross_tab |>\n  count(community_type, dominant_emotion) |>\n  mutate(\n    community_type = f...[truncated, total 936 chars]","intent":"Heatmap tile chart of community type vs dominant emotion"},"intent":"Heatmap tile chart of community type vs dominant emotion","inputPreview":{"type":"code","value":"cross_tab |>\n  count(community_type, dominant_emotion) |>\n  mutate(\n    community_type = factor(community_type,\n      levels = c(\"Core Hub\", \"Active Circle\", \"Tight Cluster\",\n                 \"Engaged Micro-Group\", \"Discussion Group\")),\n    dominant_emotion = factor(dominant_emotion, levels = emotions)\n  ) |>\n  ggplot(aes(x = dominant_emotion, y = community_type, fill = n)) +\n  geom_tile(colour = \"white\", linewidth = 0.8) +\n  geom_text(aes(label = n), size = 4.5, fontface = \"bold\") +\n  scale_fill_distiller(palette = \"YlOrRd\", direction = 1) +\n  labs(\n    title = \"Community structure vs. dominant emotion\",\n    subtitle = \"Cell = number of communities; dominant emotion by highest NRC word count\",\n    x = \"Dominant emotion\",\n    y = \"Community type\",\n    fill = \"Count\"\n  ) +\n  theme_minimal() +\n  theme(\n    axis.text.x  = element_text(angle = 30, hjust = 1),\n    panel.grid   = element_blank(),\n    legend.position = \"none\"\n  )","language":"r"}}</script></tool-call></div>

<tool-image call-id="toolu_01KXZ9YJoFDNhA64srdSnjx5" src="cbc486279e9df60c91c28cfdc27fdba4d0ddf29e.png" width="672" height="672"></tool-image>



<div><tool-result call-id="toolu_01KXZ9YJoFDNhA64srdSnjx5"><script type="application/json">{"version":1,"status":"success","result":{"type":"content","value":[{"type":"text","text":"Execution time: 4650ms"},{"type":"image-data","data":"iVBORw0KGgoAAAANSUhEUgAAAqAAAAKgCAYAAABEPM/FAABtoklEQVR4AezBb2gda37g+W+dOBAIA5G0ZOh2GtLJ82...[truncated, total 37500 chars]","mediaType":"image/png"}]},"resultPreview":{"type":"code","value":""},"elapsedMs":5947}</script></tool-result></div>



<div><thinking id="thinking-1780329102197-ifj3y">


<span class="thinking-topic">Interpreting the cross-tabulation results</span>



The chart looks great. Let me interpret the key findings:

1. **Anticipation dominates across all community types** — it's the most common dominant emotion regardless of community structure
2. **Engaged Micro-Groups show more fear** (3 communities) than any other type — relative to their count (15 total), fear is disproportionately present
3. **Trust** appears more in Discussion Groups (5) and slightly in Active Circles and Tight Clusters, but not in Engaged Micro-Groups or Core Hub
4. **Only 5 emotions appear as dominant** out of the 8 possible (anger, anticipation, disgust, joy, surprise, trust) — disgust, joy, and surprise never dominate any community. 

The most striking pattern is that Engaged Micro-Groups are uniquely prone to fear, which makes sense if these smaller groups tend to discuss more concerning or risky topics. That said, the sample sizes are quite limited outside of Discussion Groups, so I should be cautious about over-interpreting these patterns.
</thinking><span data-thinking-duration-ms="7547"></span></div>

The chart is readable but the count cells are small enough to warrant caution. A few observations:

- **Anticipation is the universal dominant emotion** — it appears across every community type, suggesting it reflects baseline discourse in the Speirgorm network rather than anything type-specific.
- **Engaged Micro-Groups show a fear signature** — 3 of 15 communities in that type have fear as their dominant emotion, which is proportionally higher than any other type. Small passionate groups discussing contentious topics would fit this pattern.
- **Trust skews toward larger, less engaged groups** — it appears in Discussion Groups (5) and Active Circles (1), but not at all in Engaged Micro-Groups.
- **Only 5 of 8 emotions ever dominate.** Disgust, joy, and surprise never top any community — consistent with the stacked bar chart earlier where those emotions had consistently smaller shares.

One caveat worth keeping in mind: most of these cells have counts of 1–3, so the patterns are suggestive at best. The Discussion Group row is the only one with enough communities to draw reasonable conclusions from.

I'll suggest some questions below.


<span class="suggestion">Normalise the heatmap by row (proportion within each community type) to make the fear pattern in Engaged Micro-Groups more visually apparent despite the unequal row sizes.</span>


<span class="suggestion">Check which specific communities are the Engaged Micro-Group / fear communities — pull their `top_member` and post samples to validate the interpretation.</span>


<span class="suggestion">Re-run dominant emotion using a proportional measure (emotion share rather than raw count) to reduce the influence of community word volume on the result.</span>





