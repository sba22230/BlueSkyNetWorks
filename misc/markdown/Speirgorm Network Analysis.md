
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

Here are the key findings:

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

<span class="suggestion">Rewrite the size thresholds using percentiles so the labels scale with the actual data distribution.</span>


<span class="suggestion">Plot `size` on a log scale to understand the shape of the community size distribution before choosing new cut-offs.</span>


Let me analyze what it shows:

1. **Trust** (yellow-green) and **anticipation** (orange) dominate most communities, which is common in community/social networks.
2. **Community 8** has a notably higher **disgust** (brown) presence and more **fear** (purple) compared to others.
3. **Community 22** has a notably high **fear** (purple) component.
4. **Community 15** and **7** have substantial **anger** (red) at the right end.


5. **Community 3088** stands out with an unusually high **surprise** (cyan) proportion, while **sadness** (blue) and **joy** (green) appear fairly consistently across the rest of the communities.

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

Good results. The cross-tabulation shows:
- Anticipation dominates across all community types
- Engaged Micro-Group has a higher proportion of fear (3/15 = 20%) compared to other types
- Trust is more prevalent in Discussion Group and Active Circle
- Fear appears in Engaged Micro-Group and Tight Cluster but not Active Circle or Core Hub

Now let me visualize this. A heatmap tile plot would work well, or a grouped bar chart. Given the small counts, I'll use a tile/heatmap with counts labeled inside.


I'm noticing that Large Community didn't show up in the results, which is odd since community 2 has 417 members and should qualify. Let me verify whether it's classified as Core Hub or Large Community based on its average degree against the median threshold.


Let me interpret the key findings:

1. **Anticipation dominates across all community types** — it's the most common dominant emotion regardless of community structure
2. **Engaged Micro-Groups show more fear** (3 communities) than any other type — relative to their count (15 total), fear is disproportionately present
3. **Trust** appears more in Discussion Groups (5) and slightly in Active Circles and Tight Clusters, but not in Engaged Micro-Groups or Core Hub
4. **Only 5 emotions appear as dominant** out of the 8 possible (anger, anticipation, disgust, joy, surprise, trust) — disgust, joy, and surprise never dominate any community. 

The most striking pattern is that Engaged Micro-Groups are uniquely prone to fear, which makes sense if these smaller groups tend to discuss more concerning or risky topics. That said, the sample sizes are quite limited outside of Discussion Groups, so I should be cautious about over-interpreting these patterns.


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





