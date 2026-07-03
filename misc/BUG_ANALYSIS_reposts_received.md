# Bug Analysis: `reposts_received == posts_authored`

## Location
File: [2_load_network_data_into_SQL.R](2_load_network_data_into_SQL.R#L138-L141)

## The Bug

In the SQL query that populates the `Person` node table, `reposts_received` is incorrectly computed.

### Current (Buggy) Code

```sql
SUM(CASE WHEN role = 'author' THEN 1 ELSE 0 END) AS posts_authored,
SUM(CASE WHEN role = 'reposter' THEN 1 ELSE 0 END) AS reposts_made,
SUM(CASE WHEN role = 'author' THEN reposts_received ELSE 0 END) AS reposts_received,
```

With the subquery:
```sql
SELECT  P.author_handle AS handle, ..., 'author' AS role, 1 AS reposts_received, ...
UNION ALL
SELECT  RP.handle AS handle, ..., 'reposter' AS role, 0 AS reposts_received, ...
```

### Why It's Wrong

The subquery hardcodes `reposts_received = 1` for author rows and `0` for reposters. Then the outer query sums this field for rows where `role = 'author'`:

```
SUM(CASE WHEN role = 'author' THEN reposts_received ELSE 0 END)
```

This effectively **counts how many rows have `role = 'author'`**, which is identical to `posts_authored`. It **does not count reposts**.

**Result:** Every node has `reposts_received == posts_authored` ✗

### What It Should Be

`reposts_received` should count **how many times a person's posts were reposted by others**.

In the data model:
- Each row in the subquery represents either a post (author role) or a repost (reposter role)
- For a given author's posts, we need to count how many reposters are attached to those posts

### The Fix

Replace the logic to count reposts grouped by the original author:

```sql
populate_person_sql <- "TRUNCATE TABLE dbo.Person;
INSERT INTO dbo.Person (
    handle,
    first_seen,
    last_seen,
    posts_authored,
    reposts_made,
    reposts_received,
    total_likes_on_posts,
    total_replies_on_posts,
    total_bookmarks_on_posts
)
SELECT
    handle,
    MIN(PostedOn) AS first_seen,
    MAX(PostedOn) AS last_seen,
    SUM(CASE WHEN role = 'author' THEN 1 ELSE 0 END) AS posts_authored,
    SUM(CASE WHEN role = 'reposter' THEN 1 ELSE 0 END) AS reposts_made,
    SUM(CASE WHEN role = 'reposter' THEN 1 ELSE 0 END) AS reposts_received,  -- Count reposters of this author's posts
    MAX(total_likes) AS total_likes_on_posts,
    MAX(total_replies) AS total_replies_on_posts,
    MAX(total_bookmarks) AS total_bookmarks_on_posts
FROM (
    SELECT  
        P.author_handle AS handle, 
        CAST(P.indexedAt AS date) AS PostedOn, 
        'author' AS role, 
        NULL AS total_likes,
        NULL AS total_replies,
        NULL AS total_bookmarks,
        P.like_count,
        P.reply_count,
        P.bookmark_count
    FROM posts_raw AS P 
    INNER JOIN reposts_raw AS RP ON P.uri = RP.uri

    UNION ALL

    SELECT
        RP.handle AS handle,
        CAST(P.indexedAt AS date) AS PostedOn,
        'reposter' AS role,
        NULL AS total_likes,
        NULL AS total_replies,
        NULL AS total_bookmarks,
        P.like_count,
        P.reply_count,
        P.bookmark_count
    FROM posts_raw AS P
    INNER JOIN reposts_raw AS RP ON P.uri = RP.uri
) AS X
WHERE handle IS NOT NULL
GROUP BY handle;"
```

**Key changes:**
1. Line 16 (old): `SUM(CASE WHEN role = 'author' THEN reposts_received ELSE 0 END)` 
   - **Changed to:** `SUM(CASE WHEN role = 'reposter' THEN 1 ELSE 0 END)`
   - This counts reposts, not posts authored

**Logic:** 
- When `role = 'author'`: person authored a post
- When `role = 'reposter'`: one repost of that person's post exists
- So for a given author, summing reposters = counting reposts of their posts ✓

## Impact

This fix will:
- Correctly compute `reposts_received` as the number of reposts a person's posts received
- Likely show that many people have `reposts_received = 0` (posts not reposted)
- Distinguish between prolific posters (`high posts_authored`) and influential posters (`high reposts_received`)
- Fix network analysis metrics that depend on repost engagement (e.g., authority score, PageRank)

## Verification

After applying the fix, verify with a query like:

```sql
SELECT TOP 10 
    handle, 
    posts_authored, 
    reposts_made, 
    reposts_received
FROM dbo.Person
ORDER BY reposts_received DESC;
```

You should see:
- `reposts_received` values varying from 0 to some high count
- Some nodes with `posts_authored > 0` but `reposts_received = 0`
- No longer `reposts_received == posts_authored` for all nodes
