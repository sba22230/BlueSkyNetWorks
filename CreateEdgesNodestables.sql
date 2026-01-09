-- 1. Drop existing graph node table if it exists
IF OBJECT_ID('dbo.Person', 'U') IS NOT NULL
BEGIN
    DROP TABLE dbo.Person;
END
GO
-- 2. Create graph node table for persons
CREATE TABLE Person (
    handle NVARCHAR(255) NOT NULL,        -- unique user handle
    display_name NVARCHAR(255) NULL,      -- optional metadata
    avatar NVARCHAR(512) NULL,            -- optional metadata
    did NVARCHAR(255) NULL,               -- Bluesky DID if available

    first_seen DATE NULL,                 -- earliest appearance in dataset
    last_seen DATE NULL,                  -- latest appearance

    posts_authored INT DEFAULT 0,         -- count of posts they created
    reposts_made INT DEFAULT 0,           -- count of reposts they performed
    reposts_received INT DEFAULT 0,       -- count of reposts their posts received

    total_likes_on_posts INT DEFAULT 0,   -- optional aggregate
    total_replies_on_posts INT DEFAULT 0, -- optional aggregate
    total_bookmarks_on_posts INT DEFAULT 0,

    PRIMARY KEY (handle)
) AS NODE;



-- 2. Drop existing edge table if it exists
IF OBJECT_ID('dbo.Reposted', 'U') IS NOT NULL
BEGIN
    DROP TABLE dbo.Reposted;
END
GO

-- 3. Create edge table for repost relationships
CREATE TABLE dbo.Reposted (
    post_uri NVARCHAR(512) NOT NULL,      -- the post being reposted
    posted_on DATE NULL,                  -- when the post was originally created

    repost_event_at DATETIME2 NULL,       -- timestamp of the repost event (if available)

    like_count INT NULL,                  -- attributes of the original post
    reply_count INT NULL,
    bookmark_count INT NULL,
    repost_count INT NULL,

    text NVARCHAR(MAX) NULL,              -- optional: post text for topic modelling

    edgeStart DATETIME2 NULL,             -- for Gephi timeline animation
    edgeEnd DATETIME2 NULL                -- optional: usually NULL unless modelling decay
) AS EDGE;
GO

-- 3. Populate Person with distinct actors (from reposts and post authors)
-- 3. Populate Person with distinct actors (from reposts and post authors)
-- Insert distinct users from the network data frame
TRUNCATE TABLE dbo.Person;
INSERT INTO dbo.Person (
    handle,
    first_seen,
    last_seen,
    posts_authored,
    reposts_made,
    reposts_received
)
SELECT
    handle,

    MIN(PostedOn) AS first_seen,
    MAX(PostedOn) AS last_seen,

    SUM(CASE WHEN role = 'author' THEN 1 ELSE 0 END) AS posts_authored,
    SUM(CASE WHEN role = 'reposter' THEN 1 ELSE 0 END) AS reposts_made,
    SUM(CASE WHEN role = 'author' THEN reposts_received ELSE 0 END) AS reposts_received
FROM (
    --------------------------------------------------------------------
    -- Canonical network data frame
    --------------------------------------------------------------------
    SELECT
        P.author_handle AS handle,
        CAST(P.indexedAt AS date) AS PostedOn,
        'author' AS role,
        1 AS reposts_received
    FROM posts_raw AS P
    INNER JOIN reposts_raw AS RP ON P.uri = RP.uri

    UNION ALL

    SELECT
        RP.handle AS handle,
        CAST(P.indexedAt AS date) AS PostedOn,
        'reposter' AS role,
        0 AS reposts_received
    FROM posts_raw AS P
    INNER JOIN reposts_raw AS RP ON P.uri = RP.uri
) AS X
WHERE handle IS NOT NULL
GROUP BY handle;

-- 4. Populate Reposted edges by resolving node ids
TRUNCATE TABLE dbo.Reposted;

WITH NetDF AS (
    SELECT  
        P.uri AS Post,
        P.author_handle AS PostedBy,
        CAST(P.indexedAt AS date) AS PostedOn,
        P.text,
        P.bookmark_count,
        P.like_count,
        P.reply_count,
        P.repost_count,
        RP.handle AS RepostedBy
    FROM posts_raw AS P
    INNER JOIN reposts_raw AS RP ON P.uri = RP.uri
)

INSERT INTO dbo.Reposted (
    $from_id,
    $to_id,
    post_uri,
    posted_on,
    like_count,
    reply_count,
    bookmark_count,
    repost_count,
    text
)
SELECT
    f.$node_id,        -- RepostedBy (source)
    t.$node_id,          -- PostedBy (target)
    r.Post,
    r.PostedOn,
    r.like_count,
    r.reply_count,
    r.bookmark_count,
    r.repost_count,
    r.text
FROM NetDF AS r
JOIN Person AS f ON f.handle = r.RepostedBy   -- reposter = FROM
JOIN Person AS t ON t.handle = r.PostedBy     -- author = TO
WHERE r.RepostedBy IS NOT NULL
  AND r.PostedBy IS NOT NULL;
