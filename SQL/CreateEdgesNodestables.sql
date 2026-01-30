-- 1. Drop existing graph node table if it exists
IF OBJECT_ID('dbo.Person', 'U') IS NOT NULL
BEGIN
    DROP TABLE dbo.Person;
END
GO
-- 2. Create graph node table for persons
CREATE TABLE [dbo].[Person](
	[handle] [nvarchar](255) NOT NULL,
	[first_seen] [date] NULL,
	[last_seen] [date] NULL,
	[posts_authored] [int] NULL,
	[reposts_made] [int] NULL,
	[reposts_received] [int] NULL,
	[total_likes_on_posts] [int] NULL,
	[total_replies_on_posts] [int] NULL,
	[total_bookmarks_on_posts] [int] NULL,
PRIMARY KEY CLUSTERED 
(
	[handle] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [dataonly]
)
AS NODE ON [dataonly];



-- 2. Drop existing edge table if it exists
IF OBJECT_ID('dbo.Reposted', 'U') IS NOT NULL
BEGIN
    DROP TABLE dbo.Reposted;
END
GO

-- 3. Create edge table for repost relationships
CREATE TABLE [dbo].[Reposted](
	[post_uri] [nvarchar](512) NOT NULL,
	[posted_on] [date] NULL,
	[repost_event_at] [datetime2](7) NULL,
	[like_count] [int] NULL,
	[reply_count] [int] NULL,
	[bookmark_count] [int] NULL,
	[repost_count] [int] NULL,
	[text] [nvarchar](max) NULL,
	[edgeStarts] [datetime2](7) NULL,
	[edgeEnds] [datetime2](7) NULL
)
AS EDGE ON [dataonly] TEXTIMAGE_ON [dataonly]

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
        RP.handle AS RepostedBy,
        CAST(P.indexedAt AS date) AS edgeStarts,
        CAST(P.indexedAt AS date) AS edgeEnds
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
    text,
    edgeStarts,
    edgeEnds
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
    r.text,
    r.edgeStarts,
    r.edgeEnds
FROM NetDF AS r
JOIN Person AS f ON f.handle = r.RepostedBy   -- reposter = FROM
JOIN Person AS t ON t.handle = r.PostedBy     -- author = TO
WHERE r.RepostedBy IS NOT NULL
  AND r.PostedBy IS NOT NULL;
