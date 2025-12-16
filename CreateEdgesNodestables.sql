---- 1. Create node table for actors (if not exists)
--CREATE TABLE Person (
--  name NVARCHAR(255) NOT NULL,
--  display_name NVARCHAR(255) NULL,
--  avatar NVARCHAR(512) NULL,
--  did NVARCHAR(255) NULL,
--  repost_count INT NULL
--) AS NODE;

---- 2. Create edge table for repost relationships
--CREATE TABLE Reposted (
--  repost_uri NVARCHAR(512) NULL,
--  created_at DATETIME2 NULL
--) AS EDGE;

-- 3. Populate Person with distinct actors (from reposts and post authors)
INSERT INTO Person (name)
SELECT DISTINCT name
FROM (
  SELECT r.handle AS name
  FROM reposts_raw r
 UNION
 SELECT p.author_handle AS name
  FROM posts_raw p
) AS u
WHERE u.name IS NOT NULL;

-- 4. Populate Reposted edges by resolving node ids
INSERT INTO Reposted ($from_id, $to_id, repost_uri, created_at)
SELECT f.$node_id, t.$node_id, r.uri, r.created_at
FROM reposts_raw r
JOIN posts_raw p ON r.original_uri = p.uri
JOIN Person f ON f.name = r.handle
JOIN Person t ON t.name = p.author_handle
WHERE r.handle IS NOT NULL AND p.author_handle IS NOT NULL;
