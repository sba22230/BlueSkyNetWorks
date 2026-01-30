-- ============================================================================
-- BlueSkyNetWorks: SQL Server Graph Table Metrics Computation
-- ============================================================================
-- This script computes network metrics using T-SQL graph table queries
-- and stores results back in the Person table for retrieval by R
-- ============================================================================

-- ============================================================================
-- SECTION 1: DEGREE METRICS (In-Degree, Out-Degree, Total Degree)
-- ============================================================================

-- Degree metrics: count incoming and outgoing edges
-- In-Degree: how many times a post was reposted (reposts_received)
-- Out-Degree: how many times a user reposted (reposts_made)
/*
UPDATE dbo.Person
SET 
    reposts_received = COALESCE((
        SELECT COUNT(*) 
        FROM dbo.Reposted
        WHERE $to_id = dbo.Person.$node_id
    ), 0),
    reposts_made = COALESCE((
        SELECT COUNT(*)
        FROM dbo.Reposted
        WHERE $from_id = dbo.Person.$node_id
    ), 0);
*/
-- ============================================================================
-- SECTION 2: CLOSENESS CENTRALITY
-- ============================================================================
-- Closeness: average shortest path distance from a node to all others
-- Inverse of mean distance; nodes with shorter paths to others have higher closeness
-- For directed graphs, we compute reachability-based closeness

CREATE OR ALTER PROCEDURE sp_ComputeCloseness
AS
BEGIN
    -- Create temp table to store closeness values
    CREATE TABLE #closeness_temp (
        handle NVARCHAR(255) NOT NULL PRIMARY KEY,
        avg_distance FLOAT,
        closeness FLOAT
    );

    -- For each person, compute average shortest path to all reachable others
    DECLARE @person_handle NVARCHAR(255);
    DECLARE @person_id UNIQUEIDENTIFIER;
    DECLARE person_cursor CURSOR FOR 
        SELECT handle, $node_id FROM dbo.Person;

    OPEN person_cursor;
    FETCH NEXT FROM person_cursor INTO @person_handle, @person_id;

    WHILE @@FETCH_STATUS = 0
    BEGIN
        DECLARE @avg_dist FLOAT;
        DECLARE @reachable INT;

        -- Use SHORTEST_PATH to find distances
        WITH reachable_nodes AS (
            SELECT DISTINCT p2.handle, 1 AS distance
            FROM dbo.Person p1
            JOIN dbo.Reposted r ON p1.$node_id = r.$from_id
            JOIN dbo.Person p2 ON r.$to_id = p2.$node_id
            WHERE p1.$node_id = @person_id
        )
        SELECT 
            @avg_dist = COALESCE(AVG(CAST(distance AS FLOAT)), 0),
            @reachable = COUNT(*)
        FROM reachable_nodes;

        -- Closeness = 1 / average_distance (or proportion of reachable nodes)
        INSERT INTO #closeness_temp
        VALUES (@person_handle, @avg_dist, 
                CASE WHEN @avg_dist > 0 THEN 1.0 / @avg_dist ELSE 0 END);

        FETCH NEXT FROM person_cursor INTO @person_handle, @person_id;
    END;

    CLOSE person_cursor;
    DEALLOCATE person_cursor;

    -- Update Person table with closeness values
    UPDATE p
    SET closeness = c.closeness
    FROM dbo.Person p
    JOIN #closeness_temp c ON p.handle = c.handle;

    DROP TABLE #closeness_temp;
END;
GO

-- ============================================================================
-- SECTION 3: LOCAL CLUSTERING COEFFICIENT
-- ============================================================================
-- Clustering: for each node, what fraction of its neighbors' neighbors are also neighbors?
-- Measures local density of triangles (triadic closure)

CREATE OR ALTER PROCEDURE sp_ComputeClusteringCoefficient
AS
BEGIN
    CREATE TABLE #clustering_temp (
        handle NVARCHAR(255) NOT NULL PRIMARY KEY,
        local_clustering FLOAT
    );

    -- For each person, count triangles involving that person
    -- Triangle: A -> B -> C -> A (or any direction)
    DECLARE @person_handle NVARCHAR(255);
    DECLARE @person_id UNIQUEIDENTIFIER;
    DECLARE @out_neighbors INT;
    DECLARE @possible_edges INT;
    DECLARE @actual_triangles INT;
    DECLARE person_cursor CURSOR FOR 
        SELECT handle, $node_id FROM dbo.Person;

    OPEN person_cursor;
    FETCH NEXT FROM person_cursor INTO @person_handle, @person_id;

    WHILE @@FETCH_STATUS = 0
    BEGIN
        -- Get neighbors of this person (both in and out)
        DECLARE @neighbors TABLE (node_id UNIQUEIDENTIFIER);
        INSERT INTO @neighbors
        SELECT DISTINCT $from_id FROM dbo.Reposted WHERE $to_id = @person_id
        UNION
        SELECT DISTINCT $to_id FROM dbo.Reposted WHERE $from_id = @person_id;

        SELECT @out_neighbors = COUNT(*) FROM @neighbors;

        -- Maximum possible edges between neighbors
        SET @possible_edges = @out_neighbors * (@out_neighbors - 1) / 2;

        -- Count actual edges between neighbors
        SELECT @actual_triangles = COUNT(*)
        FROM @neighbors n1
        JOIN dbo.Reposted r ON n1.node_id = r.$from_id
        JOIN @neighbors n2 ON r.$to_id = n2.node_id
        WHERE n1.node_id < n2.node_id;

        -- Clustering coefficient
        INSERT INTO #clustering_temp
        VALUES (@person_handle, 
                CASE WHEN @possible_edges > 0 
                     THEN CAST(@actual_triangles AS FLOAT) / @possible_edges 
                     ELSE 0 END);

        FETCH NEXT FROM person_cursor INTO @person_handle, @person_id;
    END;

    CLOSE person_cursor;
    DEALLOCATE person_cursor;

    -- Update Person table
    UPDATE p
    SET local_clustering = c.local_clustering
    FROM dbo.Person p
    JOIN #clustering_temp c ON p.handle = c.handle;

    DROP TABLE #clustering_temp;
END;
GO

-- ============================================================================
-- SECTION 4: SIMPLE BETWEENNESS CENTRALITY (Via Shortest Paths)
-- ============================================================================
-- Betweenness: count how many shortest paths pass through a node
-- Full betweenness requires all-pairs shortest paths; here we show edge betweenness
-- (For true node betweenness, consider computing this in R; SQL can be slow for large graphs)

CREATE OR ALTER PROCEDURE sp_ComputeEdgeBetweenness
AS
BEGIN
    CREATE TABLE #edge_betweenness (
        repost_id BIGINT NOT NULL PRIMARY KEY,
        betweenness INT
    );

    -- For each edge, count shortest paths that use it
    -- Simplified: count 2-hop paths through each edge
    INSERT INTO #edge_betweenness
    SELECT 
        r.$edge_id,
        COUNT(DISTINCT CONCAT(p1.handle, p3.handle)) AS betweenness
    FROM dbo.Reposted r
    JOIN dbo.Person p1 ON r.$from_id = p1.$node_id
    JOIN dbo.Person p2 ON r.$to_id = p2.$node_id
    JOIN dbo.Reposted r2 ON p2.$node_id = r2.$from_id
    JOIN dbo.Person p3 ON r2.$to_id = p3.$node_id
    WHERE p1.$node_id <> p3.$node_id
    GROUP BY r.$edge_id;

    -- You can normalize or store these results as needed
    SELECT TOP 20 * FROM #edge_betweenness ORDER BY betweenness DESC;

    DROP TABLE #edge_betweenness;
END;
GO

-- ============================================================================
-- SECTION 5: NETWORK-LEVEL METRICS
-- ============================================================================

CREATE OR ALTER PROCEDURE sp_ComputeNetworkMetrics
    @network_stats NVARCHAR(MAX) OUTPUT
AS
BEGIN
    DECLARE @node_count INT;
    DECLARE @edge_count INT;
    DECLARE @density FLOAT;
    DECLARE @reciprocity FLOAT;
    DECLARE @avg_degree FLOAT;

    -- Total nodes
    SELECT @node_count = COUNT(*) FROM dbo.Person;

    -- Total edges
    SELECT @edge_count = COUNT(*) FROM dbo.Reposted;

    -- Density = edges / (nodes * (nodes-1)) for directed graphs
    SET @density = CASE 
        WHEN @node_count > 1 
        THEN CAST(@edge_count AS FLOAT) / (@node_count * (@node_count - 1))
        ELSE 0 
    END;

    -- Reciprocity: % of edges that have a reciprocal edge
    DECLARE @reciprocal_edges INT;
    SELECT @reciprocal_edges = COUNT(*)
    FROM dbo.Reposted r1
    WHERE EXISTS (
        SELECT 1 FROM dbo.Reposted r2
        WHERE r1.$from_id = r2.$to_id AND r1.$to_id = r2.$from_id
    );
    SET @reciprocity = CASE WHEN @edge_count > 0 
                           THEN CAST(@reciprocal_edges AS FLOAT) / @edge_count 
                           ELSE 0 END;

    -- Average degree
    SET @avg_degree = CASE WHEN @node_count > 0 
                          THEN CAST(2 * @edge_count AS FLOAT) / @node_count 
                          ELSE 0 END;

    -- Format as JSON for easy R consumption
    SET @network_stats = (
        SELECT 
            @node_count AS nodes,
            @edge_count AS edges,
            @density AS density,
            @reciprocity AS reciprocity,
            @avg_degree AS avg_degree
        FOR JSON PATH, WITHOUT_ARRAY_WRAPPER
    );

    RETURN 0;
END;
GO

-- ============================================================================
-- SECTION 6: INFLUENCE METRICS (Activity-Based Ranking)
-- ============================================================================
-- Simple influence score based on posts authored, reposts made, and reposts received

CREATE OR ALTER PROCEDURE sp_ComputeInfluenceScore
AS
BEGIN
    -- Normalize activity metrics and compute composite influence score
    DECLARE @max_posts INT;
    DECLARE @max_reposts_made INT;
    DECLARE @max_reposts_received INT;

    SELECT 
        @max_posts = MAX(posts_authored),
        @max_reposts_made = MAX(reposts_made),
        @max_reposts_received = MAX(reposts_received)
    FROM dbo.Person;

    UPDATE dbo.Person
    SET influence_score = (
        0.3 * (CAST(posts_authored AS FLOAT) / NULLIF(@max_posts, 0)) +
        0.3 * (CAST(reposts_made AS FLOAT) / NULLIF(@max_reposts_made, 0)) +
        0.4 * (CAST(reposts_received AS FLOAT) / NULLIF(@max_reposts_received, 0))
    ) * 100  -- Scale to 0-100
    WHERE @max_posts > 0 OR @max_reposts_made > 0 OR @max_reposts_received > 0;
END;
GO

-- ============================================================================
-- SECTION 7: HIGH-INFLUENCE SUBGRAPH FILTERING
-- ============================================================================
-- Extract subgraph of users above influence threshold (useful for visualization)

CREATE OR ALTER FUNCTION fn_GetInfluenceSubgraph(
    @influence_threshold INT
)
RETURNS TABLE
AS
RETURN (
    SELECT DISTINCT
        f.handle AS from_handle,
        t.handle AS to_handle,
        r.post_uri,
        r.like_count,
        r.repost_count
    FROM dbo.Reposted r
    JOIN dbo.Person f ON r.$from_id = f.$node_id
    JOIN dbo.Person t ON r.$to_id = t.$node_id
    WHERE f.influence_score >= @influence_threshold
      AND t.influence_score >= @influence_threshold
);
GO

-- ============================================================================
-- EXECUTION: Run all metrics computations
-- ============================================================================

-- 1. Compute basic degree metrics (updates Person table)
EXEC sp_ComputeCloseness;
EXEC sp_ComputeClusteringCoefficient;
EXEC sp_ComputeInfluenceScore;

-- 2. Retrieve network-level metrics
DECLARE @stats NVARCHAR(MAX);
EXEC sp_ComputeNetworkMetrics @network_stats = @stats OUTPUT;
SELECT JSON_VALUE(@stats, '$.density') AS Density,
       JSON_VALUE(@stats, '$.reciprocity') AS Reciprocity,
       JSON_VALUE(@stats, '$.avg_degree') AS AvgDegree;

-- 3. Export all computed metrics back to Person table
SELECT TOP 20
    handle,
    posts_authored,
    reposts_made,
    reposts_received,
    closeness,
    local_clustering,
    influence_score
FROM dbo.Person
ORDER BY influence_score DESC;

-- 4. Example: Get high-influence subgraph
SELECT * FROM fn_GetInfluenceSubgraph(50);

