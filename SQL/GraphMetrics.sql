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

