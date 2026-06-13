-- BlueSkyNetWorks: SQL Schema for Network Metrics Storage
-- Purpose: Extend the Person and network tables with comprehensive metrics
-- Run this after 4_ComputeMetrics.R generates results
-- Create index on centrality metrics for fast filtering
-- =============================================================================
-- SECTION 8: INSTRUCTIONS FOR IMPORTING METRICS FROM R
-- =============================================================================
/*
STEPS TO POPULATE METRICS FROM 4_ComputeMetrics.R:

1. Run 4_ComputeMetrics.R to generate:
   - Report/_site/graphs/nodes_with_metrics.csv
   - Report/_site/graphs/network_metrics.csv
   - Report/_site/graphs/community_stats.csv
   - Report/_site/graphs/triad_census.csv

2. In SSMS, execute this script to create tables and procedures

3. Load node metrics into Person table:
   
   MERGE INTO dbo.Person AS target
   USING (
       SELECT handle, betweenness, closeness, eigenvector_centrality,
              pagerank, hub_score, authority_score, local_clustering, kcore,
              in_degree, out_degree, total_degree, community
       FROM OPENROWSET(
           BULK 'Report/_site/graphs/nodes_with_metrics.csv',
           DATA_SOURCE = 'YOUR_DATA_SOURCE',
           FORMAT = 'CSV', FIRSTROW = 2
       ) AS source(...)
   ) AS source ON target.handle = source.handle
   WHEN MATCHED THEN UPDATE SET
       target.betweenness = source.betweenness,
       target.pagerank = source.pagerank,
       -- ... other columns
   WHEN NOT MATCHED THEN INSERT (...) VALUES (...);

4. Insert global metrics using the stored procedure:
   
   DECLARE @metric_id INT;
   EXEC dbo.InsertGlobalMetrics
       @network_size = 12345,
       @edge_count = 67890,
       -- ... other parameters
       @search_term = 'Speirgorm',
       @metric_id = @metric_id OUTPUT;

5. Query the results using vw_TopInfluencers, vw_CommunityProfiles, or stored procs:
   
   EXEC dbo.GetTopInfluencers @top_n = 25, @metric_name = 'pagerank';
   EXEC dbo.GetCommunitySummary;

*/
