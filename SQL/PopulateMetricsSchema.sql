-- BlueSkyNetWorks: SQL Schema for Network Metrics Storage
-- Purpose: Extend the Person and network tables with comprehensive metrics
-- Run this after 4_ComputeMetrics.R generates results

-- =============================================================================
-- SECTION 1: ALTER Person TABLE TO ADD NODE-LEVEL METRICS
-- =============================================================================

-- Add centrality metrics to Person node table
ALTER TABLE dbo.Person ADD
    -- Core centrality measures
    betweenness FLOAT NULL,
    betweenness_norm FLOAT NULL,
    closeness FLOAT NULL,
    closeness_norm FLOAT NULL,
    eigenvector_centrality FLOAT NULL,
    pagerank FLOAT NULL,
    pagerank_norm FLOAT NULL,
    
    -- Authority and hub scores
    hub_score FLOAT NULL,
    authority_score FLOAT NULL,
    authority_norm FLOAT NULL,
    
    -- Clustering and cohesion
    local_clustering FLOAT NULL,
    kcore INT NULL,
    
    -- Degree variants (already may exist, but explicit here)
    in_degree INT NULL,
    out_degree INT NULL,
    total_degree INT NULL,
    
    -- Community assignment
    community INT NULL,
    modularity FLOAT NULL,
    
    -- Engagement influence score (composite)
    influence_score FLOAT NULL  -- can be computed as weighted combo of metrics
;
GO

-- Create index on centrality metrics for fast filtering
CREATE NONCLUSTERED INDEX idx_pagerank ON dbo.Person(pagerank DESC)
    WHERE pagerank IS NOT NULL;
GO

CREATE NONCLUSTERED INDEX idx_betweenness ON dbo.Person(betweenness DESC)
    WHERE betweenness IS NOT NULL;
GO

CREATE NONCLUSTERED INDEX idx_community ON dbo.Person(community)
    WHERE community IS NOT NULL;
GO

-- =============================================================================
-- SECTION 2: CREATE GlobalNetworkMetrics TABLE
-- =============================================================================
-- Store snapshot metrics computed at analysis time
-- Allows tracking changes across different analysis runs

CREATE TABLE dbo.GlobalNetworkMetrics (
    metric_id INT IDENTITY(1,1) PRIMARY KEY,
    analysis_timestamp DATETIME2(7) DEFAULT GETUTCDATE(),
    search_term NVARCHAR(255) NULL,  -- e.g., "Speirgorm"
    
    -- Basic network structure
    network_size INT NOT NULL,
    edge_count INT NOT NULL,
    dyad_count INT NOT NULL,
    
    -- Density and connectivity
    density FLOAT NOT NULL,
    reciprocity_edgewise FLOAT NOT NULL,
    reciprocity_dyadic FLOAT NOT NULL,
    
    -- Path-based metrics
    diameter INT NOT NULL,
    avg_path_length FLOAT NOT NULL,
    
    -- Components
    num_components INT NOT NULL,
    giant_component_size INT NOT NULL,
    giant_component_pct FLOAT NOT NULL,
    
    -- Clustering
    global_clustering FLOAT NOT NULL,
    avg_local_clustering FLOAT NOT NULL,
    
    -- Centralization
    centralization_indegree FLOAT NOT NULL,
    
    -- Community structure
    num_communities INT NOT NULL,
    modularity FLOAT NOT NULL,
    
    -- Metadata
    analysis_notes NVARCHAR(MAX) NULL,
    
    INDEX idx_timestamp ON analysis_timestamp,
    INDEX idx_search_term ON search_term
);
GO

-- =============================================================================
-- SECTION 3: CREATE CommunityStats TABLE
-- =============================================================================
-- Store per-community aggregated metrics

CREATE TABLE dbo.CommunityStats (
    community_stats_id INT IDENTITY(1,1) PRIMARY KEY,
    metric_snapshot_id INT NULL,  -- foreign key to GlobalNetworkMetrics
    community_id INT NOT NULL,
    
    -- Community size and structure
    community_size INT NOT NULL,
    internal_edges FLOAT NOT NULL,
    avg_internal_degree FLOAT NOT NULL,
    
    -- Community influence
    avg_pagerank FLOAT NOT NULL,
    avg_authority FLOAT NOT NULL,
    avg_betweenness FLOAT NOT NULL,
    
    -- Community cohesion
    avg_local_clustering FLOAT NOT NULL,
    
    -- Temporal info
    earliest_post DATETIME2(7) NULL,
    latest_post DATETIME2(7) NULL,
    
    --INDEX idx_community_id ON community_id,
    --INDEX idx_snapshot ON metric_snapshot_id,
    
    CONSTRAINT fk_community_snapshot 
        FOREIGN KEY (metric_snapshot_id) 
        REFERENCES dbo.GlobalNetworkMetrics(metric_id) ON DELETE CASCADE
);
GO
CREATE NONCLUSTERED INDEX idx_community_id ON dbo.CommunityStats(community_stats_id);
CREATE NONCLUSTERED INDEX idx_snapshot ON dbo.CommunityStats(metric_snapshot_id);
GO

-- =============================================================================
-- SECTION 4: CREATE TriadCensus TABLE
-- =============================================================================
-- Store triad census distributions for motif analysis

CREATE TABLE dbo.TriadCensus (
    triad_census_id INT IDENTITY(1,1) PRIMARY KEY,
    metric_snapshot_id INT NOT NULL,
    
    -- Triad type (0-15)
    triad_type INT NOT NULL,
    count INT NOT NULL,
    proportion FLOAT NULL,  -- count / total triads
    
    -- Descriptive label
    [description] NVARCHAR(255) NULL,
    
    --INDEX idx_snapshot ON metric_snapshot_id,
    
    CONSTRAINT fk_triad_snapshot 
        FOREIGN KEY (metric_snapshot_id) 
        REFERENCES dbo.GlobalNetworkMetrics(metric_id) ON DELETE CASCADE
);
GO
CREATE NONCLUSTERED INDEX idx_snapshot ON dbo.TriadCensus(metric_snapshot_id);
-- =============================================================================
-- SECTION 5: CREATE InfluenceRank TABLE
-- =============================================================================
-- Composite ranking combining multiple centrality measures
-- Useful for identifying "super-influencers"

CREATE TABLE dbo.InfluenceRank (
    influence_rank_id INT IDENTITY(1,1) PRIMARY KEY,
    metric_snapshot_id INT NOT NULL,
    handle NVARCHAR(255) NOT NULL,
    
    -- Individual rankings
    pagerank_rank INT NOT NULL,
    betweenness_rank INT NOT NULL,
    authority_rank INT NOT NULL,
    degree_rank INT NOT NULL,
    
    -- Composite score (average of normalized ranks)
    composite_influence_score FLOAT NOT NULL,
    overall_rank INT NOT NULL,
    
    -- Engagement metrics for context
    reposts_received INT NULL,
    reposts_made INT NULL,
    posts_authored INT NULL,
    
    -- Community membership
    community INT NULL,
    
    --INDEX idx_overall_rank ON overall_rank,
    --INDEX idx_snapshot ON metric_snapshot_id,
    --INDEX idx_handle ON handle,
    
    CONSTRAINT fk_influence_snapshot 
        FOREIGN KEY (metric_snapshot_id) 
        REFERENCES dbo.GlobalNetworkMetrics(metric_id) ON DELETE CASCADE
);
GO
CREATE NONCLUSTERED INDEX idx_overall_rank ON dbo.InfluenceRank(overall_rank);
CREATE NONCLUSTERED INDEX idx_snapshot ON dbo.InfluenceRank(metric_snapshot_id);
CREATE NONCLUSTERED INDEX idx_handle ON dbo.InfluenceRank(handle);
GO

-- =============================================================================
-- SECTION 6: STORED PROCEDURES FOR COMMON QUERIES
-- =============================================================================

-- Procedure: Insert global metrics from R output
CREATE PROCEDURE dbo.InsertGlobalMetrics
    @network_size INT,
    @edge_count INT,
    @dyad_count INT,
    @density FLOAT,
    @reciprocity_edgewise FLOAT,
    @reciprocity_dyadic FLOAT,
    @diameter INT,
    @avg_path_length FLOAT,
    @num_components INT,
    @giant_component_size INT,
    @giant_component_pct FLOAT,
    @global_clustering FLOAT,
    @avg_local_clustering FLOAT,
    @centralization_indegree FLOAT,
    @num_communities INT,
    @modularity FLOAT,
    @search_term NVARCHAR(255) = NULL,
    @analysis_notes NVARCHAR(MAX) = NULL,
    @metric_id INT OUTPUT
AS
BEGIN
    INSERT INTO dbo.GlobalNetworkMetrics (
        search_term, network_size, edge_count, dyad_count,
        density, reciprocity_edgewise, reciprocity_dyadic,
        diameter, avg_path_length, num_components, giant_component_size, giant_component_pct,
        global_clustering, avg_local_clustering, centralization_indegree,
        num_communities, modularity, analysis_notes
    )
    VALUES (
        @search_term, @network_size, @edge_count, @dyad_count,
        @density, @reciprocity_edgewise, @reciprocity_dyadic,
        @diameter, @avg_path_length, @num_components, @giant_component_size, @giant_component_pct,
        @global_clustering, @avg_local_clustering, @centralization_indegree,
        @num_communities, @modularity, @analysis_notes
    );
    
    SET @metric_id = SCOPE_IDENTITY();
END;
GO

-- Procedure: Get top influencers by various measures
CREATE PROCEDURE dbo.GetTopInfluencers
    @top_n INT = 20,
    @metric_name NVARCHAR(50) = 'pagerank'  -- 'pagerank', 'betweenness', 'authority', 'degree'
AS
BEGIN
    IF @metric_name = 'pagerank'
        SELECT TOP (@top_n) 
            handle, pagerank, in_degree, out_degree, 
            reposts_received, reposts_made, community
        FROM dbo.Person
        WHERE pagerank IS NOT NULL
        ORDER BY pagerank DESC;
    
    ELSE IF @metric_name = 'betweenness'
        SELECT TOP (@top_n) 
            handle, betweenness, total_degree, 
            reposts_received, reposts_made, community
        FROM dbo.Person
        WHERE betweenness IS NOT NULL
        ORDER BY betweenness DESC;
    
    ELSE IF @metric_name = 'authority'
        SELECT TOP (@top_n) 
            handle, authority_score, in_degree, 
            reposts_received, posts_authored, community
        FROM dbo.Person
        WHERE authority_score IS NOT NULL
        ORDER BY authority_score DESC;
    
    ELSE IF @metric_name = 'degree'
        SELECT TOP (@top_n) 
            handle, total_degree, in_degree, out_degree,
            reposts_received, reposts_made, community
        FROM dbo.Person
        WHERE total_degree IS NOT NULL
        ORDER BY total_degree DESC;
    
    ELSE
        RAISERROR('Invalid metric_name. Use: pagerank, betweenness, authority, or degree', 16, 1);
END;
GO

-- Procedure: Get community summary
CREATE PROCEDURE dbo.GetCommunitySummary
    @metric_snapshot_id INT = NULL
AS
BEGIN
    -- If no snapshot specified, use most recent
    IF @metric_snapshot_id IS NULL
        SET @metric_snapshot_id = (
            SELECT TOP 1 metric_id FROM dbo.GlobalNetworkMetrics 
            ORDER BY analysis_timestamp DESC
        );
    
    SELECT 
        cs.community_id,
        cs.community_size,
        cs.internal_edges,
        cs.avg_internal_degree,
        cs.avg_pagerank,
        cs.avg_authority,
        cs.avg_local_clustering,
        COUNT(DISTINCT p.handle) AS member_count,
        STRING_AGG(p.handle, ', ') WITHIN GROUP (ORDER BY p.pagerank DESC) AS top_members
    FROM dbo.CommunityStats cs
    LEFT JOIN dbo.Person p ON p.community = cs.community_id
    WHERE cs.metric_snapshot_id = @metric_snapshot_id
    GROUP BY cs.community_id, cs.community_size, cs.internal_edges, 
             cs.avg_internal_degree, cs.avg_pagerank, cs.avg_authority, cs.avg_local_clustering
    ORDER BY cs.community_size DESC;
END;
GO

-- Procedure: Compare metrics across analysis runs
CREATE PROCEDURE dbo.CompareMetricsOverTime
    @top_n INT = 10
AS
BEGIN
    SELECT 
        gm.analysis_timestamp,
        gm.network_size,
        gm.edge_count,
        gm.density,
        gm.global_clustering,
        gm.avg_path_length,
        gm.num_communities,
        gm.modularity,
        gm.giant_component_pct
    FROM dbo.GlobalNetworkMetrics gm
    ORDER BY gm.analysis_timestamp DESC;
END;
GO

-- =============================================================================
-- SECTION 7: SAMPLE VIEWS FOR ANALYSIS
-- =============================================================================

-- View: Top influencers composite ranking
CREATE VIEW vw_TopInfluencers AS
SELECT 
    p.handle,
    p.pagerank,
    p.betweenness,
    p.authority_score,
    p.total_degree,
    p.community,
    p.reposts_received,
    p.reposts_made,
    p.posts_authored,
    -- Composite influence score
    (
        ISNULL(p.pagerank_norm, 0) * 0.4 +
        ISNULL(p.betweenness_norm, 0) * 0.3 +
        ISNULL(p.authority_norm, 0) * 0.3
    ) AS composite_influence_score
FROM dbo.Person p
WHERE p.pagerank IS NOT NULL
    AND p.betweenness IS NOT NULL
    AND p.authority_score IS NOT NULL;
GO

-- View: Community profiles
CREATE VIEW vw_CommunityProfiles AS
SELECT 
    p.community,
    COUNT(DISTINCT p.handle) AS member_count,
    AVG(p.pagerank) AS avg_pagerank,
    AVG(p.betweenness) AS avg_betweenness,
    AVG(p.total_degree) AS avg_degree,
    SUM(p.reposts_received) AS total_reposts_received,
    SUM(p.posts_authored) AS total_posts_authored,
    CAST(SUM(p.posts_authored) AS FLOAT) / NULLIF(COUNT(DISTINCT p.handle), 0) AS avg_posts_per_member
FROM dbo.Person p
WHERE p.community IS NOT NULL
GROUP BY p.community;
GO

-- =============================================================================
-- SECTION 8: INSTRUCTIONS FOR IMPORTING METRICS FROM R
-- =============================================================================
/*
STEPS TO POPULATE METRICS FROM 4_ComputeMetrics.R:

1. Run 4_ComputeMetrics.R to generate:
   - graphs/nodes_with_metrics.csv
   - graphs/network_metrics.csv
   - graphs/community_stats.csv
   - graphs/triad_census.csv

2. In SSMS, execute this script to create tables and procedures

3. Load node metrics into Person table:
   
   MERGE INTO dbo.Person AS target
   USING (
       SELECT handle, betweenness, closeness, eigenvector_centrality,
              pagerank, hub_score, authority_score, local_clustering, kcore,
              in_degree, out_degree, total_degree, community
       FROM OPENROWSET(
           BULK 'graphs/nodes_with_metrics.csv',
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
