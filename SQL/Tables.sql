USE [BlueSkyNet]
GO

-- Created by GitHub Copilot in SSMS - review carefully before executing
SET NOCOUNT ON;

DECLARE @schema SYSNAME = N'dbo';
DECLARE @table SYSNAME = N'Person';

DECLARE @cols TABLE (col SYSNAME, defExpr NVARCHAR(200));
INSERT INTO @cols (col, defExpr) VALUES
  (N'posts_authored', N'DEFAULT (0)'),
  (N'reposts_made', N'DEFAULT (0)'),
  (N'reposts_received', N'DEFAULT (0)'),
  (N'total_likes_on_posts', N'DEFAULT (0)'),
  (N'total_replies_on_posts', N'DEFAULT (0)'),
  (N'total_bookmarks_on_posts', N'DEFAULT (0)');

DECLARE @col SYSNAME;
DECLARE @defExpr NVARCHAR(200);
DECLARE @newConstraint SYSNAME;
DECLARE @oldConstraint SYSNAME;
DECLARE @sql NVARCHAR(MAX);

WHILE EXISTS (SELECT 1 FROM @cols)
BEGIN
    SELECT TOP (1) @col = col, @defExpr = defExpr FROM @cols;
    SET @newConstraint = N'DF_' + @schema + N'_' + @table + N'_' + @col;

    SELECT @oldConstraint = dc.name
    FROM sys.default_constraints dc
    JOIN sys.columns c ON dc.parent_object_id = c.object_id AND dc.parent_column_id = c.column_id
    JOIN sys.tables t ON t.object_id = c.object_id
    JOIN sys.schemas s ON s.schema_id = t.schema_id
    WHERE s.name = @schema AND t.name = @table AND c.name = @col;

    IF @oldConstraint IS NOT NULL
    BEGIN
        SET @sql = N'ALTER TABLE ' + QUOTENAME(@schema) + N'.' + QUOTENAME(@table)
                 + N' DROP CONSTRAINT ' + QUOTENAME(@oldConstraint) + N';';
        EXEC sp_executesql @sql;
    END

    --SET @sql = N'ALTER TABLE ' + QUOTENAME(@schema) + N'.' + QUOTENAME(@table)
    --         + N' ADD CONSTRAINT ' + QUOTENAME(@newConstraint) + N' ' + @defExpr
    --         + N' FOR ' + QUOTENAME(@col) + N';';
    --EXEC sp_executesql @sql;

    DELETE FROM @cols WHERE col = @col;
END

SET NOCOUNT OFF;

/****** Object:  Table [dbo].[Person]    Script Date: 30/01/2026 11:00:36 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Person]') AND type in (N'U'))
DROP TABLE [dbo].[Person]
GO

/****** Object:  Table [dbo].[Person]    Script Date: 30/01/2026 11:00:36 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

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
	[betweenness] [float] NULL,
	[betweenness_norm] [float] NULL,
	[closeness] [float] NULL,
	[closeness_norm] [float] NULL,
	[eigenvector_centrality] [float] NULL,
	[pagerank] [float] NULL,
	[pagerank_norm] [float] NULL,
	[hub_score] [float] NULL,
	[authority_score] [float] NULL,
	[authority_norm] [float] NULL,
	[local_clustering] [float] NULL,
	[kcore] [int] NULL,
	[in_degree] [int] NULL,
	[out_degree] [int] NULL,
	[total_degree] [int] NULL,
	[community] [int] NULL,
	[modularity] [float] NULL,
	[influence_score] [float] NULL,
PRIMARY KEY CLUSTERED 
(
	[handle] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [dataonly]
)
AS NODE ON [dataonly]
GO

-- Created by GitHub Copilot in SSMS - review carefully before executing
SET NOCOUNT ON;

DECLARE @schema SYSNAME = N'dbo';
DECLARE @table SYSNAME = N'Person';

DECLARE @cols TABLE (col SYSNAME, defExpr NVARCHAR(200));
INSERT INTO @cols (col, defExpr) VALUES
  (N'posts_authored', N'DEFAULT (0)'),
  (N'reposts_made', N'DEFAULT (0)'),
  (N'reposts_received', N'DEFAULT (0)'),
  (N'total_likes_on_posts', N'DEFAULT (0)'),
  (N'total_replies_on_posts', N'DEFAULT (0)'),
  (N'total_bookmarks_on_posts', N'DEFAULT (0)');

DECLARE @col SYSNAME;
DECLARE @defExpr NVARCHAR(200);
DECLARE @newConstraint SYSNAME;
DECLARE @oldConstraint SYSNAME;
DECLARE @sql NVARCHAR(MAX);

WHILE EXISTS (SELECT 1 FROM @cols)
BEGIN
    SELECT TOP (1) @col = col, @defExpr = defExpr FROM @cols;
    SET @newConstraint = N'DF_' + @schema + N'_' + @table + N'_' + @col;

    SELECT @oldConstraint = dc.name
    FROM sys.default_constraints dc
    JOIN sys.columns c ON dc.parent_object_id = c.object_id AND dc.parent_column_id = c.column_id
    JOIN sys.tables t ON t.object_id = c.object_id
    JOIN sys.schemas s ON s.schema_id = t.schema_id
    WHERE s.name = @schema AND t.name = @table AND c.name = @col;

    IF @oldConstraint IS NOT NULL
    BEGIN
        SET @sql = N'ALTER TABLE ' + QUOTENAME(@schema) + N'.' + QUOTENAME(@table)
                 + N' DROP CONSTRAINT ' + QUOTENAME(@oldConstraint) + N';';
        EXEC sp_executesql @sql;
    END

    SET @sql = N'ALTER TABLE ' + QUOTENAME(@schema) + N'.' + QUOTENAME(@table)
             + N' ADD CONSTRAINT ' + QUOTENAME(@newConstraint) + N' ' + @defExpr
             + N' FOR ' + QUOTENAME(@col) + N';';
    EXEC sp_executesql @sql;

    DELETE FROM @cols WHERE col = @col;
END

SET NOCOUNT OFF;
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
-- SECTION 1: ALTER Person TABLE TO ADD NODE-LEVEL METRICS
-- =============================================================================
/* Added to 1a_load network data into SQL.r
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
*/

-- 2. Drop existing edge table if it exists
USE [BlueSkyNet]
GO

/****** Object:  Table [dbo].[Reposted]    Script Date: 30/01/2026 11:01:35 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[Reposted]') AND type in (N'U'))
DROP TABLE [dbo].[Reposted]
GO

/****** Object:  Table [dbo].[Reposted]    Script Date: 30/01/2026 11:01:35 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

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
GO

-- =============================================================================
-- SECTION 2: CREATE GlobalNetworkMetrics TABLE
-- =============================================================================
-- Store snapshot metrics computed at analysis time
-- Allows tracking changes across different analysis runs

USE [BlueSkyNet]
GO

-- Created by GitHub Copilot in SSMS - review carefully before executing
DECLARE @schema SYSNAME = N'dbo';
DECLARE @table SYSNAME = N'GlobalNetworkMetrics';
DECLARE @column SYSNAME = N'analysis_timestamp';
DECLARE @newConstraint SYSNAME = N'DF_dbo_GlobalNetworkMetrics_analysis_timestamp';
DECLARE @sql NVARCHAR(MAX);
DECLARE @oldConstraint SYSNAME;

SELECT @oldConstraint = dc.name
FROM sys.default_constraints dc
JOIN sys.columns c ON dc.parent_object_id = c.object_id AND dc.parent_column_id = c.column_id
JOIN sys.tables t ON t.object_id = c.object_id
JOIN sys.schemas s ON s.schema_id = t.schema_id
WHERE s.name = @schema AND t.name = @table AND c.name = @column;

IF @oldConstraint IS NOT NULL
BEGIN
    SET @sql = N'ALTER TABLE ' + QUOTENAME(@schema) + N'.' + QUOTENAME(@table) + N' DROP CONSTRAINT ' + QUOTENAME(@oldConstraint) + N';';
    EXEC sp_executesql @sql;
END
GO

/****** Object:  Table [dbo].[GlobalNetworkMetrics]    Script Date: 30/01/2026 10:40:27 ******/
-- Dynamically drop any FK that references GlobalNetworkMetrics
DECLARE @sql NVARCHAR(MAX) = '';

SELECT @sql += 'ALTER TABLE ' + QUOTENAME(OBJECT_SCHEMA_NAME(parent_object_id)) + '.'
               + QUOTENAME(OBJECT_NAME(parent_object_id)) + ' DROP CONSTRAINT ' + QUOTENAME(name) + ';' + CHAR(13)
FROM sys.foreign_keys
WHERE referenced_object_id = OBJECT_ID('dbo.GlobalNetworkMetrics');

IF (@sql <> '') EXEC sp_executesql @sql;
-- Now you can safely:
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[GlobalNetworkMetrics]') AND type in (N'U'))
DROP TABLE [dbo].[GlobalNetworkMetrics]
GO

/****** Object:  Table [dbo].[GlobalNetworkMetrics]    Script Date: 30/01/2026 10:40:27 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[GlobalNetworkMetrics](
	[metric_id] [int] IDENTITY(1,1) NOT NULL,
	[analysis_timestamp] [datetime2](7) NULL,
	[search_term] [nvarchar](255) NULL,
	[network_size] [int] NOT NULL,
	[edge_count] [int] NOT NULL,
	[dyad_count] [int] NOT NULL,
	[density] [float] NOT NULL,
	[reciprocity_edgewise] [float] NOT NULL,
	[reciprocity_dyadic] [float] NOT NULL,
	[diameter] [int] NOT NULL,
	[avg_path_length] [float] NOT NULL,
	[num_components] [int] NOT NULL,
	[giant_component_size] [int] NOT NULL,
	[giant_component_pct] [float] NOT NULL,
	[global_clustering] [float] NOT NULL,
	[avg_local_clustering] [float] NOT NULL,
	[centralization_indegree] [float] NOT NULL,
	[num_communities] [int] NOT NULL,
	[modularity] [float] NOT NULL,
	[analysis_notes] [nvarchar](max) NULL,
PRIMARY KEY CLUSTERED 
(
	[metric_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [dataonly]
) ON [dataonly] TEXTIMAGE_ON [dataonly]
GO

-- Add (or re-add) deterministic named default constraint. Use SYSUTCDATETIME() to match datetime2.
-- Created by GitHub Copilot in SSMS - review carefully before executing
DECLARE @schema SYSNAME = N'dbo';
DECLARE @table SYSNAME = N'GlobalNetworkMetrics';
DECLARE @column SYSNAME = N'analysis_timestamp';
DECLARE @newConstraint SYSNAME = N'DF_dbo_GlobalNetworkMetrics_analysis_timestamp';
DECLARE @sql NVARCHAR(MAX);
DECLARE @oldConstraint SYSNAME;

SELECT @oldConstraint = dc.name
FROM sys.default_constraints dc
JOIN sys.columns c ON dc.parent_object_id = c.object_id AND dc.parent_column_id = c.column_id
JOIN sys.tables t ON t.object_id = c.object_id
JOIN sys.schemas s ON s.schema_id = t.schema_id
WHERE s.name = @schema AND t.name = @table AND c.name = @column;

SET @sql = N'ALTER TABLE ' + QUOTENAME(@schema) + N'.' + QUOTENAME(@table) + N' ADD CONSTRAINT ' + QUOTENAME(@newConstraint) + N' DEFAULT (SYSUTCDATETIME()) FOR ' + QUOTENAME(@column) + N';';
EXEC sp_executesql @sql;
GO

-- =============================================================================
-- SECTION 3: CREATE CommunityStats TABLE
-- =============================================================================
-- Store per-community aggregated metrics

USE [BlueSkyNet]
GO
--ALTER TABLE [dbo].[CommunityStats] DROP CONSTRAINT [fk_community_snapshot]
GO

/****** Object:  Table [dbo].[CommunityStats]    Script Date: 30/01/2026 10:41:04 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[CommunityStats]') AND type in (N'U'))
DROP TABLE [dbo].[CommunityStats]
GO

/****** Object:  Table [dbo].[CommunityStats]    Script Date: 30/01/2026 10:41:04 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[CommunityStats](
	[community_stats_id] [int] IDENTITY(1,1) NOT NULL,
	[metric_snapshot_id] [int] NULL,
	[community_id] [int] NOT NULL,
	[community_size] [int] NOT NULL,
	[internal_edges] [float] NOT NULL,
	[avg_internal_degree] [float] NOT NULL,
	[avg_pagerank] [float] NOT NULL,
	[avg_authority] [float] NOT NULL,
	[avg_betweenness] [float] NOT NULL,
	[avg_local_clustering] [float] NOT NULL,
	[earliest_post] [datetime2](7) NULL,
	[latest_post] [datetime2](7) NULL,
PRIMARY KEY CLUSTERED 
(
	[community_stats_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [dataonly]
) ON [dataonly]
GO

ALTER TABLE [dbo].[CommunityStats]  WITH CHECK ADD  CONSTRAINT [fk_community_snapshot] FOREIGN KEY([metric_snapshot_id])
REFERENCES [dbo].[GlobalNetworkMetrics] ([metric_id])
ON DELETE CASCADE
GO

ALTER TABLE [dbo].[CommunityStats] CHECK CONSTRAINT [fk_community_snapshot]
GO

CREATE NONCLUSTERED INDEX idx_community_id ON dbo.CommunityStats(community_stats_id);
CREATE NONCLUSTERED INDEX idx_snapshot ON dbo.CommunityStats(metric_snapshot_id);
GO

-- =============================================================================
-- SECTION 4: CREATE TriadCensus TABLE
-- =============================================================================
-- Store triad census distributions for motif analysis

USE [BlueSkyNet]
GO

--ALTER TABLE [dbo].[TriadCensus] DROP CONSTRAINT [fk_triad_snapshot]
GO

/****** Object:  Table [dbo].[TriadCensus]    Script Date: 30/01/2026 10:41:48 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[TriadCensus]') AND type in (N'U'))
DROP TABLE [dbo].[TriadCensus]
GO

/****** Object:  Table [dbo].[TriadCensus]    Script Date: 30/01/2026 10:41:48 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[TriadCensus](
	[triad_census_id] [int] IDENTITY(1,1) NOT NULL,
	[metric_snapshot_id] [int] NOT NULL,
	[triad_type] [int] NOT NULL,
	[count] [int] NOT NULL,
	[proportion] [float] NULL,
	[description] [nvarchar](255) NULL,
PRIMARY KEY CLUSTERED 
(
	[triad_census_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [dataonly]
) ON [dataonly]
GO

ALTER TABLE [dbo].[TriadCensus]  WITH CHECK ADD  CONSTRAINT [fk_triad_snapshot] FOREIGN KEY([metric_snapshot_id])
REFERENCES [dbo].[GlobalNetworkMetrics] ([metric_id])
ON DELETE CASCADE
GO

ALTER TABLE [dbo].[TriadCensus] CHECK CONSTRAINT [fk_triad_snapshot]
GO

CREATE NONCLUSTERED INDEX idx_snapshot ON dbo.TriadCensus(metric_snapshot_id);
-- =============================================================================
-- SECTION 5: CREATE InfluenceRank TABLE
-- =============================================================================
-- Composite ranking combining multiple centrality measures
-- Useful for identifying "super-influencers"

USE [BlueSkyNet]
GO

--ALTER TABLE [dbo].[InfluenceRank] DROP CONSTRAINT [fk_influence_snapshot]
GO

/****** Object:  Table [dbo].[InfluenceRank]    Script Date: 30/01/2026 10:42:25 ******/
IF  EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[InfluenceRank]') AND type in (N'U'))
DROP TABLE [dbo].[InfluenceRank]
GO

/****** Object:  Table [dbo].[InfluenceRank]    Script Date: 30/01/2026 10:42:25 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[InfluenceRank](
	[influence_rank_id] [int] IDENTITY(1,1) NOT NULL,
	[metric_snapshot_id] [int] NOT NULL,
	[handle] [nvarchar](255) NOT NULL,
	[pagerank_rank] [int] NOT NULL,
	[betweenness_rank] [int] NOT NULL,
	[authority_rank] [int] NOT NULL,
	[degree_rank] [int] NOT NULL,
	[composite_influence_score] [float] NOT NULL,
	[overall_rank] [int] NOT NULL,
	[reposts_received] [int] NULL,
	[reposts_made] [int] NULL,
	[posts_authored] [int] NULL,
	[community] [int] NULL,
PRIMARY KEY CLUSTERED 
(
	[influence_rank_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [dataonly]
) ON [dataonly]
GO

ALTER TABLE [dbo].[InfluenceRank]  WITH CHECK ADD  CONSTRAINT [fk_influence_snapshot] FOREIGN KEY([metric_snapshot_id])
REFERENCES [dbo].[GlobalNetworkMetrics] ([metric_id])
ON DELETE CASCADE
GO

ALTER TABLE [dbo].[InfluenceRank] CHECK CONSTRAINT [fk_influence_snapshot]
GO

CREATE NONCLUSTERED INDEX idx_overall_rank ON dbo.InfluenceRank(overall_rank);
CREATE NONCLUSTERED INDEX idx_snapshot ON dbo.InfluenceRank(metric_snapshot_id);
CREATE NONCLUSTERED INDEX idx_handle ON dbo.InfluenceRank(handle);
GO
