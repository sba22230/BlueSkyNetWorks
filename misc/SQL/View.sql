-- =============================================================================
-- SECTION 7: SAMPLE VIEWS FOR ANALYSIS
-- =============================================================================

-- View: Top influencers composite ranking
USE [BlueSkyNet]
GO

/****** Object:  View [dbo].[vw_TopInfluencers]    Script Date: 30/01/2026 10:55:54 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


-- =============================================================================
-- SECTION 7: SAMPLE VIEWS FOR ANALYSIS
-- =============================================================================

-- View: Top influencers composite ranking
CREATE OR ALTER VIEW [dbo].[vw_TopInfluencers] AS
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
USE [BlueSkyNet]
GO

/****** Object:  View [dbo].[vw_CommunityProfiles]    Script Date: 30/01/2026 10:58:10 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO






-- View: Community profiles
CREATE OR ALTER   VIEW [dbo].[vw_CommunityProfiles] AS
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






