-- ============================================================================
-- SECTION 7: HIGH-INFLUENCE SUBGRAPH FILTERING
-- ============================================================================
-- Extract subgraph of users above influence threshold (useful for visualization)

USE [BlueSkyNet]
GO

/****** Object:  UserDefinedFunction [dbo].[fn_GetInfluenceSubgraph]    Script Date: 30/01/2026 10:18:13 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE OR ALTER   FUNCTION [dbo].[fn_GetInfluenceSubgraph](
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



