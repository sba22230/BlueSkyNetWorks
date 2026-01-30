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
