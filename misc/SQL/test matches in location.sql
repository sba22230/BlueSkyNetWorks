;WITH Matches AS
(
    SELECT
        p.PosterID,
        lp.country_code,
        lp.jurisdiction,
        lp.region,
        lp.locality,
        lp.evidence_type,
        lp.evidence_weight,
        lp.phrase
    FROM dbo.posters p
    JOIN dbo.location_phrase lp
        ON LOWER(p.description)
           LIKE '%' + LOWER(lp.phrase) + '%'
    WHERE p.PosterID = 4257
)
SELECT
    country_code,
    jurisdiction,
    region,
    locality,
    SUM(evidence_weight) AS Score,
    COUNT(*) AS MatchCount,
    STRING_AGG(phrase,'; ') AS Evidence
FROM Matches

GROUP BY
    country_code,
    jurisdiction,
    region,
    locality
ORDER BY
    Score DESC;