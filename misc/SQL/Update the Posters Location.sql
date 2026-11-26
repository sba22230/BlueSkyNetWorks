DROP INDEX [VectorIndex-cosine] ON [dbo].[posters];
GO
/* Clear previous classifications if rerunning */

UPDATE dbo.posters
SET
    location_country_code = NULL,
    location_jurisdiction = NULL,
    location_region = NULL,
    location_locality = NULL,
    location_status = NULL,
    location_confidence = NULL,
    location_method = NULL,
    location_evidence = NULL,
    location_rule_version = NULL,
    location_classified_at = NULL;
GO


;WITH Matches AS
(
    SELECT
        p.PosterID,

        lp.country_code,
        lp.jurisdiction,
        lp.region,
        lp.locality,

        COUNT(*) AS MatchCount,

        SUM(lp.evidence_weight) AS Score,

        STRING_AGG(lp.phrase, '; ')
            WITHIN GROUP (ORDER BY lp.evidence_weight DESC)
            AS Evidence

    FROM dbo.posters p
    INNER JOIN dbo.location_phrase lp
        ON LOWER(p.description)
           LIKE '%' + LOWER(lp.phrase) + '%'

    WHERE lp.active = 1
      AND lp.region IS NOT NULL

    GROUP BY
        p.PosterID,
        lp.country_code,
        lp.jurisdiction,
        lp.region,
        lp.locality
),

Ranked AS
(
    SELECT
        m.*,

        ROW_NUMBER() OVER
        (
            PARTITION BY m.PosterID
            ORDER BY
                m.Score DESC,
                m.MatchCount DESC
        ) AS rn,

        COUNT(*) OVER
        (
            PARTITION BY m.PosterID
        ) AS CandidateCount,

        LEAD(m.Score) OVER
        (
            PARTITION BY m.PosterID
            ORDER BY
                m.Score DESC,
                m.MatchCount DESC
        ) AS SecondScore

    FROM Matches m
),

BestMatch AS
(
    SELECT *
    FROM Ranked
    WHERE rn = 1
)

UPDATE p
SET
    location_country_code   = b.country_code,
    location_jurisdiction   = b.jurisdiction,
    location_region         = b.region,
    location_locality       = b.locality,

    location_status =
        CASE

            /* Only one location candidate */
            WHEN b.CandidateCount = 1
                THEN 'resolved'

            /* Clear winner */
            WHEN b.SecondScore IS NOT NULL
             AND b.Score >= b.SecondScore * 1.5
                THEN 'resolved'

            /* Good winner */
            WHEN b.SecondScore IS NOT NULL
             AND b.Score - b.SecondScore >= 0.50
                THEN 'resolved'

            /* Close call */
            WHEN b.SecondScore IS NOT NULL
             AND ABS(b.Score - b.SecondScore) < 0.20
                THEN 'conflicting'

            ELSE 'probable'

        END,

    location_confidence =
        CASE

            WHEN b.Score >= 2.00 THEN 1.00
            WHEN b.Score >= 1.70 THEN 0.95
            WHEN b.Score >= 1.50 THEN 0.90
            WHEN b.Score >= 1.25 THEN 0.80
            WHEN b.Score >= 1.00 THEN 0.70
            ELSE 0.60

        END,

    location_method =
        CASE

            WHEN b.CandidateCount = 1
                THEN 'rule_single'

            WHEN b.SecondScore IS NOT NULL
             AND b.Score >= b.SecondScore * 1.5
                THEN 'rule_clear_winner'

            WHEN b.SecondScore IS NOT NULL
             AND ABS(b.Score - b.SecondScore) < 0.20
                THEN 'rule_conflict'

            ELSE 'rule_score'

        END,

    location_evidence =
        LEFT(b.Evidence,500),

    location_rule_version =
        'v5',

    location_classified_at =
        SYSUTCDATETIME()

FROM dbo.posters p
INNER JOIN BestMatch b
    ON p.PosterID = b.PosterID;
GO

-- Run the location update here.

CREATE VECTOR INDEX [VectorIndex-cosine]
    ON [dbo].[posters] ([desc_embbeded])
    WITH
    (
        TYPE = 'DISKANN',
        METRIC = 'COSINE'
    );
GO