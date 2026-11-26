USE [BlueSkyNet];
GO

DROP INDEX [VectorIndex-cosine] ON [dbo].[posters];
GO

BEGIN TRY
    BEGIN TRANSACTION;

    WITH matched_location AS
    (
        SELECT
            p.PosterID,
            lp.location_phrase_id,
            lp.phrase,
            lp.country_code,
            lp.jurisdiction,
            lp.region,
            lp.locality,
            lp.evidence_type,
            lp.evidence_weight
        FROM dbo.posters AS p
        OUTER APPLY
        (
            SELECT TOP (1)
                lp.location_phrase_id,
                lp.phrase,
                lp.country_code,
                lp.jurisdiction,
                lp.region,
                lp.locality,
                lp.evidence_type,
                lp.evidence_weight
            FROM dbo.location_phrase AS lp
            WHERE lp.active = 1
              AND CHARINDEX(
                      LOWER(LTRIM(RTRIM(lp.phrase))),
                      LOWER(COALESCE(p.description, N''))
                  ) > 0
            ORDER BY
                lp.ambiguous_term ASC,
                lp.evidence_weight DESC,
                LEN(lp.phrase) DESC,
                lp.location_phrase_id ASC
        ) AS lp
    )
    UPDATE p
    SET
        p.location_country_code = ml.country_code,
        p.location_jurisdiction = ml.jurisdiction,
        p.location_region = ml.region,
        p.location_locality = ml.locality,
        p.location_status = CASE
            WHEN ml.location_phrase_id IS NULL THEN 'unknown'
            WHEN ml.evidence_type = 'negative' THEN 'outside_ireland'
            WHEN ml.country_code IN ('IE', 'GB') THEN 'in_ireland'
            ELSE 'outside_ireland'
        END,
        p.location_confidence = ml.evidence_weight,
        p.location_method = CASE
            WHEN ml.location_phrase_id IS NULL THEN 'phrase_table'
            ELSE ml.evidence_type
        END,
        p.location_evidence = CASE
            WHEN ml.location_phrase_id IS NULL THEN NULL
            ELSE ml.phrase
        END,
        p.location_rule_version = 'location_phrase-v1',
        p.location_model_version = 'rule-table',
        p.location_classified_at = SYSDATETIME()
    FROM dbo.posters AS p
    LEFT JOIN matched_location AS ml
        ON ml.PosterID = p.PosterID;

    COMMIT TRANSACTION;
END TRY
BEGIN CATCH
    IF @@TRANCOUNT > 0
        ROLLBACK TRANSACTION;

    THROW;
END CATCH;
GO

SELECT
    location_status,
    location_country_code,
    location_jurisdiction,
    location_region,
    location_locality,
    COUNT(*) AS poster_count
FROM dbo.posters
GROUP BY
    location_status,
    location_country_code,
    location_jurisdiction,
    location_region,
    location_locality
ORDER BY poster_count DESC;
-- Run the location update here.

CREATE VECTOR INDEX [VectorIndex-cosine]
    ON [dbo].[posters] ([desc_embbeded])
    WITH
    (
        TYPE = 'DISKANN',
        METRIC = 'COSINE'
    );
GO