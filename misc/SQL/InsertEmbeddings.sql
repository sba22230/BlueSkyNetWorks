SET NOCOUNT ON;

IF OBJECT_ID('tempdb..#todo') IS NOT NULL
    DROP TABLE #todo;

SELECT
    did,
    MAX(handle) AS handle,
    MAX(description) AS description
INTO #todo
FROM dbo.reposts_raw
WHERE NULLIF(LTRIM(RTRIM(description)), N'') IS NOT NULL
  AND NOT EXISTS
  (
      SELECT 1
      FROM dbo.posters p
      WHERE p.did = reposts_raw.did
  )
GROUP BY did;

select count(*) as rows_to_insert from #todo;

DECLARE @RowsInserted AS INT = 1;

WHILE @RowsInserted > 0
    BEGIN
        WITH Batch
        AS   (SELECT   TOP (200) *
              FROM     #todo
              WHERE NOT EXISTS
                      (
                          SELECT 1
                          FROM dbo.posters p
                          WHERE p.did = #todo.did
                      )
              ORDER BY did)
        INSERT INTO dbo.posters (
            did,
            handle,
            description,
            desc_embbeded
        )
        SELECT did,
               handle,
               description,
               dbo.GenerateDescriptionEmbedding(description)
        FROM   Batch;
        SET @RowsInserted = @@ROWCOUNT;
        PRINT CONCAT('Inserted ', @RowsInserted, ' rows');
    END