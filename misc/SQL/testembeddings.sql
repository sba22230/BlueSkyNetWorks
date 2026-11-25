-- Skips rows currently locked by another transaction
-- Function call 1:50, 1:04; 1:00
SELECT  TOP (100) did, handle, description, dbo.GenerateDescriptionEmbedding(description) AS Expr1
FROM      reposts_raw
WHERE   (NULLIF (LTRIM(RTRIM(description)), N'') IS NOT NULL)

-- Proc call 1;49, 1:05, 1:00
SET NOCOUNT ON;

DECLARE @Description nvarchar(max);

DECLARE description_cursor CURSOR LOCAL FAST_FORWARD READ_ONLY
FOR
    SELECT TOP (100) r.did,  r.description
    FROM dbo.reposts_raw AS r
    WHERE NULLIF(LTRIM(RTRIM(r.description)), N'') IS NOT NULL;

OPEN description_cursor;

FETCH NEXT FROM description_cursor INTO @Description;

WHILE @@FETCH_STATUS = 0
BEGIN
    EXEC dbo.GetDescriptionEmbedding @Description = @Description;
    
    FETCH NEXT FROM description_cursor INTO @Description;
END;

CLOSE description_cursor;
DEALLOCATE description_cursor;