--Drop table if it already exists.
IF OBJECT_ID('posters','U') IS NOT NULL
    DROP TABLE dbo.posters
GO

--Create memory optimized table and indexes on the memory optimized table.
CREATE TABLE dbo.posters
(
	did nvarchar(255) NOT NULL PRIMARY KEY NONCLUSTERED,
	handle nvarchar(255) NOT NULL,
	description nvarchar(max)	 NOT NULL,
	desc_embbeded vector(384, float32) NOT NULL,


)  ON [dataonly] TEXTIMAGE_ON [dataonly]
GO