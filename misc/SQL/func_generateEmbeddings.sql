USE [BlueSkyNet]
GO

/****** Object:  UserDefinedFunction [dbo].[GenerateDescriptionEmbedding]    Script Date: 25/11/2026 14:44:08 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE OR ALTER FUNCTION [dbo].[GenerateDescriptionEmbedding]
(
    @Description nvarchar(max)
)
RETURNS vector(384)
AS
BEGIN

    RETURN AI_GENERATE_EMBEDDINGS
    (
        @Description
        USE MODEL DescEmbeddings
    );

END
GO


