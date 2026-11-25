USE [BlueSkyNet]
GO

/****** Object:  StoredProcedure [dbo].[GetDescriptionEmbedding]    Script Date: 25/11/2026 14:43:31 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE OR ALTER PROCEDURE [dbo].[GetDescriptionEmbedding]
(
    @Description nvarchar(max)
)
AS
BEGIN

    BEGIN TRY

        SELECT
            AI_GENERATE_EMBEDDINGS
            (
                @Description
                USE MODEL DescEmbeddings
            ) AS Embedding;

    END TRY

    BEGIN CATCH

        SELECT
            ERROR_NUMBER() AS ErrorNumber,
            ERROR_MESSAGE() AS ErrorMessage;

    END CATCH

END
GO


