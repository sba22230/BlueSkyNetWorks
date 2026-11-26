USE [BlueSkyNet]
GO

/****** Object:  Table [dbo].[posters]    Script Date: 26/11/2026 15:27:05 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[posters](
	[did] [nvarchar](255) NOT NULL,
	[handle] [nvarchar](255) NOT NULL,
	[description] [nvarchar](max) NOT NULL,
	[desc_embbeded] [vector](384, float32) NOT NULL,
	[PosterID] [int] IDENTITY(1,1) NOT NULL,
	[location_country_code] [char](2) NULL,
	[location_jurisdiction] [nvarchar](50) NULL,
	[location_region] [nvarchar](100) NULL,
	[location_locality] [nvarchar](100) NULL,
	[location_status] [varchar](20) NULL,
	[location_confidence] [decimal](5, 4) NULL,
	[location_method] [varchar](30) NULL,
	[location_evidence] [nvarchar](500) NULL,
	[location_rule_version] [varchar](20) NULL,
	[location_model_version] [varchar](100) NULL,
	[location_classified_at] [datetime2](0) NULL,
 CONSTRAINT [PK_posters] PRIMARY KEY CLUSTERED 
(
	[PosterID] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [dataonly],
 CONSTRAINT [UX_posters_did] UNIQUE NONCLUSTERED 
(
	[did] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [dataonly]
) ON [dataonly] TEXTIMAGE_ON [dataonly]
GO

