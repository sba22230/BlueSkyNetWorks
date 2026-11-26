USE [BlueSkyNet]
GO

/****** Object:  Table [dbo].[location_phrase]    Script Date: 26/11/2026 15:55:24 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[location_phrase](
	[location_phrase_id] [int] IDENTITY(1,1) NOT NULL,
	[phrase] [nvarchar](200) NOT NULL,
	[country_code] [char](2) NULL,
	[jurisdiction] [nvarchar](50) NULL,
	[region] [nvarchar](100) NULL,
	[locality] [nvarchar](100) NULL,
	[evidence_type] [varchar](20) NOT NULL,
	[evidence_weight] [decimal](5, 4) NOT NULL,
	[ambiguous_term] [bit] NOT NULL,
	[active] [bit] NOT NULL,
	[phrase_embedding] [vector](384, float32) NULL,
PRIMARY KEY CLUSTERED 
(
	[location_phrase_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [dataonly]
) ON [dataonly]
GO

ALTER TABLE [dbo].[location_phrase] ADD  DEFAULT ((0)) FOR [ambiguous_term]
GO

ALTER TABLE [dbo].[location_phrase] ADD  DEFAULT ((1)) FOR [active]
GO

ALTER TABLE [dbo].[location_phrase]  WITH CHECK ADD  CONSTRAINT [CK_location_phrase_evidence_type] CHECK  (([evidence_type]='bare_mention' OR [evidence_type]='negative' OR [evidence_type]='origin' OR [evidence_type]='current' OR [evidence_type]='institution' OR [evidence_type]='gaa' OR [evidence_type]='national_signal'))
GO

ALTER TABLE [dbo].[location_phrase] CHECK CONSTRAINT [CK_location_phrase_evidence_type]
GO

