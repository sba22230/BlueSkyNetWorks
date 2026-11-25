
EXECUTE sp_configure 'show advanced options', 1;
RECONFIGURE WITH OVERRIDE;
EXECUTE sp_configure 'external AI runtimes enabled', 1;
RECONFIGURE WITH OVERRIDE;
EXECUTE sp_configure 'external rest endpoint enabled', 1;
RECONFIGURE WITH OVERRIDE;
ALTER DATABASE SCOPED CONFIGURATION
SET PREVIEW_FEATURES = ON;

DROP EXTERNAL MODEL DescEmbeddings;

CREATE EXTERNAL MODEL DescEmbeddings
WITH (
    LOCATION = 'L:\onnx_runtime\model\all-MiniLM-L6-v2-onnx',
    API_FORMAT = 'ONNX Runtime',
    MODEL_TYPE = EMBEDDINGS,
    MODEL = 'allMiniLM',
    PARAMETERS = '{"valid":"JSON"}',
    LOCAL_RUNTIME_PATH = 'L:\onnx_runtime\'
);

SELECT TOP (1)
    t.description,
    AI_GENERATE_EMBEDDINGS(
        t.description USE MODEL DescEmbeddings
    ) AS embedding
FROM dbo.reposts_raw AS t
WHERE NULLIF(LTRIM(RTRIM(t.description)), N'') IS NOT NULL;

SELECT
    DESCRIPTION,
    AI_GENERATE_EMBEDDINGS(
        DESCRIPTION USE MODEL DescEmbeddings
    ) AS Embedding
FROM dbo.reposts_raw
WHERE NULLIF(LTRIM(RTRIM(DESCRIPTION)), N'') IS NOT NULL;

UPDATE t
    SET [desc_embedded] = AI_GENERATE_EMBEDDINGS(t.description USE MODEL DescEmbeddings)
FROM dbo.reposts_raw AS t
WHERE NULLIF(LTRIM(RTRIM(DESCRIPTION)), N'') IS NOT NULL 
AND t.desc_embedded IS NULL;



SELECT
    *
FROM sys.external_models
WHERE name = N'DescEmbeddings';

SELECT
    p.name AS PackageName,
    o.name AS EventName,
    o.description
FROM sys.dm_xe_objects AS o
JOIN sys.dm_xe_packages AS p
    ON o.package_guid = p.guid
WHERE o.object_type = N'event'
  AND o.name IN (
      N'ai_generate_embeddings_summary',
      N'external_rest_endpoint_summary'
  );

CREATE EVENT SESSION [AI_Embeddings_Diagnostics]
ON SERVER
ADD EVENT sqlserver.ai_generate_embeddings_summary,
ADD EVENT sqlserver.external_rest_endpoint_summary
ADD TARGET package0.ring_buffer
(
    SET max_memory = 4096
)
WITH
(
    MAX_DISPATCH_LATENCY = 1 SECONDS
);
GO

ALTER EVENT SESSION [AI_Embeddings_Diagnostics]
ON SERVER
STATE = START;
GO

SELECT
    CAST(xet.target_data AS XML) AS EventData
FROM sys.dm_xe_sessions AS s
JOIN sys.dm_xe_session_targets AS xet
    ON s.address = xet.event_session_address
WHERE s.name = N'AI_Embeddings_Diagnostics'
  AND xet.target_name = N'ring_buffer';

ALTER EVENT SESSION [AI_Embeddings_Diagnostics]
ON SERVER
STATE = STOP;