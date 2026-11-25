
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


SELECT AI_GENERATE_EMBEDDINGS
(
    (SELECT Top 1 description from reposts_raw)
    USE MODEL DescEmbeddings
);