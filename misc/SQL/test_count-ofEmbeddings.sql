SELECT count(*)  FROM [BlueSkyNet].[dbo].[posters]
where did = 'did:plc:2fkqf2w4ri7mvnali62fvets'

SELECT 
    did
    ,
    MAX(handle) AS handle,
    MAX(description) AS description

FROM dbo.reposts_raw 

WHERE NULLIF(LTRIM(RTRIM(description)), N'') IS NOT NULL
  AND NOT EXISTS
  (
      SELECT 1
      FROM dbo.posters p
      WHERE p.did = reposts_raw.did
  )
  AND did IN ('did:plc:27prm64ncc2fcdqfmtlsl26i', 'did:plc:2fkqf2w4ri7mvnali62fvets', 'did:plc:2knfwwwysmhzsxxan6b5nfc2')

  
GROUP BY did;