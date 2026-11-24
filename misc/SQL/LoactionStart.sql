WITH profile AS
(
    SELECT
        handle,
        LOWER(LTRIM(RTRIM(COALESCE(description, N'')))) AS description
    FROM dbo.reposts_raw
),
handle_profile AS
(
    SELECT
        handle,
        STRING_AGG(description, N' ') WITHIN GROUP (ORDER BY description)
            AS description_text
    FROM profile
    WHERE NULLIF(LTRIM(RTRIM(description)), N'') IS NOT NULL
    GROUP BY handle
),
classified AS
(
    SELECT
        handle,
        description_text,
        CASE
            WHEN description_text LIKE '%not in ireland%'
              OR description_text LIKE '%not living in ireland%'
              OR description_text LIKE '%not from ireland%'
              OR description_text LIKE '%outside ireland%'
              OR description_text LIKE '%based outside ireland%'
              OR description_text LIKE '%living outside ireland%'
            THEN 1 ELSE 0
        END AS explicitly_not_in_ireland,
        CASE
            WHEN description_text LIKE '%belfast%'
              OR description_text LIKE '%northern ireland%'
              OR description_text LIKE '%londonderry%'
              OR description_text LIKE '%derry%'
              OR description_text LIKE '%county antrim%'
              OR description_text LIKE '%county armagh%'
              OR description_text LIKE '%county down%'
              OR description_text LIKE '%county fermanagh%'
              OR description_text LIKE '%county tyrone%'
            THEN 1 ELSE 0
        END AS northern_ireland_flag,
        CASE
            WHEN description_text LIKE '%dublin%'
              OR description_text LIKE '%cork%'
              OR description_text LIKE '%galway%'
              OR description_text LIKE '%limerick%'
              OR description_text LIKE '%kilkenny%'
              OR description_text LIKE '%waterford%'
              OR description_text LIKE '%kerry%'
              OR description_text LIKE '%clare%'
              OR description_text LIKE '%sligo%'
              OR description_text LIKE '%mayo%'
              OR description_text LIKE '%donegal%'
              OR description_text LIKE '%wicklow%'
              OR description_text LIKE '%wexford%'
              OR description_text LIKE '%meath%'
              OR description_text LIKE '%monaghan%'
              OR description_text LIKE '%cavan%'
              OR description_text LIKE '%laois%'
              OR description_text LIKE '%longford%'
              OR description_text LIKE '%leitrim%'
              OR description_text LIKE '%roscommon%'
              OR description_text LIKE '%tipperary%'
              OR description_text LIKE '%offaly%'
              OR description_text LIKE '%westmeath%'
              OR description_text LIKE '%louth%'
              OR description_text LIKE '%republic of ireland%'
              OR description_text LIKE '%based in ireland%'
              OR description_text LIKE '%living in ireland%'
              OR description_text LIKE '%lives in ireland%'
            THEN 1 ELSE 0
        END AS republic_location_flag,
        CASE
            WHEN description_text LIKE '%based in england%'
              OR description_text LIKE '%living in england%'
              OR description_text LIKE '%based in scotland%'
              OR description_text LIKE '%living in scotland%'
              OR description_text LIKE '%based in wales%'
              OR description_text LIKE '%living in wales%'
              OR description_text LIKE '%united kingdom%'
              OR description_text LIKE '% london%'
              OR description_text LIKE '%manchester%'
              OR description_text LIKE '%edinburgh%'
              OR description_text LIKE '%glasgow%'
              OR description_text LIKE '%cardiff%'
              OR description_text LIKE '%based in usa%'
              OR description_text LIKE '%based in canada%'
              OR description_text LIKE '%based in australia%'
              OR description_text LIKE '%based in france%'
              OR description_text LIKE '%based in germany%'
              OR description_text LIKE '%based in spain%'
              OR description_text LIKE '%based in italy%'
            THEN 1 ELSE 0
        END AS other_location_flag
    FROM handle_profile
),
located AS
(
    SELECT
        handle,
        description_text,
        CASE
            WHEN explicitly_not_in_ireland = 1
                THEN N'Not in Ireland'
            WHEN northern_ireland_flag = 1
                THEN N'In Ireland'
            WHEN republic_location_flag = 1
                THEN N'In Ireland'
            WHEN other_location_flag = 1
                THEN N'Not in Ireland'
            ELSE N'No location found'
        END AS location_result,
        CASE
            WHEN explicitly_not_in_ireland = 1
                THEN N'Explicit statement of living outside Ireland'
            WHEN northern_ireland_flag = 1
                THEN N'Northern Ireland location mentioned'
            WHEN republic_location_flag = 1
                THEN N'Republic of Ireland location mentioned'
            WHEN other_location_flag = 1
                THEN N'Location outside Ireland mentioned'
            ELSE N'No location could be inferred'
        END AS location_reason
    FROM classified
)
SELECT
    location_result,
    handle,
    location_reason,
    description_text
FROM located

ORDER BY
    CASE location_result
        WHEN N'In Ireland' THEN 1
        WHEN N'Not in Ireland' THEN 2
        ELSE 3
    END,
    handle;