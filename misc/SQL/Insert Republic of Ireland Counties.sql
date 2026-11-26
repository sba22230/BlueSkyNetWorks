-- Republic of Ireland counties
INSERT dbo.location_phrase
(
    phrase,
    country_code,
    jurisdiction,
    region,
    locality,
    evidence_type,
    evidence_weight,
    ambiguous_term
)
VALUES

-- Dublin
(N'dublin','IE',N'Republic of Ireland',N'Dublin',NULL,'bare_mention',0.70,0),
(N'county dublin','IE',N'Republic of Ireland',N'Dublin',NULL,'bare_mention',0.80,0),
(N'co dublin','IE',N'Republic of Ireland',N'Dublin',NULL,'bare_mention',0.80,0),

-- Cork
(N'cork','IE',N'Republic of Ireland',N'Cork',NULL,'bare_mention',0.60,1),
(N'county cork','IE',N'Republic of Ireland',N'Cork',NULL,'bare_mention',0.80,0),
(N'co cork','IE',N'Republic of Ireland',N'Cork',NULL,'bare_mention',0.80,0),

-- Galway
(N'galway','IE',N'Republic of Ireland',N'Galway',NULL,'bare_mention',0.70,0),
(N'county galway','IE',N'Republic of Ireland',N'Galway',NULL,'bare_mention',0.80,0),
(N'co galway','IE',N'Republic of Ireland',N'Galway',NULL,'bare_mention',0.80,0),

-- Kerry
(N'kerry','IE',N'Republic of Ireland',N'Kerry',NULL,'bare_mention',0.50,1),
(N'county kerry','IE',N'Republic of Ireland',N'Kerry',NULL,'bare_mention',0.80,0),
(N'co kerry','IE',N'Republic of Ireland',N'Kerry',NULL,'bare_mention',0.80,0),

-- Clare
(N'clare','IE',N'Republic of Ireland',N'Clare',NULL,'bare_mention',0.50,1),
(N'county clare','IE',N'Republic of Ireland',N'Clare',NULL,'bare_mention',0.80,0),
(N'co clare','IE',N'Republic of Ireland',N'Clare',NULL,'bare_mention',0.80,0),

-- Limerick
(N'limerick','IE',N'Republic of Ireland',N'Limerick',NULL,'bare_mention',0.70,0),
(N'county limerick','IE',N'Republic of Ireland',N'Limerick',NULL,'bare_mention',0.80,0),
(N'co limerick','IE',N'Republic of Ireland',N'Limerick',NULL,'bare_mention',0.80,0),

-- Waterford
(N'waterford','IE',N'Republic of Ireland',N'Waterford',NULL,'bare_mention',0.70,0),
(N'county waterford','IE',N'Republic of Ireland',N'Waterford',NULL,'bare_mention',0.80,0),
(N'co waterford','IE',N'Republic of Ireland',N'Waterford',NULL,'bare_mention',0.80,0),

-- Kilkenny
(N'kilkenny','IE',N'Republic of Ireland',N'Kilkenny',NULL,'bare_mention',0.70,0),
(N'county kilkenny','IE',N'Republic of Ireland',N'Kilkenny',NULL,'bare_mention',0.80,0),
(N'co kilkenny','IE',N'Republic of Ireland',N'Kilkenny',NULL,'bare_mention',0.80,0),

-- Mayo
(N'mayo','IE',N'Republic of Ireland',N'Mayo',NULL,'bare_mention',0.40,1),
(N'county mayo','IE',N'Republic of Ireland',N'Mayo',NULL,'bare_mention',0.80,0),
(N'co mayo','IE',N'Republic of Ireland',N'Mayo',NULL,'bare_mention',0.80,0),

-- Donegal
(N'donegal','IE',N'Republic of Ireland',N'Donegal',NULL,'bare_mention',0.70,0),
(N'county donegal','IE',N'Republic of Ireland',N'Donegal',NULL,'bare_mention',0.80,0),
(N'co donegal','IE',N'Republic of Ireland',N'Donegal',NULL,'bare_mention',0.80,0),

-- Wexford
(N'wexford','IE',N'Republic of Ireland',N'Wexford',NULL,'bare_mention',0.70,0),
(N'county wexford','IE',N'Republic of Ireland',N'Wexford',NULL,'bare_mention',0.80,0),
(N'co wexford','IE',N'Republic of Ireland',N'Wexford',NULL,'bare_mention',0.80,0),

-- Wicklow
(N'wicklow','IE',N'Republic of Ireland',N'Wicklow',NULL,'bare_mention',0.70,0),
(N'county wicklow','IE',N'Republic of Ireland',N'Wicklow',NULL,'bare_mention',0.80,0),
(N'co wicklow','IE',N'Republic of Ireland',N'Wicklow',NULL,'bare_mention',0.80,0),

-- Meath
(N'meath','IE',N'Republic of Ireland',N'Meath',NULL,'bare_mention',0.70,0),
(N'county meath','IE',N'Republic of Ireland',N'Meath',NULL,'bare_mention',0.80,0),
(N'co meath','IE',N'Republic of Ireland',N'Meath',NULL,'bare_mention',0.80,0),

-- Monaghan
(N'monaghan','IE',N'Republic of Ireland',N'Monaghan',NULL,'bare_mention',0.70,0),
(N'county monaghan','IE',N'Republic of Ireland',N'Monaghan',NULL,'bare_mention',0.80,0),
(N'co monaghan','IE',N'Republic of Ireland',N'Monaghan',NULL,'bare_mention',0.80,0),

-- Cavan
(N'cavan','IE',N'Republic of Ireland',N'Cavan',NULL,'bare_mention',0.40,1),
(N'county cavan','IE',N'Republic of Ireland',N'Cavan',NULL,'bare_mention',0.80,0),
(N'co cavan','IE',N'Republic of Ireland',N'Cavan',NULL,'bare_mention',0.80,0),

-- Laois
(N'laois','IE',N'Republic of Ireland',N'Laois',NULL,'bare_mention',0.70,0),
(N'county laois','IE',N'Republic of Ireland',N'Laois',NULL,'bare_mention',0.80,0),
(N'co laois','IE',N'Republic of Ireland',N'Laois',NULL,'bare_mention',0.80,0),

-- Longford
(N'longford','IE',N'Republic of Ireland',N'Longford',NULL,'bare_mention',0.70,0),
(N'county longford','IE',N'Republic of Ireland',N'Longford',NULL,'bare_mention',0.80,0),
(N'co longford','IE',N'Republic of Ireland',N'Longford',NULL,'bare_mention',0.80,0),

-- Leitrim
(N'leitrim','IE',N'Republic of Ireland',N'Leitrim',NULL,'bare_mention',0.70,0),
(N'county leitrim','IE',N'Republic of Ireland',N'Leitrim',NULL,'bare_mention',0.80,0),
(N'co leitrim','IE',N'Republic of Ireland',N'Leitrim',NULL,'bare_mention',0.80,0),

-- Roscommon
(N'roscommon','IE',N'Republic of Ireland',N'Roscommon',NULL,'bare_mention',0.70,0),
(N'county roscommon','IE',N'Republic of Ireland',N'Roscommon',NULL,'bare_mention',0.80,0),
(N'co roscommon','IE',N'Republic of Ireland',N'Roscommon',NULL,'bare_mention',0.80,0),

-- Offaly
(N'offaly','IE',N'Republic of Ireland',N'Offaly',NULL,'bare_mention',0.70,0),
(N'county offaly','IE',N'Republic of Ireland',N'Offaly',NULL,'bare_mention',0.80,0),
(N'co offaly','IE',N'Republic of Ireland',N'Offaly',NULL,'bare_mention',0.80,0),

-- Westmeath
(N'westmeath','IE',N'Republic of Ireland',N'Westmeath',NULL,'bare_mention',0.70,0),
(N'county westmeath','IE',N'Republic of Ireland',N'Westmeath',NULL,'bare_mention',0.80,0),
(N'co westmeath','IE',N'Republic of Ireland',N'Westmeath',NULL,'bare_mention',0.80,0),

-- Louth
(N'louth','IE',N'Republic of Ireland',N'Louth',NULL,'bare_mention',0.70,0),
(N'county louth','IE',N'Republic of Ireland',N'Louth',NULL,'bare_mention',0.80,0),
(N'co louth','IE',N'Republic of Ireland',N'Louth',NULL,'bare_mention',0.80,0),

-- Tipperary
(N'tipperary','IE',N'Republic of Ireland',N'Tipperary',NULL,'bare_mention',0.70,0),
(N'county tipperary','IE',N'Republic of Ireland',N'Tipperary',NULL,'bare_mention',0.80,0),
(N'co tipperary','IE',N'Republic of Ireland',N'Tipperary',NULL,'bare_mention',0.80,0),

-- Carlow
(N'carlow','IE',N'Republic of Ireland',N'Carlow',NULL,'bare_mention',0.70,0),
(N'county carlow','IE',N'Republic of Ireland',N'Carlow',NULL,'bare_mention',0.80,0),
(N'co carlow','IE',N'Republic of Ireland',N'Carlow',NULL,'bare_mention',0.80,0),

-- Kildare
(N'kildare','IE',N'Republic of Ireland',N'Kildare',NULL,'bare_mention',0.70,0),
(N'county kildare','IE',N'Republic of Ireland',N'Kildare',NULL,'bare_mention',0.80,0),
(N'co kildare','IE',N'Republic of Ireland',N'Kildare',NULL,'bare_mention',0.80,0);