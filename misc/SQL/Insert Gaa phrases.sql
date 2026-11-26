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

(N'mayo gaa','IE','Republic of Ireland','Mayo',NULL,'gaa',0.75,0),
(N'cork gaa','IE','Republic of Ireland','Cork',NULL,'gaa',0.75,0),
(N'dublin gaa','IE','Republic of Ireland','Dublin',NULL,'gaa',0.75,0),
(N'galway gaa','IE','Republic of Ireland','Galway',NULL,'gaa',0.75,0),
(N'kerry gaa','IE','Republic of Ireland','Kerry',NULL,'gaa',0.75,0),
(N'donegal gaa','IE','Republic of Ireland','Donegal',NULL,'gaa',0.75,0),
(N'tyrone gaa','GB','Northern Ireland','Tyrone',NULL,'gaa',0.75,0),
(N'derry gaa','GB','Northern Ireland','Derry',NULL,'gaa',0.75,0),
(N'armagh gaa','GB','Northern Ireland','Armagh',NULL,'gaa',0.75,0),
(N'down gaa','GB','Northern Ireland','Down',NULL,'gaa',0.75,0);