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

(N'🇮🇪','IE','Republic of Ireland',NULL,NULL,'national_signal',0.45,0),
(N'irish','IE','Republic of Ireland',NULL,NULL,'national_signal',0.30,1),
(N'gaeilge','IE','Republic of Ireland',NULL,NULL,'national_signal',0.40,0),
(N'as gaeilge','IE','Republic of Ireland',NULL,NULL,'national_signal',0.45,0);