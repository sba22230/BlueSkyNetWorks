-- Northern Ireland Counties 
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
(N'county antrim','GB',N'Northern Ireland',N'Antrim',NULL,'bare_mention',0.80,0),
(N'co antrim','GB',N'Northern Ireland',N'Antrim',NULL,'bare_mention',0.80,0),

(N'county armagh','GB',N'Northern Ireland',N'Armagh',NULL,'bare_mention',0.80,0),
(N'co armagh','GB',N'Northern Ireland',N'Armagh',NULL,'bare_mention',0.80,0),

(N'county down','GB',N'Northern Ireland',N'Down',NULL,'bare_mention',0.80,0),
(N'co down','GB',N'Northern Ireland',N'Down',NULL,'bare_mention',0.80,0),

(N'county fermanagh','GB',N'Northern Ireland',N'Fermanagh',NULL,'bare_mention',0.80,0),
(N'co fermanagh','GB',N'Northern Ireland',N'Fermanagh',NULL,'bare_mention',0.80,0),

(N'county londonderry','GB',N'Northern Ireland',N'Derry',NULL,'bare_mention',0.80,0),
(N'co londonderry','GB',N'Northern Ireland',N'Derry',NULL,'bare_mention',0.80,0),

(N'county tyrone','GB',N'Northern Ireland',N'Tyrone',NULL,'bare_mention',0.80,0),
(N'co tyrone','GB',N'Northern Ireland',N'Tyrone',NULL,'bare_mention',0.80,0);