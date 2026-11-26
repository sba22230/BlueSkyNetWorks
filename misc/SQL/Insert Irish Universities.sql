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

(N'trinity college dublin','IE','Republic of Ireland','Dublin','Dublin','institution',0.60,0),
(N'tcd','IE','Republic of Ireland','Dublin','Dublin','institution',0.50,1),

(N'university college dublin','IE','Republic of Ireland','Dublin','Dublin','institution',0.60,0),
(N'ucd','IE','Republic of Ireland','Dublin','Dublin','institution',0.50,1),

(N'dublin city university','IE','Republic of Ireland','Dublin','Dublin','institution',0.60,0),
(N'dcu','IE','Republic of Ireland','Dublin','Dublin','institution',0.50,1),

(N'technological university dublin','IE','Republic of Ireland','Dublin','Dublin','institution',0.60,0),
(N'tu dublin','IE','Republic of Ireland','Dublin','Dublin','institution',0.60,0),

-- Cork

(N'university college cork','IE','Republic of Ireland','Cork','Cork','institution',0.60,0),
(N'ucc','IE','Republic of Ireland','Cork','Cork','institution',0.50,1),

(N'munster technological university','IE','Republic of Ireland','Cork',NULL,'institution',0.55,0),
(N'mtu','IE','Republic of Ireland','Cork',NULL,'institution',0.40,1),

-- Galway

(N'university of galway','IE','Republic of Ireland','Galway','Galway','institution',0.60,0),
(N'nuig','IE','Republic of Ireland','Galway','Galway','institution',0.55,1),

-- Limerick

(N'university of limerick','IE','Republic of Ireland','Limerick','Limerick','institution',0.60,0),
(N'ul','IE','Republic of Ireland','Limerick','Limerick','institution',0.40,1),

-- Maynooth

(N'maynooth university','IE','Republic of Ireland','Kildare','Maynooth','institution',0.60,0),

-- Waterford

(N'south east technological university','IE','Republic of Ireland','Waterford',NULL,'institution',0.55,0),
(N'setu','IE','Republic of Ireland','Waterford',NULL,'institution',0.45,1),

-- Sligo

(N'atlantic technological university','IE','Republic of Ireland','Sligo',NULL,'institution',0.50,0),
(N'atu','IE','Republic of Ireland','Sligo',NULL,'institution',0.40,1);