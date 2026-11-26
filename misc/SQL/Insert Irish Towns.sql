DECLARE @IrishTowns TABLE
(
    CountryCode    char(2),
    Jurisdiction   nvarchar(50),
    RegionName     nvarchar(100),
    LocalityName   nvarchar(100)
);

INSERT INTO @IrishTowns
VALUES

-- Dublin

('IE','Republic of Ireland','Dublin','Dublin'),
('IE','Republic of Ireland','Dublin','Swords'),
('IE','Republic of Ireland','Dublin','Malahide'),
('IE','Republic of Ireland','Dublin','Howth'),
('IE','Republic of Ireland','Dublin','Balbriggan'),
('IE','Republic of Ireland','Dublin','Blanchardstown'),
('IE','Republic of Ireland','Dublin','Tallaght'),
('IE','Republic of Ireland','Dublin','Dundrum'),
('IE','Republic of Ireland','Dublin','Dun Laoghaire'),
('IE','Republic of Ireland','Dublin','Blackrock'),
('IE','Republic of Ireland','Dublin','Skerries'),

-- Wicklow

('IE','Republic of Ireland','Wicklow','Bray'),
('IE','Republic of Ireland','Wicklow','Greystones'),
('IE','Republic of Ireland','Wicklow','Wicklow'),
('IE','Republic of Ireland','Wicklow','Arklow'),

-- Kildare

('IE','Republic of Ireland','Kildare','Naas'),
('IE','Republic of Ireland','Kildare','Newbridge'),
('IE','Republic of Ireland','Kildare','Maynooth'),
('IE','Republic of Ireland','Kildare','Celbridge'),
('IE','Republic of Ireland','Kildare','Leixlip'),

-- Meath

('IE','Republic of Ireland','Meath','Navan'),
('IE','Republic of Ireland','Meath','Trim'),
('IE','Republic of Ireland','Meath','Ashbourne'),
('IE','Republic of Ireland','Meath','Dunshaughlin'),
('IE','Republic of Ireland','Meath','Kells'),

-- Louth

('IE','Republic of Ireland','Louth','Drogheda'),
('IE','Republic of Ireland','Louth','Dundalk'),
('IE','Republic of Ireland','Louth','Ardee'),

-- Cork

('IE','Republic of Ireland','Cork','Cork'),
('IE','Republic of Ireland','Cork','Cobh'),
('IE','Republic of Ireland','Cork','Midleton'),
('IE','Republic of Ireland','Cork','Mallow'),
('IE','Republic of Ireland','Cork','Clonakilty'),
('IE','Republic of Ireland','Cork','Skibbereen'),
('IE','Republic of Ireland','Cork','Kinsale'),
('IE','Republic of Ireland','Cork','Bantry'),
('IE','Republic of Ireland','Cork','Youghal'),

-- Galway

('IE','Republic of Ireland','Galway','Galway'),
('IE','Republic of Ireland','Galway','Tuam'),
('IE','Republic of Ireland','Galway','Ballinasloe'),
('IE','Republic of Ireland','Galway','Loughrea'),
('IE','Republic of Ireland','Galway','Oranmore'),

-- Limerick

('IE','Republic of Ireland','Limerick','Limerick'),
('IE','Republic of Ireland','Limerick','Newcastle West'),

-- Clare

('IE','Republic of Ireland','Clare','Ennis'),
('IE','Republic of Ireland','Clare','Shannon'),
('IE','Republic of Ireland','Clare','Kilrush'),

-- Kerry

('IE','Republic of Ireland','Kerry','Tralee'),
('IE','Republic of Ireland','Kerry','Killarney'),
('IE','Republic of Ireland','Kerry','Listowel'),
('IE','Republic of Ireland','Kerry','Dingle'),

-- Mayo

('IE','Republic of Ireland','Mayo','Castlebar'),
('IE','Republic of Ireland','Mayo','Ballina'),
('IE','Republic of Ireland','Mayo','Westport'),

-- Sligo

('IE','Republic of Ireland','Sligo','Sligo'),

-- Donegal

('IE','Republic of Ireland','Donegal','Letterkenny'),
('IE','Republic of Ireland','Donegal','Buncrana'),
('IE','Republic of Ireland','Donegal','Donegal'),

-- Waterford

('IE','Republic of Ireland','Waterford','Waterford'),
('IE','Republic of Ireland','Waterford','Dungarvan'),

-- Kilkenny

('IE','Republic of Ireland','Kilkenny','Kilkenny'),

-- Wexford

('IE','Republic of Ireland','Wexford','Wexford'),
('IE','Republic of Ireland','Wexford','Enniscorthy'),
('IE','Republic of Ireland','Wexford','Gorey'),
('IE','Republic of Ireland','Wexford','New Ross'),

-- Tipperary

('IE','Republic of Ireland','Tipperary','Clonmel'),
('IE','Republic of Ireland','Tipperary','Nenagh'),
('IE','Republic of Ireland','Tipperary','Thurles'),

-- Westmeath

('IE','Republic of Ireland','Westmeath','Athlone'),
('IE','Republic of Ireland','Westmeath','Mullingar'),

-- Offaly

('IE','Republic of Ireland','Offaly','Tullamore'),
('IE','Republic of Ireland','Offaly','Birr'),

-- Laois

('IE','Republic of Ireland','Laois','Portlaoise'),

-- Roscommon

('IE','Republic of Ireland','Roscommon','Roscommon'),
('IE','Republic of Ireland','Roscommon','Boyle'),

-- Monaghan

('IE','Republic of Ireland','Monaghan','Monaghan'),
('IE','Republic of Ireland','Monaghan','Carrickmacross'),

-- Cavan

('IE','Republic of Ireland','Cavan','Cavan'),

-- Leitrim

('IE','Republic of Ireland','Leitrim','Carrick on Shannon'),

-- Carlow

('IE','Republic of Ireland','Carlow','Carlow'),

-- Longford

('IE','Republic of Ireland','Longford','Longford'),

-- Northern Ireland

('GB','Northern Ireland','Antrim','Belfast'),
('GB','Northern Ireland','Antrim','Lisburn'),
('GB','Northern Ireland','Antrim','Ballymena'),
('GB','Northern Ireland','Antrim','Carrickfergus'),
('GB','Northern Ireland','Armagh','Armagh'),
('GB','Northern Ireland','Down','Bangor'),
('GB','Northern Ireland','Down','Newtownards'),
('GB','Northern Ireland','Down','Newry'),
('GB','Northern Ireland','Derry','Derry'),
('GB','Northern Ireland','Tyrone','Omagh'),
('GB','Northern Ireland','Fermanagh','Enniskillen');

DECLARE @Patterns TABLE
(
    PatternTemplate nvarchar(100),
    EvidenceType varchar(20),
    Weight decimal(5,2)
);
INSERT INTO @Patterns
VALUES
('%s ireland','current',0.95),
('%s, ireland','current',0.95),
('based near %s','current',0.90),
('living near %s','current',0.90),
('from %s ireland','origin',0.70),
('originally from %s','origin',0.70),
('native of %s','origin',0.70);

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
SELECT
    LOWER(REPLACE(p.PatternTemplate,'%s',t.LocalityName)),
    t.CountryCode,
    t.Jurisdiction,
    t.RegionName,
    t.LocalityName,
    p.EvidenceType,
    p.Weight,
    0
FROM @IrishTowns t
CROSS JOIN @Patterns p;