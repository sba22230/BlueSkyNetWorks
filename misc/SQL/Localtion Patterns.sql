DECLARE @IrelandLocations TABLE
(
    CountryCode    char(2),
    Jurisdiction   nvarchar(50),
    RegionName     nvarchar(100)
);

INSERT INTO @IrelandLocations
VALUES

-- Republic of Ireland (26)

('IE','Republic of Ireland','Carlow'),
('IE','Republic of Ireland','Cavan'),
('IE','Republic of Ireland','Clare'),
('IE','Republic of Ireland','Cork'),
('IE','Republic of Ireland','Donegal'),
('IE','Republic of Ireland','Dublin'),
('IE','Republic of Ireland','Galway'),
('IE','Republic of Ireland','Kerry'),
('IE','Republic of Ireland','Kildare'),
('IE','Republic of Ireland','Kilkenny'),
('IE','Republic of Ireland','Laois'),
('IE','Republic of Ireland','Leitrim'),
('IE','Republic of Ireland','Limerick'),
('IE','Republic of Ireland','Longford'),
('IE','Republic of Ireland','Louth'),
('IE','Republic of Ireland','Mayo'),
('IE','Republic of Ireland','Meath'),
('IE','Republic of Ireland','Monaghan'),
('IE','Republic of Ireland','Offaly'),
('IE','Republic of Ireland','Roscommon'),
('IE','Republic of Ireland','Sligo'),
('IE','Republic of Ireland','Tipperary'),
('IE','Republic of Ireland','Waterford'),
('IE','Republic of Ireland','Westmeath'),
('IE','Republic of Ireland','Wexford'),
('IE','Republic of Ireland','Wicklow'),

-- Northern Ireland (6)

('GB','Northern Ireland','Antrim'),
('GB','Northern Ireland','Armagh'),
('GB','Northern Ireland','Down'),
('GB','Northern Ireland','Fermanagh'),
('GB','Northern Ireland','Derry'),
('GB','Northern Ireland','Tyrone');

DECLARE @Patterns TABLE
(
    PatternTemplate nvarchar(100),
    EvidenceType varchar(20),
    Weight decimal(5,2)
);

INSERT INTO @Patterns
VALUES
('based in %s','current',0.99),
('living in %s','current',0.99),
('lives in %s','current',0.99),
('resident of %s','current',0.99),
('%s resident','current',0.98),
('%s based','current',0.98),

('from %s','origin',0.65),
('born in %s','origin',0.45),
('%s born','origin',0.45),
('raised in %s','origin',0.45),

('%s','bare_mention',0.70),
('county %s','bare_mention',0.80),
('co %s','bare_mention',0.80);



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
    LOWER(REPLACE(p.PatternTemplate,'%s',l.RegionName)),
    l.CountryCode,
    l.Jurisdiction,
    l.RegionName,
    NULL,
    p.EvidenceType,
    p.Weight,

    CASE
        WHEN l.RegionName IN
        (
            'Mayo',
            'Clare',
            'Kerry',
            'Cavan',
            'Cork'
        )
        AND p.EvidenceType = 'bare_mention'
        THEN 1
        ELSE 0
    END
FROM @IrelandLocations l
CROSS JOIN @Patterns p;