

UPDATE dbo.location_phrase
SET phrase_embedding =
    dbo.GenerateDescriptionEmbedding(phrase)
WHERE phrase_embedding IS NULL;