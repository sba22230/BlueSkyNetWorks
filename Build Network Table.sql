SELECT 
P.uri AS Post , 
P.author_handle AS PostedBy,  
CAST(P.indexedAt AS date) AS PostedOn, P.text, 
P.bookmark_count, P.like_count, p.reply_count, P.repost_count,
RP.handle AS RepostedBy 
FROM posts_raw AS P
INNER JOIN reposts_raw as RP ON P.uri = RP.uri
WHERE p.uri = 'at://did:plc:36hqooiwcmgr3vfc4ovmi2bd/app.bsky.feed.post/3lzembczmss2z'