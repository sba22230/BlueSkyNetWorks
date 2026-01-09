SELECT P.uri AS Post , 
P.author_handle AS PostedBy,  
CAST(P.indexedAt AS date) AS PostedOn, P.text, 
P.bookmark_count, P.like_count, p.reply_count, P.repost_count,
RP.handle AS RepostedBy 
FROM posts_raw AS P
INNER JOIN reposts_raw as RP ON P.uri = RP.uri
ORDER BY P.uri
