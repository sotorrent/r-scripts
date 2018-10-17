# set working directory (see https://stackoverflow.com/a/35842119)
dir = tryCatch({
  # script being sourced
  getSrcDirectory()[1]
}, error = function(e) {
  # script being run in RStudio
  dirname(rstudioapi::getActiveDocumentContext()$path)
})
setwd(dir)

library(data.table)
library(sqldf)

edited_threads <- fread("data/EditedThreads.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"))
names(edited_threads) <- c("PostId", "PostTypeId", "VersionCount", "ParentId")
nrow(edited_threads)
# 28,844,629

thread_owners <- fread("data/EditedThreadsOwners.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"))
names(thread_owners) <- c("PostId", "OwnerUserId", "OwnerDisplayName")
nrow(thread_owners)
# 28,844,629

comments <- fread("data/EditedThreadsComments.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"))
names(comments) <- c("PostId", "PostTypeId", "CommentId", "CreationDate", "UserId", "OwnerDisplayName")  
nrow(comments)
# 53,095,008

edits <- fread("data/EditedThreadsEdits.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"))
names(edits) <- c("PostId", "PostTypeId", "PostHistoryId", "PostHistoryTypeId", "CreationDate", "UserId", "UserDisplayName", "Comment")
nrow(edits)
# 52,859,748

# add owners to threads
edited_threads_owners <- merge(edited_threads, thread_owners, by="PostId", all.x=TRUE)

# add comment count to posts
comment_count <- sqldf("SELECT PostId, COUNT(CommentId) as CommentCount FROM comments GROUP BY PostId")
edited_threads_owners_comment_count <- merge(edited_threads_owners, comment_count, by="PostId", all.x=TRUE) 
edited_threads_owners_comment_count[is.na(edited_threads_owners_comment_count$CommentCount),]$CommentCount <- 0

# select threads 
threads_edits_comments <- sqldf("SELECT ParentId as PostId, SUM(VersionCount)-COUNT(*) AS EditCount, SUM(CommentCount) AS CommentCount FROM edited_threads_owners_comment_count GROUP BY ParentId")
nrow(threads_edits_comments)
# 10,343,415 

# select threads with at least one edit and at least one comment
threads_edits_comments <- threads_edits_comments[threads_edits_comments$EditCount>0 & threads_edits_comments$CommentCount>0,]
nrow(threads_edits_comments)
# 9,147,581

# get IDs of questions (they identify individual threads)
thread_ids <- threads_edits_comments$PostId
length(thread_ids)
# 9,147,581

# draw sample of thread ids
sample_thread_ids <- thread_ids[sample(length(thread_ids), 50)]
sample_df <- threads_edits_comments[threads_edits_comments$PostId %in% sample_thread_ids,]
length(unique(sample_df$PostId))
# 50

# randomize order
sample_df <- sample_df[sample(nrow(sample_df)),]

# get comments
sample_comments <- comments[comments$PostId %in% sample_df$PostId,]

# get edits
sample_edits <- edits[edits$PostId %in% sample_df$PostId,]

# write sampled threads to CSV file
write.table(sample_df, file="data/EditedThreads_Sample50.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

# write sampled comments to CSV file
write.table(sample_comments, file="data/EditedThreads_Sample50_Comments.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

# write sampled edits to CSV file
write.table(sample_edits, file="data/EditedThreads_Sample50_Edits.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

