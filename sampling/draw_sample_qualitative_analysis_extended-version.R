setwd("") # Pfad bitte anpassen

library(data.table)

edited_threads <- fread("data/EditedThreads.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"))
names(edited_threads) <- c("PostId", "PostTypeId", "VersionCount", "ParentId")
nrow(edited_threads)
# 27,344,479

thread_owners <- fread("data/EditedThreadsOwners.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"))
names(thread_owners) <- c("PostId", "OwnerUserId", "OwnerDisplayName")
nrow(thread_owners)
# 27,344,479

comments <- fread("data/EditedThreadsComments.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"))
names(comments) <- c("PostId", "PostTypeId", "CommentId", "CreationDate", "UserId", "OwnerDisplayName")  
nrow(comments)
# 50,112,975

edits <- fread("data/EditedThreadsEdits.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"))
names(edits) <- c("PostId", "PostTypeId", "PostHistoryId", "PostHistoryTypeId", "CreationDate", "UserId", "UserDisplayName", "Comment")
nrow(edits)
# 49,952,070

# add owners to threads
edited_threads_owners <- merge(edited_threads, thread_owners, by="PostId", all.x=TRUE)
# add comment count to posts
library(sqldf)
comment_count <- sqldf("SELECT PostId, COUNT(CommentId) as CommentCount FROM comments GROUP BY PostId")
edited_threads_owners_comment_count <- merge(edited_threads_owners, comment_count, by="PostId", all.x=TRUE) 
edited_threads_owners_comment_count[is.na(edited_threads_owners_comment_count$CommentCount),]$CommentCount <- 0

# get IDs of questions (they identify individual threads)
thread_ids <- edited_threads[edited_threads$PostTypeId==1,]$ParentId
length(thread_ids)
# 9,756,316

# draw sample of thread ids
sample_thread_ids <- thread_ids[sample(length(thread_ids), 400)]
sample <- edited_threads_owners_comment_count[edited_threads_owners_comment_count$ParentId %in% sample_thread_ids,
                                              c("PostId", "PostTypeId", "VersionCount", "CommentCount", "ParentId", "OwnerUserId", "OwnerDisplayName")]  # change order of columns
length(unique(sample[sample$PostTypeId==1]$PostId))
# 400

# get comments
sample_comments <- comments[comments$PostId %in% sample$PostId,]

# get edits
sample_edits <- edits[edits$PostId %in% sample$PostId,]

# write sampled threads to CSV file
write.table(sample, file="data/EditedThreads_Sample400.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

# write sampled comments to CSV file
write.table(sample_comments, file="data/EditedThreads_Sample400_Comments.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

# write sampled edits to CSV file
write.table(sample_edits, file="data/EditedThreads_Sample400_Edits.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
