setwd("F:/Git/github/r-scripts/analysis/") # please update path
#setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)
library(sqldf)

# post history (edits)
posthistory <- fread("data/posthistory_date_editcount.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"), stringsAsFactors=FALSE)
names(posthistory) <- c("PostId", "Date", "EditCount")
# parse date
posthistory$Date <- as.Date(posthistory$Date)

# comments
comments <- fread("data/comments_date_commentcount.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"), stringsAsFactors=FALSE)
names(comments) <- c("PostId", "Date", "CommentCount")
# parse date
comments$Date <- as.Date(comments$Date)


# merge post history and comments

posthistory_comments <- merge(posthistory, comments, by=c("PostId", "Date"), all.x=TRUE, all.y=TRUE)
posthistory_comments$EditCount[is.na(posthistory_comments$EditCount)] <- 0
posthistory_comments$CommentCount[is.na(posthistory_comments$CommentCount)] <- 0

n <- nrow(posthistory_comments)
n
# 54,279,453

only_edit <- posthistory_comments[posthistory_comments$EditCount>0 & posthistory_comments$CommentCount==0,]
n_edit <- nrow(only_edit)
n_edit
# 27,087,615
n_edit/n*100
# 49.90399

only_comment <- posthistory_comments[posthistory_comments$EditCount==0 & posthistory_comments$CommentCount>0,]
n_comment <- nrow(only_comment)
n_comment
# 9,671,784
n_comment/n*100
# 17.8185

both <- posthistory_comments[posthistory_comments$EditCount>0 & posthistory_comments$CommentCount>0,]
n_both <- nrow(both)
n_both
# 17,520,054
n_both/n*100
# 32.27751


# focus on comments
comments <- posthistory_comments[posthistory_comments$CommentCount>0,]
n <- nrow(comments)
n
# 27,191,838

only_comment <- comments[comments$EditCount==0 & comments$CommentCount>0,]
n_comment <- nrow(only_comment)
n_comment
# 9,671,784
n_comment/n*100
# 35.5687

both <- comments[comments$EditCount>0 & comments$CommentCount>0,]
n_both <- nrow(both)
n_both
# 17,520,054
n_both/n*100
# 64.4313


# focus on edits
edits <- posthistory_comments[posthistory_comments$EditCount>0,]
n <- nrow(edits)
n
# 44,607,669

only_edit <- edits[edits$EditCount>0 & edits$CommentCount==0,]
n_edit <- nrow(only_edit)
n_edit
# 27,087,615
n_edit/n*100
# 60.72412

both <- edits[edits$EditCount>0 & edits$CommentCount>0,]
n_both <- nrow(both)
n_both
# 17,520,054
n_both/n*100
# 39.27588


# focus on days with both comments and edits
remove(posthistory)
remove(comments)
remove(posthistory_comments)
remove(only_comment)
remove(only_edit)
remove(edits)
gc()

both_per_day <- sqldf("select PostId, sum(EditCount) as Edits, sum(CommentCount) as Comments from both group by PostId, Date")

summary(both_per_day$Edits)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   1.000   1.578   2.000 367.000
sd(both_per_day$Edits)
# 0.9501668

summary(both_per_day$Comments)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   2.805   4.000 117.000 
sd(both_per_day$Comments)
# 2.437511

remove(both_per_day)


# read final dataset from BigQuery (comments on same day as edits)

edits_comments_final <- fread("data/edits_comments_final.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"), stringsAsFactors=FALSE)
names(edits_comments_final) <- c("PostHistoryId", "CommentId", "TimestampDiff")

# TimestampDiff = CommentTimestamp - EditTimestamp (in seconds)
# convert to integer
edits_comments_final$TimestampDiff <- as.integer(edits_comments_final$TimestampDiff)

nrow(edits_comments_final)
# 89,514,938

edits_comments_final <- edits_comments_final[!is.na(edits_comments_final$TimestampDiff)]

n <- nrow(edits_comments_final)
n
# 89,514,937

# comments before edits
before <- edits_comments_final[edits_comments_final$TimestampDiff<0,]
n_before <- nrow(before)
n_before
# 20,621,187
n_before/n*100
# 23.03659

summary(before$TimestampDiff/3600) # hours
      # Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -23.980000  -1.053000  -0.315800  -1.245000  -0.098610  -0.000278 
sd(before$TimestampDiff/3600)
# 2.565737

n_before_1 <- nrow(before[abs(before$TimestampDiff/3600) < 1,])
n_before_1
# 15,283,439
n_before_1/n_before*100
# 74.11522


# comments after edits
after <- edits_comments_final[edits_comments_final$TimestampDiff>0,]
n_after <- nrow(after)
n_after
# 68,875,599
n_after/n*100
# 76.94314

summary(after$TimestampDiff/3600) # hours
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000278  0.110600  0.328300  1.284000  1.077000 23.980000 
sd(after$TimestampDiff/3600)


n_after_1 <- nrow(after[abs(after$TimestampDiff/3600) < 1,])
n_after_1
# 50,781,113
n_after_1/n_after*100
# 73.72874




# merge post history and votes

# votes
edits_votes <- fread("data/edits_votes.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"), stringsAsFactors=FALSE)
names(edits_votes) <- c("PostId", "Date", "EditCount", "UpVotes", "DownVotes")
# parse date
edits_votes$Date <- as.Date(edits_votes$Date)
# set NA values to 0
edits_votes$UpVotes[is.na(edits_votes$UpVotes)] <- 0
edits_votes$DownVotes[is.na(edits_votes$DownVotes)] <- 0

n <- nrow(edits_votes)
n
# 159,04,858

summary(edits_votes$EditCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   1.000   1.507   2.000 367.000 

summary(edits_votes$UpVotes)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     1.0     1.0     1.6     2.0  1436.0 

summary(edits_votes$DownVotes)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.2185  0.0000 59.0000 


only_edit <- edits_votes[edits_votes$EditCount>0 & edits_votes$UpVotes==0 & edits_votes$DownVotes==0,]
n_edit <- nrow(only_edit)
n_edit
# 0
n_edit/n*100
# 0

only_votes <- edits_votes[edits_votes$EditCount==0 & (edits_votes$UpVotes>0 | edits_votes$DownVotes>0),]
n_votes <- nrow(only_votes)
n_votes
# 0
n_votes/n*100
# 0

both <- edits_votes[edits_votes$EditCount>0 & (edits_votes$UpVotes>0 | edits_votes$DownVotes>0),]
n_both <- nrow(both)
n_both
# 15,904,858
n_both/n*100
# 100



