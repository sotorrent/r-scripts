setwd("F:/Git/github/r-scripts/analysis/") # please update path
#setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)
library(sqldf)
library(effsize)

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


# read final dataset (comments on same day as edits)

edits_comments_final <- fread("data/edits_comments_final.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"), stringsAsFactors=FALSE)
names(edits_comments_final) <- c("PostHistoryId", "CommentId", "TimestampDiff")

# TimestampDiff = CommentTimestamp - EditTimestamp (in seconds)
# convert to integer
edits_comments_final$TimestampDiff <- as.integer(edits_comments_final$TimestampDiff)

nrow(edits_comments_final)
# 89,514,937

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
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000278  0.110556  0.328333  1.284003  1.076667 23.979444 
sd(after$TimestampDiff/3600)
# 2.639866

n_after_1 <- nrow(after[abs(after$TimestampDiff/3600) < 1,])
n_after_1
# 50,781,113
n_after_1/n_after*100
# 73.72874

same_time <- nrow(edits_comments_final[edits_comments_final$TimestampDiff==0,])
same_time
# 18,151
same_time/n*100
# 0.02027706


# draw sample for qualitative analysis

# get (PostId, PostHistoryId) mapping
postid_posthistoryid <- fread("data/postid_posthistoryid.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"), stringsAsFactors=FALSE)
names(postid_posthistoryid) <- c("PostId", "PostHistoryId")

# get (PostId, PostTypeId) mapping
postid_posttypeid <- fread("data/postid_posttypeid.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"), stringsAsFactors=FALSE)
names(postid_posttypeid) <- c("PostId", "PostTypeId")

# filter comments that happened up to 10 minutes before or after an edit (on the same day)

# comments at most 10 minutes before edits
before_10 <- edits_comments_final[edits_comments_final$TimestampDiff<0 & edits_comments_final$TimestampDiff/60>=-10,]
n_before_10 <- nrow(before_10)
n_before_10
# 7,318,147
n_before_10/n_before*100
# 35.48849 (35.5% of the comments before edits on the same day happened up to 10 minutes before)

before_10_posthistoryids <- unique(before_10$PostHistoryId)
sample_before_10 <- sample(before_10_posthistoryids, 25)
sample_before_10 <- postid_posthistoryid[postid_posthistoryid$PostHistoryId %in% sample_before_10,]
sample_before_10 <- merge(sample_before_10, postid_posttypeid, by="PostId", all.x=TRUE, all.y=FALSE)
sample_before_10 <- sample_before_10[,c("PostHistoryId", "PostId", "PostTypeId")]  # change order of columns

# write sample to CSV file
write.table(sample_before_10, file="data/sample_before_10.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

# comments at most 10 minutes after edits
after_10 <- edits_comments_final[edits_comments_final$TimestampDiff>0 & edits_comments_final$TimestampDiff/60<=10,]
n_after_10 <- nrow(after_10)
n_after_10
# 23,322,636
n_after_10/n_after*100
# 33.86197 (33.9% of the comments after edits on the same day happened up to 10 minutes after)

after_10_posthistoryids <- unique(after_10$PostHistoryId)
sample_after_10 <- sample(after_10_posthistoryids, 25)
sample_after_10 <- postid_posthistoryid[postid_posthistoryid$PostHistoryId %in% sample_after_10,]
sample_after_10 <- merge(sample_after_10, postid_posttypeid, by="PostId", all.x=TRUE, all.y=FALSE)
sample_after_10 <- sample_after_10[,c("PostHistoryId", "PostId", "PostTypeId")]  # change order of columns

# write sample to CSV file
write.table(sample_after_10, file="data/sample_after_10.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")


# merge post history and votes

# votes
votes <- fread("data/votes_date_votecount.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"), stringsAsFactors=FALSE)
names(votes) <- c("PostId", "Date", "UpVotes", "DownVotes")
# parse date
votes$Date <- as.Date(votes$Date)

# merge post history and votes
posthistory_votes <- merge(posthistory, votes, by=c("PostId", "Date"), all.x=TRUE, all.y=TRUE)
# set NA values to 0
posthistory_votes$EditCount[is.na(posthistory_votes$EditCount)] <- 0
posthistory_votes$UpVotes[is.na(posthistory_votes$UpVotes)] <- 0
posthistory_votes$DownVotes[is.na(posthistory_votes$DownVotes)] <- 0


n <- nrow(posthistory_votes)
n
# 117,401,431

summary(posthistory_votes$EditCount)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0000   0.0000   0.0000   0.5149   1.0000 367.0000 

summary(posthistory_votes$UpVotes)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000    0.0000    1.0000    0.8404    1.0000 1436.0000 

summary(posthistory_votes$DownVotes)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0000   0.0000   0.0000   0.1071   0.0000 102.0000 


only_edit <- posthistory_votes[posthistory_votes$EditCount>0 & posthistory_votes$UpVotes==0 & posthistory_votes$DownVotes==0,]
n_edit <- nrow(only_edit)
n_edit
# 28,702,811
n_edit/n*100
# 24.44843

only_votes <- posthistory_votes[posthistory_votes$EditCount==0 & (posthistory_votes$UpVotes>0 | posthistory_votes$DownVotes>0),]
n_votes <- nrow(only_votes)
n_votes
# 72,793,762
n_votes/n*100
# 62.00415

both <- posthistory_votes[posthistory_votes$EditCount>0 & (posthistory_votes$UpVotes>0 | posthistory_votes$DownVotes>0),]
n_both <- nrow(both)
n_both
# 15,904,858
n_both/n*100
# 13.54741


# read data from BigQuery (see db-scripts for more information)
sample_votes_before_edits <- fread("data/sample_votes_before_edits.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"), stringsAsFactors=FALSE)
names(sample_votes_before_edits) <- c("PostId", "UpVotesBefore")
sample_votes_after_edits <- fread("data/sample_votes_after_edits.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"), stringsAsFactors=FALSE)
names(sample_votes_after_edits) <- c("PostId", "UpVotesAfter")

# merge before and after
sample_votes <- merge(sample_votes_before_edits, sample_votes_after_edits, on="PostId", all.x=TRUE, all.y=TRUE)
sample_votes$UpVotesBefore[is.na(sample_votes$UpVotesBefore)] <- 0
sample_votes$UpVotesAfter[is.na(sample_votes$UpVotesAfter)] <- 0

summary(sample_votes$UpVotesBefore)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0000   0.0000   1.0000   0.8825   1.0000 665.0000
sd(sample_votes$UpVotesBefore)
# 2.269097

summary(sample_votes$UpVotesAfter)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0000   0.0000   0.0000   0.7769   1.0000 425.0000 
sd(sample_votes$UpVotesAfter)
# 1.974936

wilcox.test(sample_votes$UpVotesAfter,
            sample_votes$UpVotesBefore,
            alternative="two.sided",
            paired=F, correct=T)
# W = 1.4191e+10, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cohen.d(sample_votes$UpVotesAfter, # "treatment"
        sample_votes$UpVotesBefore, # "control"
        paired=TRUE)
# d estimate: -0.03899819 (negligible)
# 95 percent confidence interval:
#   inf sup 
# NA  NA 
