setwd("F:/Git/github/r-scripts/analysis/") # please update path
#setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)
library(sqldf)
library(effsize)

# post history (edits)
posthistory <- fread("data/posthistory_date_editcount.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"), stringsAsFactors=FALSE)
names(posthistory) <- c("PostId", "Date", "Creation", "Edits")
# parse date
posthistory$Date <- as.Date(posthistory$Date)

# comments
comments <- fread("data/comments_date_commentcount.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"), stringsAsFactors=FALSE)
names(comments) <- c("PostId", "Date", "Comments")
# parse date
comments$Date <- as.Date(comments$Date)

# merge post history and comments
posthistory_comments <- merge(posthistory, comments, by=c("PostId", "Date"), all.x=TRUE, all.y=TRUE)
posthistory_comments$Creation[is.na(posthistory_comments$Creation)] <- 0
posthistory_comments$Edits[is.na(posthistory_comments$Edits)] <- 0
posthistory_comments$Comments[is.na(posthistory_comments$Comments)] <- 0

n <- nrow(posthistory_comments)
n
# 54,279,453

only_creation <- posthistory_comments[posthistory_comments$Creation>0 & posthistory_comments$Edits==0 & posthistory_comments$Comments==0,]
n_only_creation <- nrow(only_creation)
n_only_creation
# 18,083,313
n_only_creation/n*100
# 33.31521

only_edit <- posthistory_comments[posthistory_comments$Creation==0 & posthistory_comments$Edits>0 & posthistory_comments$Comments==0,]
n_only_edit <- nrow(only_edit)
n_only_edit
# 4,949,569
n_only_edit/n*100
# 9.118679

only_creation_or_edit <- posthistory_comments[(posthistory_comments$Creation>0 | posthistory_comments$Edits>0) & posthistory_comments$Comments==0,]
n_only_creation_or_edit <- nrow(only_creation_or_edit)
n_only_creation_or_edit
# 27,087,615
n_only_creation_or_edit/n*100
# 49.90399

only_comment <- posthistory_comments[posthistory_comments$Creation==0 & posthistory_comments$Edits==0 & posthistory_comments$Comments>0,]
n_only_comment <- nrow(only_comment)
n_only_comment
# 9,671,784
n_only_comment/n*100
# 17.8185

creation_or_edit_and_comment <- posthistory_comments[(posthistory_comments$Creation>0 | posthistory_comments$Edits>0) & posthistory_comments$Comments>0,]
n_creation_or_edit_and_comment <- nrow(creation_or_edit_and_comment)
n_creation_or_edit_and_comment
# 17,520,054
n_creation_or_edit_and_comment/n*100
# 32.27751


# focus on comments
comments <- posthistory_comments[posthistory_comments$Comments>0,]
n <- nrow(comments)
n
# 27,191,838

n_only_comment/n*100
# 35.5687

n_creation_or_edit_and_comment/n*100
# 64.4313


# focus on creation or edits
creation_or_edits <- posthistory_comments[posthistory_comments$Creation>0 | posthistory_comments$Edits>0,]
n <- nrow(creation_or_edits)
n
# 44,607,669

n_only_creation_or_edit/n*100
# 60.72412

n_creation_or_edit_and_comment/n*100
# 39.27588


# focus on days with both comments and edits (or creation)
remove(posthistory)
remove(comments)
remove(posthistory_comments)
remove(creation_or_edits)
remove(only_comment)
remove(only_creation)
remove(only_edit)
remove(only_creation_or_edit)
gc()

actions_per_day <- sqldf("select PostId, sum(Creation) as Creation, sum(Edits) as Edits, sum(Comments) as Comments from creation_or_edit_and_comment group by PostId, Date")

summary(actions_per_day$Creation)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.000   1.000   0.933   1.000   1.000 
sd(actions_per_day$Creation)
# 0.249948

summary(actions_per_day$Edits)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0000   0.0000   0.0000   0.6452   1.0000 367.0000 
sd(actions_per_day$Edits)
# 0.9641421

summary(actions_per_day$Comments)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   2.000   2.805   4.000 117.000
sd(actions_per_day$Comments)
# 2.437511

remove(actions_per_day)


# read dataset with comments on same day as edits

edits_comments <- fread("data/edits_comments_final.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"), stringsAsFactors=FALSE)
names(edits_comments) <- c("PostHistoryId", "CommentId", "TimestampDiff", "PostHistoryTypeId")

# TimestampDiff = CommentTimestamp - EditTimestamp (in seconds)
# convert to integer
edits_comments$TimestampDiff <- as.integer(edits_comments$TimestampDiff)

nrow(edits_comments)
# 65,965,375

# distinguish between real edits and the initial version
creation_comments <- edits_comments[edits_comments$PostHistoryTypeId == 2,]
n_creation <- nrow(creation_comments)
n_creation
# 22,906,423
edits_comments <- edits_comments[edits_comments$PostHistoryTypeId != 2,]
n_edits <- nrow(edits_comments)
n_edits
# 43,058,952
n <- n_creation+n_edits
n
# 65,965,375

# comments related to initial version
n_creation/n*100
# 34.72492

# comments realted to an edit
n_edits/n*100
# 65.27508

# comments before edits
before <- edits_comments[edits_comments$TimestampDiff<0,]
n_before <- nrow(before)
n_before
# 20,620,655
n_before/n_edits*100
# 47.88936

summary(before$TimestampDiff/3600) # hours
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -23.981667  -1.052778  -0.315833  -1.245290  -0.098611  -0.000278 
sd(before$TimestampDiff/3600)
# 2.565722

n_before_1 <- nrow(before[abs(before$TimestampDiff/3600) < 1,])
n_before_1
# 15,283,167
n_before_1/n_before*100
# 74.11582

# comments after edits
after <- edits_comments[edits_comments$TimestampDiff>0,]
n_after <- nrow(after)
n_after
# 22,420,159
n_after/n_edits*100
# 52.06852

summary(after$TimestampDiff/3600) # hours
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000278  0.076111  0.292778  1.292018  1.072778 23.965556 
sd(after$TimestampDiff/3600)
# 2.707173

n_after_1 <- nrow(after[abs(after$TimestampDiff/3600) < 1,])
n_after_1
# 16,559,789
n_after_1/n_after*100
# 73.86116

same_time <- nrow(edits_comments[edits_comments$TimestampDiff==0,])
same_time
# 18,138
same_time/n_edits*100
# 0.04212364


# draw sample for qualitative analysis

# get (PostId, PostHistoryId) mapping
postid_posthistoryid <- fread("data/postid_posthistoryid.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"), stringsAsFactors=FALSE)
names(postid_posthistoryid) <- c("PostId", "PostHistoryId")

# get (PostId, PostTypeId) mapping
postid_posttypeid <- fread("data/postid_posttypeid.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"), stringsAsFactors=FALSE)
names(postid_posttypeid) <- c("PostId", "PostTypeId")

# filter comments that happened up to 10 minutes before or after an edit (on the same day)

# comments at most 10 minutes before edits
before_10 <- edits_comments[edits_comments$TimestampDiff<0 & edits_comments$TimestampDiff/60>=-10,]
n_before_10 <- nrow(before_10)
n_before_10
# 7,318,072
n_before_10/n_before*100
# 35.48904 (35.5% of the comments before edits on the same day happened up to 10 minutes before)

before_10_posthistoryids <- unique(before_10$PostHistoryId)
sample_before_10 <- sample(before_10_posthistoryids, 25)
sample_before_10 <- postid_posthistoryid[postid_posthistoryid$PostHistoryId %in% sample_before_10,]
sample_before_10 <- merge(sample_before_10, postid_posttypeid, by="PostId", all.x=TRUE, all.y=FALSE)
sample_before_10 <- sample_before_10[,c("PostHistoryId", "PostId", "PostTypeId")]  # change order of columns

# write sample to CSV file
write.table(sample_before_10, file="data/sample_before_10.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

# comments at most 10 minutes after edits
after_10 <- edits_comments[edits_comments$TimestampDiff>0 & edits_comments$TimestampDiff/60<=10,]
n_after_10 <- nrow(after_10)
n_after_10
# 8,627,277
n_after_10/n_after*100
# 38.48 (38.5% of the comments after edits on the same day happened up to 10 minutes after)

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
n_only_edit <- nrow(only_edit)
n_only_edit
# 28,702,811
n_only_edit/n*100
# 24.44843

only_votes <- posthistory_votes[posthistory_votes$EditCount==0 & (posthistory_votes$UpVotes>0 | posthistory_votes$DownVotes>0),]
n_only_votes <- nrow(only_votes)
n_only_votes
# 72,793,762
n_only_votes/n*100
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
