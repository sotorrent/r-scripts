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

