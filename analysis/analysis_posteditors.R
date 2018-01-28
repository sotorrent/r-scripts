setwd("F:/Git/github/r-scripts/analysis/") # please update path
#setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)
library(sqldf)
library(Hmisc) # for rcorr
library(effsize)

# posts with version count + first/last post history id
posthistory_users <- fread("data/posthistory_users.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(posthistory_users) <- c("PostId", "PostTypeId", "OwnerUserId", "PostHistoryId", "PostHistoryTypeId", "PostHistoryCreationDate", "UserId", "UserCreationDate", "UserReputation")

# user reputation
users_reputation <- fread("data/users_reputation.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(users_reputation) <- c("UserId", "Reputation")

n <- nrow(posthistory_users)
n
# 60,235,290


posthistory_users$author_edit <- posthistory_users$OwnerUserId == posthistory_users$UserId

author_edits <- posthistory_users[posthistory_users$author_edit == TRUE,]
nrow(author_edits)
# 52,650,113
nrow(author_edits)/n*100
# 87.40742

other_edits <- posthistory_users[posthistory_users$author_edit == FALSE,]
nrow(other_edits)
# 7,585,177
nrow(other_edits)/n*100
# 12.59258

# author reputation of posts with author vs. non-author edits
post_id_filter_nonauthor <- unique(other_edits$PostId)
posthistory_nonauthor_edits <- posthistory_users[posthistory_users$PostId %in% post_id_filter_nonauthor,]
posthistory_nonauthor_edits$OwnerUserId <- as.numeric(posthistory_nonauthor_edits$OwnerUserId)
# some author ids are actually login names -> NAs introduced
# add author reputation
posthistory_nonauthor_edits <- merge(posthistory_nonauthor_edits, users_reputation, by.x="OwnerUserId", by.y="UserId")

summary(posthistory_nonauthor_edits$Reputation)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1      28     351   13560    2899  990400 

# get posts that only the author edited
posthistory_users$author_edit_flag <- 0
posthistory_users[posthistory_users$author_edit==TRUE,]$author_edit_flag <- 0
posthistory_users[posthistory_users$author_edit==FALSE,]$author_edit_flag <- 1
postid_edits <- sqldf("select PostId, sum(author_edit_flag) as edit_flag_sum from posthistory_users group by PostId")
post_id_filter_onlyauthor <- postid_edits$PostId[postid_edits$edit_flag_sum == 0]
length(post_id_filter_onlyauthor)
# 32,146,026

# analyze reputation of posts that only the author edited
posthistory_onlyauthor_edits <- posthistory_users[posthistory_users$PostId %in% post_id_filter_onlyauthor,]
posthistory_onlyauthor_edits$OwnerUserId <- as.numeric(posthistory_onlyauthor_edits$OwnerUserId)
# some author ids are actually login names -> NAs introduced
# add author reputation
posthistory_onlyauthor_edits <- merge(posthistory_onlyauthor_edits, users_reputation, by.x="OwnerUserId", by.y="UserId")

summary(posthistory_onlyauthor_edits$Reputation)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1     257    1802   25780   11360  990400 

intersect(post_id_filter_nonauthor, post_id_filter_onlyauthor)
# integer(0)

# compare reputation of authors with non-author edits and reputation of authors with author-only edits
wilcox.test(posthistory_nonauthor_edits$Reputation,
            posthistory_onlyauthor_edits$Reputation,
            alternative="two.sided",
            paired=F, correct=T)
# W = 2.5194e+14, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cohen.d(posthistory_nonauthor_edits$Reputation, # "treatment"
        posthistory_onlyauthor_edits$Reputation, # "control"
        paired=FALSE)
# d estimate: -0.1565245 (negligible)
# 95 percent confidence interval:
#   inf sup 
# NA  NA 
