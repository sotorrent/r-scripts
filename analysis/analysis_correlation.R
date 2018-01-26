setwd("F:/Git/github/r-scripts/analysis/") # please update path
#setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)
library(corrgram)
library(Hmisc) # for rcorr
library(car)

# post metadata
posts_metadata <- fread("data/posts_metadata.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(posts_metadata) <- c("PostId", "PostTypeId", "Score", "CommentCount", "Age")

# post version count
posts_versioncount <- fread("data/posts_versioncount.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(posts_versioncount) <- c("PostId", "PostTypeId", "VersionCount", "FirstPostHistoryId", "LastPostHistoryId")

# gh matches
posts_gh <- fread("data/posts_gh-matches.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(posts_gh) <- c("PostId", "GHMatchCount")

# merge post data
posts <- merge(posts_metadata, posts_versioncount, by="PostId", all.x=TRUE)
posts <- posts[,c("PostId", "PostTypeId.x", "Score", "CommentCount", "Age", "VersionCount")]
names(posts) <- c("PostId", "PostTypeId", "Score", "CommentCount", "Age", "VersionCount")
posts <- merge(posts, posts_gh, by="PostId", all.x=TRUE)
posts <- posts[,c("PostId", "PostTypeId", "Score", "CommentCount", "Age", "VersionCount", "GHMatchCount")]
posts$GHMatchCount[is.na(posts$GHMatchCount)] <- 0

correlations <- rcorr(as.matrix(posts[,c("Score", "CommentCount", "Age", "VersionCount", "GHMatchCount")]), type="spearman") # pairwise deletion, Alternativ: type="pearson"
correlations
#              Score CommentCount   Age VersionCount GHMatchCount
# Score         1.00         0.08 -0.25         0.09         0.09
# CommentCount  0.08         1.00  0.03         0.26         0.02
# Age          -0.25         0.03  1.00         0.03        -0.04
# VersionCount  0.09         0.26  0.03         1.00         0.04
# GHMatchCount  0.09         0.02 -0.04         0.04         1.00
# n
#                 Score CommentCount      Age VersionCount GHMatchCount
# Score        38394917     38394917 38394917     38394895     38394917
# CommentCount 38394917     38394917 38394917     38394895     38394917
# Age          38394917     38394917 38394917     38394895     38394917
# VersionCount 38394895     38394895 38394895     38394895     38394895
# GHMatchCount 38394917     38394917 38394917     38394895     38394917
# P
#           Score CommentCount Age VersionCount GHMatchCount
# Score               0            0   0            0          
# CommentCount  0                  0   0            0          
# Age           0     0                0            0          
# VersionCount  0     0            0                0          
# GHMatchCount  0     0            0   0                 

