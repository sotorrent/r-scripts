setwd("E:/Git/github/r-scripts/analysis/") # please update path
#setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)
library(Hmisc) # for rcorr
library(effsize)

# post metadata
posts_metadata <- fread("data/posts_metadata.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(posts_metadata) <- c("PostId", "PostTypeId", "Score", "CommentCount", "Age", "OwnerUserId")
posts_metadata$OwnerUserId <- as.numeric(posts_metadata$OwnerUserId)
length(which(is.na(posts_metadata$OwnerUserId)))
# 349,518 -> login name instead of id

# post version count
posts_versioncount <- fread("data/posts_versioncount.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(posts_versioncount) <- c("PostId", "PostTypeId", "VersionCount", "FirstPostHistoryId", "LastPostHistoryId")

# gh matches
posts_gh <- fread("data/posts_gh-matches.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(posts_gh) <- c("PostId", "GHMatchCount")

# user reputation
users_reputation <- fread("data/users_reputation.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(users_reputation) <- c("UserId", "Reputation")

# merge post data
posts <- merge(posts_metadata, posts_versioncount, by="PostId", all.x=TRUE)
posts <- posts[,c("PostId", "PostTypeId.x", "Score", "CommentCount", "Age", "OwnerUserId", "VersionCount")]
names(posts) <- c("PostId", "PostTypeId", "Score", "CommentCount", "Age", "OwnerUserId", "VersionCount")
posts <- merge(posts, posts_gh, by="PostId", all.x=TRUE)
posts <- posts[,c("PostId", "PostTypeId", "Score", "CommentCount", "Age", "OwnerUserId", "VersionCount", "GHMatchCount")]
posts <- merge(posts, users_reputation, by.x="OwnerUserId", by.y="UserId", all.x=TRUE)
posts <- posts[,c("PostId", "PostTypeId", "Score", "CommentCount", "Age", "OwnerUserId", "VersionCount", "GHMatchCount", "Reputation")]


correlations <- rcorr(as.matrix(posts[,c("VersionCount", "Age", "Score", "CommentCount", "GHMatchCount", "Reputation")]), type="spearman") # pairwise deletion, Alternativ: type="pearson"
correlations
#              VersionCount   Age Score CommentCount GHMatchCount Reputation
# VersionCount         1.00 -0.03  0.09         0.26         0.09      -0.05
# Age                 -0.03  1.00  0.25        -0.03         0.10       0.32
# Score                0.09  0.25  1.00         0.08         0.23       0.33
# CommentCount         0.26 -0.03  0.08         1.00         0.09       0.03
# GHMatchCount         0.09  0.10  0.23         0.09         1.00       0.09
# Reputation          -0.05  0.32  0.33         0.03         0.09       1.00

# n
#              VersionCount      Age    Score CommentCount GHMatchCount Reputation
# VersionCount     38394895 38394895 38394895     38394895       136609   38045375
# Age              38394895 38394917 38394917     38394917       136609   38045397
# Score            38394895 38394917 38394917     38394917       136609   38045397
# CommentCount     38394895 38394917 38394917     38394917       136609   38045397
# GHMatchCount       136609   136609   136609       136609       136609     134721
# Reputation       38045375 38045397 38045397     38045397       134721   38045397

# P
#             VersionCount Age Score CommentCount GHMatchCount Reputation
# VersionCount               0   0     0            0            0        
# Age           0                0     0            0            0        
# Score         0            0         0            0            0        
# CommentCount  0            0   0                  0            0        
# GHMatchCount  0            0   0     0                         0        
# Reputation    0            0   0     0            0                     



#####
# quasi experiments
#####

summary(posts$VersionCount)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.000   1.000   1.000   1.569   2.000 754.000      22
sd(posts$VersionCount[!is.na(posts$VersionCount)])
# 1.013765

length(posts$VersionCount[posts$VersionCount==1])/length(posts$VersionCount[!is.na(posts$VersionCount)])*100
# 63.86325

summary(posts$CommentCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   1.000   1.625   2.000 176.000 
sd(posts$CommentCount)
# 2.464756

posts_v1 <- posts[posts$VersionCount==1 & !is.na(posts$CommentCount),]
posts_v2 <- posts[posts$VersionCount>1 & !is.na(posts$CommentCount),]

wilcox.test(posts_v2$CommentCount,
            posts_v1$CommentCount,
            alternative="two.sided",
            paired=F,correct=T)
# W = 2.1567e+14, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cohen.d(posts_v2$CommentCount, # "treatment"
        posts_v1$CommentCount, # "control"
        paired=FALSE)
# d estimate: 0.5218075 (medium)
# 95 percent confidence interval:
#  inf sup 
#   NA  NA

boxplot(posts_v1$CommentCount, posts_v2$CommentCount, outline=FALSE)


posts_c1 <- posts[posts$CommentCount<=1 & !is.na(posts$VersionCount),]
posts_c2 <- posts[posts$CommentCount>1 & !is.na(posts$VersionCount),]

wilcox.test(posts_c2$VersionCount,
            posts_c1$VersionCount,
            alternative="two.sided",
            paired=F,correct=T)
# W = 2.121e+14, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cohen.d(posts_c2$VersionCount, # "treatment"
        posts_c1$VersionCount, # "control"
        paired=FALSE)
# d estimate: 0.4876967 (small)
# 95 percent confidence interval:
#  inf sup 
#   NA  NA 

boxplot(posts_c1$VersionCount, posts_c2$VersionCount, outline=FALSE)

