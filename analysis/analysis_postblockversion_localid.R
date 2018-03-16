setwd("E:/Git/github/r-scripts/analysis/") # please update path
#setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)

# difference between local ids of connected post block versions
localid_diff <- fread("data/postblockversion_localiddiff.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(localid_diff) <- c("PostBlockVersionId", "LocalIdDiff")


summary(localid_diff$LocalIdDiff)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -146.0000    0.0000    0.0000   -0.0529    0.0000  562.0000

localid_diff$LocalIdDiff <- abs(localid_diff$LocalIdDiff)

summary(localid_diff$LocalIdDiff)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0000   0.0000   0.0000   0.1119   0.0000 562.0000 

n <- nrow(localid_diff)
n
# 71,440,329

n_0 <- length(localid_diff$LocalIdDiff[localid_diff$LocalIdDiff==0])
n_0
# 68,195,919
n_0/n*100
# 95.45857

n_1 <- length(localid_diff$LocalIdDiff[localid_diff$LocalIdDiff==1])
n_1
# 373,094
n_1/n*100
# 0.5222456

n_2 <- length(localid_diff$LocalIdDiff[localid_diff$LocalIdDiff==2])
n_2
# 2,239,611
n_2/n*100
# 3.134939

n_3 <- length(localid_diff$LocalIdDiff[localid_diff$LocalIdDiff==3])
n_3
# 44,422
n_3/n*100
# 0.06218056

table(localid_diff$LocalIdDiff[localid_diff$LocalIdDiff>0])

table(localid_diff$LocalIdDiff[localid_diff$LocalIdDiff>0])[1:10]
#       1         2        3        4        5        6        7        8       9       10 
# 373,094 2,239,611   44,422  414,870    8,080   96,906    2,442   33,482     805   13,130   
