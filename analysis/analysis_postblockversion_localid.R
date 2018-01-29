setwd("F:/Git/github/r-scripts/analysis/") # please update path
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
# 71,439,829

n_0 <- length(localid_diff$LocalIdDiff[localid_diff$LocalIdDiff==0])
n_0
# 68,195,111
n_0/n*100
# 95.45811

n_1 <- length(localid_diff$LocalIdDiff[localid_diff$LocalIdDiff==1])
n_1
# 372,992
n_1/n*100
# 0.5221065

n_2 <- length(localid_diff$LocalIdDiff[localid_diff$LocalIdDiff==2])
n_2
# 2,239,925
n_2/n*100
# 3.135401

n_3 <- length(localid_diff$LocalIdDiff[localid_diff$LocalIdDiff==3])
n_3
# 44,418
n_3/n*100
# 0.0621754

table(localid_diff$LocalIdDiff[localid_diff$LocalIdDiff>0])

table(localid_diff$LocalIdDiff[localid_diff$LocalIdDiff>0])[1:10]
#       1         2        3        4        5        6        7        8       9       10 
# 372,992 2,239,925   44,418  414,925    8,078   96,892    2,443   33,526     802   13,139  

