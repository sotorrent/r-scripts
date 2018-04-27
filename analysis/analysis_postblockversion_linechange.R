setwd("E:/Git/github/r-scripts/analysis/") # please update path
#setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)
library(effsize)

# lines added/deleted between postblock versions
postblockversion_linesdeleted <- fread("data/postblockversion_linesdeleted.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
#names(postblockversion_linesdeleted) <- c("PostId", "PostHistoryId", "PostBlockVersionId", "PredPostBlockVersionId", "LinesDeleted")

postblockversion_linesadded <- fread("data/postblockversion_linesadded.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
#names(postblockversion_linesadded) <- c("PostId", "PostHistoryId", "PostBlockVersionId", "PredPostBlockVersionId", "LinesAdded")

# merge data
postblockversion_linechange <- merge(postblockversion_linesadded, postblockversion_linesdeleted, by=c("PostId", "PostHistoryId", "PostBlockVersionId", "PredPostBlockVersionId"), all.x=TRUE, all.y=TRUE)
postblockversion_linechange$LinesAdded[is.na(postblockversion_linechange$LinesAdded)] <- 0
postblockversion_linechange$LinesDeleted[is.na(postblockversion_linechange$LinesDeleted)] <- 0


summary(postblockversion_linechange$LinesAdded)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000    1.000    1.000    2.886    2.000 1562.000

sd(postblockversion_linechange$LinesAdded)
# 8.40198

summary(postblockversion_linechange$LinesDeleted)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000    1.000    1.000    3.318    3.000 1323.000 

sd(postblockversion_linechange$LinesDeleted)
# 9.004448

n <- nrow(postblockversion_linechange)
n
# 26,145,129

postblockversion_linechange_1 <- postblockversion_linechange[postblockversion_linechange$LinesAdded <= 1 & postblockversion_linechange$LinesDeleted <= 1,]
n_1 <- nrow(postblockversion_linechange_1)
n_1
# 11,536,944
n_1/n*100
# 44.12655

# text vs. code blocks?
postblockversion_type <- fread("data/postblockversion_type.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(postblockversion_type) <- c("PostBlockVersionId", "PostBlockTypeId")
postblockversion_linechange <- merge(postblockversion_linechange, postblockversion_type, by="PostBlockVersionId")

# text blocks
postblockversion_linechange_text <- postblockversion_linechange[postblockversion_linechange$PostBlockTypeId==1,]

summary(postblockversion_linechange_text$LinesAdded)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000    1.000    1.000    1.737    2.000 1002.000 

n <- nrow(postblockversion_linechange_text)
n
# 18,883,039
postblockversion_linechange_1_text <- postblockversion_linechange_text[postblockversion_linechange_text$LinesAdded <= 1 & postblockversion_linechange_text$LinesDeleted <= 1,]
n_1 <- nrow(postblockversion_linechange_1_text)
n_1
# 8,999,884
n_1/n*100
# 47.66121

# code blocks
postblockversion_linechange_code <- postblockversion_linechange[postblockversion_linechange$PostBlockTypeId==2,]

summary(postblockversion_linechange_code$LinesAdded)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000    1.000    2.000    5.875    5.000 1562.000 

n <- nrow(postblockversion_linechange_code)
n
# 7,262,090
postblockversion_linechange_1_code <- postblockversion_linechange_code[postblockversion_linechange_code$LinesAdded <= 1 & postblockversion_linechange_code$LinesDeleted <= 1,]
n_1 <- nrow(postblockversion_linechange_1_code)
n_1
# 2,537,060
n_1/n*100
# 34.93567

# text vs. code

# lines added
wilcox.test(postblockversion_linechange_code$LinesAdded,
            postblockversion_linechange_text$LinesAdded,
            alternative="two.sided",
            paired=F,correct=T)
# W = 8.7346e+13, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cohen.d(postblockversion_linechange_code$LinesAdded, # "treatment"
        postblockversion_linechange_text$LinesAdded, # "control"
        paired=FALSE)
# d estimate: 0.5050054 (medium)
# 95 percent confidence interval:
#   inf sup 
# NA  NA 

# lines deleted
wilcox.test(postblockversion_linechange_code$LinesDeleted,
            postblockversion_linechange_text$LinesDeleted,
            alternative="two.sided",
            paired=F,correct=T)
# W = 8.9075e+13, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cohen.d(postblockversion_linechange_code$LinesDeleted, # "treatment"
        postblockversion_linechange_text$LinesDeleted, # "control"
        paired=FALSE)
# d estimate: 0.5709666 (medium)
# 95 percent confidence interval:
#   inf sup 
# NA  NA
