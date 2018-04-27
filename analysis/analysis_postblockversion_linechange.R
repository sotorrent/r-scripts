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
# 
R version 3.4.3 (2017-11-30) -- "Kite-Eating Tree"
Copyright (C) 2017 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

> setwd("E:/Git/github/r-scripts/analysis/") # please update path
> #setwd("/Users/sebastian/git/github/r-scripts/analysis/")
  > 
  > library(data.table)
data.table 1.10.4.3
The fastest way to learn (by data.table authors): https://www.datacamp.com/courses/data-analysis-the-data-table-way
Documentation: ?data.table, example(data.table) and browseVignettes("data.table")
Release notes, videos and slides: http://r-datatable.com
> library(effsize)
> 
  > # lines added/deleted between postblock versions
  > postblockversion_linesdeleted <- fread("data/postblockversion_linesdeleted.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
Read 25544798 rows and 5 (of 5) columns from 0.925 GB file in 00:00:05
> #names(postblockversion_linesdeleted) <- c("PostId", "PostHistoryId", "PostBlockVersionId", "PredPostBlockVersionId", "LinesDeleted")
  > 
  > postblockversion_linesadded <- fread("data/postblockversion_linesadded.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
Read 24862260 rows and 5 (of 5) columns from 0.900 GB file in 00:00:05
> #names(postblockversion_linesadded) <- c("PostId", "PostHistoryId", "PostBlockVersionId", "PredPostBlockVersionId", "LinesAdded")
  > 
  > # merge data
  > postblockversion_linechange <- merge(postblockversion_linesadded, postblockversion_linesdeleted, by=c("PostId", "PostHistoryId", "PostBlockVersionId", "PredPostBlockVersionId"), all.x=TRUE, all.y=TRUE)
> postblockversion_linechange$LinesAdded[is.na(postblockversion_linechange$LinesAdded)] <- 0
> postblockversion_linechange$LinesDeleted[is.na(postblockversion_linechange$LinesDeleted)] <- 0
> summary(postblockversion_linechange$LinesAdded)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.000   1.000   1.000   1.158   1.000 319.000 
> summary(postblockversion_linechange$LinesDeleted)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.000   1.000   1.000   1.193   1.000 286.000 
> n <- nrow(postblockversion_linechange)
> n
[1] 26221182
> postblockversion_linechange_1 <- postblockversion_linechange[postblockversion_linechange$LinesAdded <= 1 & postblockversion_linechange$LinesDeleted <= 1,]
> n_1 <- nrow(postblockversion_linechange_1)
> n_1
[1] 22706077
> # 22,706,077
  > n_1/n*100
[1] 86.59441
> setwd("E:/Git/github/r-scripts/analysis/") # please update path
> #setwd("/Users/sebastian/git/github/r-scripts/analysis/")
  > 
  > library(data.table)
> library(effsize)
> 
  > # lines added/deleted between postblock versions
  > postblockversion_linesdeleted <- fread("data/postblockversion_linesdeleted.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
Read 25544798 rows and 5 (of 5) columns from 0.925 GB file in 00:00:05
> #names(postblockversion_linesdeleted) <- c("PostId", "PostHistoryId", "PostBlockVersionId", "PredPostBlockVersionId", "LinesDeleted")
  > 
  > postblockversion_linesadded <- fread("data/postblockversion_linesadded.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
Read 24862260 rows and 5 (of 5) columns from 0.900 GB file in 00:00:05
> #names(postblockversion_linesadded) <- c("PostId", "PostHistoryId", "PostBlockVersionId", "PredPostBlockVersionId", "LinesAdded")
  > 
  > # merge data
  > postblockversion_linechange <- merge(postblockversion_linesadded, postblockversion_linesdeleted, by=c("PostId", "PostHistoryId", "PostBlockVersionId", "PredPostBlockVersionId"), all.x=TRUE, all.y=TRUE)
> postblockversion_linechange$LinesAdded[is.na(postblockversion_linechange$LinesAdded)] <- 0
> postblockversion_linechange$LinesDeleted[is.na(postblockversion_linechange$LinesDeleted)] <- 0
> summary(postblockversion_linechange$LinesAdded)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.000   1.000   1.000   1.158   1.000 319.000 
> postblockversion_linechange[postblockversion_linechange$PostHistoryId==155295527,]
PostId PostHistoryId PostBlockVersionId PredPostBlockVersionId LinesAdded LinesDeleted
1: 5445161     155295527          161410760              161410757          1            1
> setwd("E:/Git/github/r-scripts/analysis/") # please update path
> #setwd("/Users/sebastian/git/github/r-scripts/analysis/")
  > 
  > library(data.table)
> library(effsize)
> 
  > # lines added/deleted between postblock versions
  > postblockversion_linesdeleted <- fread("data/postblockversion_linesdeleted.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
Read 25427554 rows and 5 (of 5) columns from 0.922 GB file in 00:00:05
> #names(postblockversion_linesdeleted) <- c("PostId", "PostHistoryId", "PostBlockVersionId", "PredPostBlockVersionId", "LinesDeleted")
  > 
  > postblockversion_linesadded <- fread("data/postblockversion_linesadded.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
Read 24719298 rows and 5 (of 5) columns from 0.896 GB file in 00:00:05
> #names(postblockversion_linesadded) <- c("PostId", "PostHistoryId", "PostBlockVersionId", "PredPostBlockVersionId", "LinesAdded")
  > 
  > # merge data
  > postblockversion_linechange <- merge(postblockversion_linesadded, postblockversion_linesdeleted, by=c("PostId", "PostHistoryId", "PostBlockVersionId", "PredPostBlockVersionId"), all.x=TRUE, all.y=TRUE)
> postblockversion_linechange$LinesAdded[is.na(postblockversion_linechange$LinesAdded)] <- 0
> postblockversion_linechange$LinesDeleted[is.na(postblockversion_linechange$LinesDeleted)] <- 0
> summary(postblockversion_linechange$LinesAdded)
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.000    1.000    1.000    2.886    2.000 1562.000 
> summary(postblockversion_linechange$LinesDeleted)
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.000    1.000    1.000    3.318    3.000 1323.000 
> n <- nrow(postblockversion_linechange)
> n
[1] 26145129
> postblockversion_linechange_1 <- postblockversion_linechange[postblockversion_linechange$LinesAdded <= 1 & postblockversion_linechange$LinesDeleted <= 1,]
> n_1 <- nrow(postblockversion_linechange_1)
> n_1
[1] 11536944
> # 11,536,944
  > n_1/n*100
[1] 44.12655
> postblockversion_type <- fread("data/postblockversion_type.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
Read 186924947 rows and 2 (of 2) columns from 1.986 GB file in 00:00:17
> names(postblockversion_type) <- c("PostBlockVersionId", "PostBlockTypeId")
> postblockversion_linechange <- merge(postblockversion_linechange, postblockversion_type, by="PostBlockVersionId")
> sd(postblockversion_linechange$LinesAdded)
[1] 8.40198
> sd(postblockversion_linechange$LinesDeleted)
[1] 9.004448
> postblockversion_linechange_text <- postblockversion_linechange[postblockversion_linechange$PostBlockTypeId==1,]
> 
  > summary(postblockversion_linechange_text$LinesAdded)
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.000    1.000    1.000    1.737    2.000 1002.000 
> n <- nrow(postblockversion_linechange_text)
> n
[1] 18883039
> postblockversion_linechange_1_text <- postblockversion_linechange_text[postblockversion_linechange_text$LinesAdded <= 1 & postblockversion_linechange_text$LinesDeleted <= 1,]
> n_1 <- nrow(postblockversion_linechange_1_text)
> n_1
[1] 8999884
> # 8,999,884
  > n_1/n*100
[1] 47.66121
> # code blocks
  > postblockversion_linechange_code <- postblockversion_linechange[postblockversion_linechange$PostBlockTypeId==2,]
> summary(postblockversion_linechange_code$LinesAdded)
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
0.000    1.000    2.000    5.875    5.000 1562.000 
> n <- nrow(postblockversion_linechange_code)
> n
[1] 7262090
> postblockversion_linechange_1_code <- postblockversion_linechange_code[postblockversion_linechange_code$LinesAdded <= 1 & postblockversion_linechange_code$LinesDeleted <= 1,]
> n_1 <- nrow(postblockversion_linechange_1_code)
> n_1
[1] 2537060
> # 2,537,060
  > n_1/n*100
[1] 34.93567
> # lines added
  > wilcox.test(postblockversion_linechange_code$LinesAdded,
                +             postblockversion_linechange_text$LinesAdded,
                +             alternative="two.sided",
                +             paired=F,correct=T)

Wilcoxon rank sum test with continuity correction

data:  postblockversion_linechange_code$LinesAdded and postblockversion_linechange_text$LinesAdded
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
