#setwd("E:/Git/github/r-scripts/analysis/") # please update path
setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)
library(sqldf)
library(plotrix)

# use defined colors
source("../colors.R")

# textblock edits
postversion_edits <- fread("data/postversion_edits.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "\\N"), stringsAsFactors=FALSE)
names(postversion_edits) <- c("PostId", "PostTypeId", "PostHistoryId", "PostHistoryTypeId", "CreationDate", "SuccPostHistoryId", "SuccPostHistoryTypeId", "SuccCreationDate", "SuccCreationDateDiff")
# parse timestamps
postversion_edits$CreationDate <- as.POSIXct(postversion_edits$CreationDate, tz="UTC")

nrow(postversion_edits)
# 60,235,289

postversion_edits_succ <- postversion_edits[!is.na(postversion_edits$SuccCreationDateDiff),]
nrow(postversion_edits_succ)
# 21,840,394

broken_entries <- postversion_edits_succ[postversion_edits_succ$SuccCreationDateDiff < 0,]
nrow(broken_entries)
# 0

postversion_edits_succ <- postversion_edits_succ[postversion_edits_succ$SuccCreationDateDiff >= 0,]
n <- nrow(postversion_edits_succ)
n
# 21,840,394

summary(postversion_edits_succ$SuccCreationDateDiff)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.0     0.0   134.4     0.0  3375.0 
sd(postversion_edits_succ$SuccCreationDateDiff)
# 421.4498

n_0 <- length(postversion_edits_succ$SuccCreationDateDiff[postversion_edits_succ$SuccCreationDateDiff==0])
n_0
# 17,085,046
n_0/n*100
# 78.22682

n_week <- length(postversion_edits_succ$SuccCreationDateDiff[postversion_edits_succ$SuccCreationDateDiff > 0 & postversion_edits_succ$SuccCreationDateDiff <= 7])
n_week
# 1,128,993
n_week/n*100
# 5.169289

n_year <- length(postversion_edits_succ$SuccCreationDateDiff[postversion_edits_succ$SuccCreationDateDiff > 7 & postversion_edits_succ$SuccCreationDateDiff <= 365])
n_year
# 1,342,332
n_year/n*100
# 6.146098

n_more_than_one_year <- length(postversion_edits_succ$SuccCreationDateDiff[postversion_edits_succ$SuccCreationDateDiff > 365])
n_more_than_one_year
# 2,284,023
n_more_than_one_year/n*100
# 10.45779



##########
# first edit
##########
postversion_edits_succ_first <- sqldf("select PostId, min(PostHistoryId) PostHistoryId, SuccCreationDateDiff from postversion_edits_succ group by PostId")
n <- nrow(postversion_edits_succ_first)
n
# 13,874,688

summary(postversion_edits_succ_first$SuccCreationDateDiff)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.0     0.0   125.9     0.0  3375.0 

n_0 <- length(postversion_edits_succ_first$SuccCreationDateDiff[postversion_edits_succ_first$SuccCreationDateDiff==0])
n_0
# 11,177,672
n_0/n*100
# 80.56161

n_week <- length(postversion_edits_succ_first$SuccCreationDateDiff[postversion_edits_succ_first$SuccCreationDateDiff > 0 & postversion_edits_succ_first$SuccCreationDateDiff <= 7])
n_week
# 636,843
n_week/n*100
# 4.589963

n_year <- length(postversion_edits_succ_first$SuccCreationDateDiff[postversion_edits_succ_first$SuccCreationDateDiff > 7 & postversion_edits_succ_first$SuccCreationDateDiff <= 365])
n_year
# 711,690
n_year/n*100
# 5.129413

n_more_than_year <- length(postversion_edits_succ_first$SuccCreationDateDiff[postversion_edits_succ_first$SuccCreationDateDiff > 365])
n_more_than_year
# 1,348,483
n_more_than_year/n*100
# 9.719015


##########
# later edits
##########
postversion_edits_succ_later <- sqldf("select PostId, PostHistoryId, SuccCreationDateDiff from postversion_edits_succ where PostHistoryId not in (select min(PostHistoryId) PostHistoryId from postversion_edits_succ group by PostId)")
n <- nrow(postversion_edits_succ_later)
n
# 7,965,706

summary(postversion_edits_succ_later$SuccCreationDateDiff)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.0     0.0   149.3     1.0  3374.0 

n_0 <- length(postversion_edits_succ_later$SuccCreationDateDiff[postversion_edits_succ_later$SuccCreationDateDiff==0])
n_0
# 5,907,374
n_0/n*100
# 74.16008

n_week <- length(postversion_edits_succ_later$SuccCreationDateDiff[postversion_edits_succ_later$SuccCreationDateDiff > 0 & postversion_edits_succ_later$SuccCreationDateDiff <= 7])
n_week
# 492,150
n_week/n*100
# 6.17836

n_year <- length(postversion_edits_succ_later$SuccCreationDateDiff[postversion_edits_succ_later$SuccCreationDateDiff > 7 & postversion_edits_succ_later$SuccCreationDateDiff <= 365])
n_year
# 630,642
n_year/n*100
# 7.916963

n_more_than_year <- length(postversion_edits_succ_later$SuccCreationDateDiff[postversion_edits_succ_later$SuccCreationDateDiff > 365])
n_more_than_year
# 935,540
n_more_than_year/n*100
# 11.7446


##########
# score
##########
posts_score <- fread("data/posts_score.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(posts_score) <- c("PostId", "PostTypeId", "Score")

edits_after_one_year <- postversion_edits_succ[postversion_edits_succ$SuccCreationDateDiff > 365,]
post_ids <- unique(edits_after_one_year$PostId)

filtered_posts <- posts_score[posts_score$PostId %in% post_ids,]

summary(filtered_posts$Score)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -147.00     0.00     2.00    11.62     5.00 19904.00



##########
# plot
##########

max(postversion_edits_succ$SuccCreationDateDiff)
# 3375

date_diff <- postversion_edits_succ$SuccCreationDateDiff
max_weeks <- 8
for (i in 1:max_weeks) {
  date_diff[date_diff>(i-1)*7 & date_diff<=i*7] <- i-1 
}
date_diff[date_diff>max_weeks*7] <- max_weeks+1
date_diff <- date_diff+1 # to adjust x axis of plot
date_diff_table <- table(date_diff)

n <- length(date_diff)
n
# 21,840,394
n_1 <- length(date_diff[date_diff==1])
n_1
# 18,214,039
n_1/n*100
# 83.39611
n_excluded <- length(date_diff[date_diff==max_weeks+2])
n_excluded
# 3,162,390
n_excluded/n*100
# 14.47955
n-n_excluded
# 18,678,004
(1-n_excluded/n)*100
# 85.52045

# range of missing values
max(postversion_edits_succ$SuccCreationDateDiff/7)-max_weeks
# 474.1429


quartz(type="pdf", file="figures/timespan_weeks.pdf", width=8, height=6) # prevents unicode issues in pdf
#pdf("figures/timespan_weeks.pdf", width=8, height=6)
par(
  bg="white",
  #mar = c(3, 3, 3, 1)+0.1, # subplot margins (bottom, left, top, right)
  #  omi = c(0.0, 0.0, 0.0, 0.0),  # outer margins in inches (bottom, left, top, right)
  mfrow = c(1, 1),
  #pin = (width, height)
  #mfcol # draw in columns
  # increase font size
  cex=1.3,
  cex.main=1.3,
  cex.sub=1,
  cex.lab=1,
  cex.axis=1
)

options(scipen=5) # prevent scientific notation
gap.barplot(
  date_diff_table[1:max_weeks],
  gap=c(220000, 18100000),
  ytics=c(0, 100000, 200000, 18200000),
  xlim=c(0.6, 8.4),
  col=c(gray_selected, rep(gray_lighter, max_weeks)),
  main="Timespan between edits (weeks)",
  xlab="Timespan (weeks)",
  ylab="Number of edits"
)
# labels
text(2.2, 300000, "\u2190 83.4%", font=1, col=gray_darker) # cex=1.3

dev.off()


##########
# years
##########
max(postversion_edits_succ$SuccCreationDateDiff)/365
# 9.246575
date_diff <- postversion_edits_succ$SuccCreationDateDiff
max_years <- 9
for (i in 1:max_years) {
  date_diff[date_diff>(i-1)*365 & date_diff<=i*365] <- i 
}
date_diff[date_diff>max_years*365] <- max_years+1
date_diff_table <- table(date_diff)

pdf("figures/timespan_years.pdf", width=8, height=6)
par(
  bg="white",
  #mar = c(3, 3, 3, 1)+0.1, # subplot margins (bottom, left, top, right)
  #  omi = c(0.0, 0.0, 0.0, 0.0),  # outer margins in inches (bottom, left, top, right)
  mfrow = c(1, 1),
  #pin = (width, height)
  #mfcol # draw in columns
  # increase font size
  cex=1.3,
  cex.main=1.3,
  cex.sub=1,
  cex.lab=1,
  cex.axis=1
)

gap.barplot(
  date_diff_table,
  gap=c(2800000, 16500000),
  ytics=c(0, 1000000, 2000000, 17000000),
  col=rep(gray_selected, max_years+1),
  main="Timespan between edits (years)",
  xlab="Timespan (years)",
  ylab="Number of edits"
)

dev.off()
