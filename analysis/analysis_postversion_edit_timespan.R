setwd("F:/Git/github/r-scripts/analysis/") # please update path
#setwd("/Users/sebastian/git/github/r-scripts/analysis/")

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
# 283

postversion_edits_succ <- postversion_edits_succ[postversion_edits_succ$SuccCreationDateDiff >= 0,]
n <- nrow(postversion_edits_succ)
n
# 21,840,111

summary(postversion_edits_succ$SuccCreationDateDiff)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.0     0.0   134.4     0.0  3375.0
sd(postversion_edits_succ$SuccCreationDateDiff)
# 421.4525

n_0 <- length(postversion_edits_succ$SuccCreationDateDiff[postversion_edits_succ$SuccCreationDateDiff==0])
n_0
# 17,084,794
n_0/n*100
# 78.22668

n_week <- length(postversion_edits_succ$SuccCreationDateDiff[postversion_edits_succ$SuccCreationDateDiff > 0 & postversion_edits_succ$SuccCreationDateDiff <= 7])
n_week
# 1,128,962
n_week/n*100
# 5.169214

n_year <- length(postversion_edits_succ$SuccCreationDateDiff[postversion_edits_succ$SuccCreationDateDiff > 7 & postversion_edits_succ$SuccCreationDateDiff <= 365])
n_year
# 1,342,329
n_year/n*100
# 6.146164

n_more_than_one_year <- length(postversion_edits_succ$SuccCreationDateDiff[postversion_edits_succ$SuccCreationDateDiff > 365])
n_more_than_one_year
# 664,515
n_more_than_one_year/n*100
# 3.042636



##########
# first edit
##########
postversion_edits_succ_first <- sqldf("select PostId, min(PostHistoryId) PostHistoryId, SuccCreationDateDiff from postversion_edits_succ group by PostId")
n <- nrow(postversion_edits_succ_first)
n
# 13,874,674

summary(postversion_edits_succ_first$SuccCreationDateDiff)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.0     0.0   125.9     0.0  3375.0 

n_0 <- length(postversion_edits_succ_first$SuccCreationDateDiff[postversion_edits_succ_first$SuccCreationDateDiff==0])
n_0
# 11,177,661
n_0/n*100
# 80.56161

n_week <- length(postversion_edits_succ_first$SuccCreationDateDiff[postversion_edits_succ_first$SuccCreationDateDiff > 0 & postversion_edits_succ_first$SuccCreationDateDiff <= 7])
n_week
# 636,871
n_week/n*100
# 4.590169

n_year <- length(postversion_edits_succ_first$SuccCreationDateDiff[postversion_edits_succ_first$SuccCreationDateDiff > 7 & postversion_edits_succ_first$SuccCreationDateDiff <= 365])
n_year
# 711,703
n_year/n*100
# 5.129512

n_more_than_year <- length(postversion_edits_succ_first$SuccCreationDateDiff[postversion_edits_succ_first$SuccCreationDateDiff > 365])
n_more_than_year
# 1,348,439
n_more_than_year/n*100
# 9.718708


##########
# later edits
##########
postversion_edits_succ_later <- sqldf("select PostId, PostHistoryId, SuccCreationDateDiff from postversion_edits_succ where PostHistoryId not in (select min(PostHistoryId) PostHistoryId from postversion_edits_succ group by PostId)")
n <- nrow(postversion_edits_succ_later)
n
# 7,965,437

summary(postversion_edits_succ_later$SuccCreationDateDiff)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.0     0.0   149.4     1.0  3374.0 

n_0 <- length(postversion_edits_succ_later$SuccCreationDateDiff[postversion_edits_succ_later$SuccCreationDateDiff==0])
n_0
# 5,907,133
n_0/n*100
# 74.15956

n_week <- length(postversion_edits_succ_later$SuccCreationDateDiff[postversion_edits_succ_later$SuccCreationDateDiff > 0 & postversion_edits_succ_later$SuccCreationDateDiff <= 7])
n_week
# 492,091
n_week/n*100
# 6.177828

n_year <- length(postversion_edits_succ_later$SuccCreationDateDiff[postversion_edits_succ_later$SuccCreationDateDiff > 7 & postversion_edits_succ_later$SuccCreationDateDiff <= 365])
n_year
# 630,626
n_year/n*100
# 7.91703

n_more_than_year <- length(postversion_edits_succ_later$SuccCreationDateDiff[postversion_edits_succ_later$SuccCreationDateDiff > 365])
n_more_than_year
# 935,587
n_more_than_year/n*100
# 11.74558


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
# -147.00     0.00     2.00    11.61     5.00 19900.00 



##########
# plot
##########

date_diff <- postversion_edits_succ$SuccCreationDateDiff
max_weeks <- 8
for (i in 1:max_weeks) {
  date_diff[date_diff>(i-1)*7 & date_diff<=i*7] <- i 
}
date_diff[date_diff>max_weeks*7] <- max_weeks+1
date_diff_table <- table(date_diff)

pdf("figures/timespan_weeks.pdf", width=8, height=6)
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

#options(scipen=5) # prevent scientific notation
gap.barplot(
  date_diff_table[1:max_weeks],
  gap=c(1500000, 16500000),
  ytics=c(0, 1000000, 17000000),
  col=rep(gray_selected, max_weeks+1),
  main="Timespan between edits (weeks)",
  xlab="Timespan (weeks)",
  ylab="Number of edits"
)

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
