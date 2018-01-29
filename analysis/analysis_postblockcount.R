# setwd("F:/Git/github/r-scripts/analysis/") # please update path
setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)
library(effsize)

# use defined colors
source("../colors.R")

# posts with version count + first/last post history id
posts_versioncount <- fread("data/posts_versioncount.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(posts_versioncount) <- c("PostId", "PostTypeId", "VersionCount", "FirstPostHistoryId", "LastPostHistoryId")

# post histories with text block count
posts_textblockcount <- fread("data/posts_textblockcount.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(posts_textblockcount) <- c("PostId", "PostHistoryId", "TextBlockCount")

# post histories with code block count
posts_codeblockcount <- fread("data/posts_codeblockcount.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(posts_codeblockcount) <- c("PostId", "PostHistoryId", "CodeBlockCount")

# add FirstTextBlockCount
posts_versioncount_postblockcount <- merge(posts_versioncount, posts_textblockcount, by.x="FirstPostHistoryId", by.y="PostHistoryId", all.x=TRUE)
posts_versioncount_postblockcount <- posts_versioncount_postblockcount[,c("PostId.x", "PostTypeId", "VersionCount", "FirstPostHistoryId", "TextBlockCount", "LastPostHistoryId")]
names(posts_versioncount_postblockcount) <- c("PostId", "PostTypeId", "VersionCount", "FirstPostHistoryId", "FirstTextBlockCount", "LastPostHistoryId")

# add LastTextBlockCount
posts_versioncount_postblockcount <- merge(posts_versioncount_postblockcount, posts_textblockcount, by.x="LastPostHistoryId", by.y="PostHistoryId", all.x=TRUE)
posts_versioncount_postblockcount <- posts_versioncount_postblockcount[,c("PostId.x", "PostTypeId", "VersionCount", "FirstPostHistoryId", "FirstTextBlockCount", "LastPostHistoryId", "TextBlockCount")]
names(posts_versioncount_postblockcount) <- c("PostId", "PostTypeId", "VersionCount", "FirstPostHistoryId", "FirstTextBlockCount", "LastPostHistoryId", "LastTextBlockCount")

# add FirstCodeBlockCount
posts_versioncount_postblockcount <- merge(posts_versioncount_postblockcount, posts_codeblockcount, by.x="FirstPostHistoryId", by.y="PostHistoryId", all.x=TRUE)
posts_versioncount_postblockcount <- posts_versioncount_postblockcount[,c("PostId.x", "PostTypeId", "VersionCount", "FirstPostHistoryId", "FirstTextBlockCount", "CodeBlockCount", "LastPostHistoryId", "LastTextBlockCount")]
names(posts_versioncount_postblockcount) <- c("PostId", "PostTypeId", "VersionCount", "FirstPostHistoryId", "FirstTextBlockCount", "FirstCodeBlockCount", "LastPostHistoryId", "LastTextBlockCount")

# add LastCodeBlockCount
posts_versioncount_postblockcount <- merge(posts_versioncount_postblockcount, posts_codeblockcount, by.x="LastPostHistoryId", by.y="PostHistoryId", all.x=TRUE)
posts_versioncount_postblockcount <- posts_versioncount_postblockcount[,c("PostId.x", "PostTypeId", "VersionCount", "FirstPostHistoryId", "FirstTextBlockCount", "FirstCodeBlockCount", "LastPostHistoryId", "LastTextBlockCount", "CodeBlockCount")]
names(posts_versioncount_postblockcount) <- c("PostId", "PostTypeId", "VersionCount", "FirstPostHistoryId", "FirstTextBlockCount", "FirstCodeBlockCount", "LastPostHistoryId", "LastTextBlockCount", "LastCodeBlockCount")

# replace NAs with 0 (versions without text/code blocks)
posts_versioncount_postblockcount$FirstTextBlockCount[is.na(posts_versioncount_postblockcount$FirstTextBlockCount)] <- 0
posts_versioncount_postblockcount$LastTextBlockCount[is.na(posts_versioncount_postblockcount$LastTextBlockCount)] <- 0
posts_versioncount_postblockcount$FirstCodeBlockCount[is.na(posts_versioncount_postblockcount$FirstCodeBlockCount)] <- 0
posts_versioncount_postblockcount$LastCodeBlockCount[is.na(posts_versioncount_postblockcount$LastCodeBlockCount)] <- 0

nrow(posts_versioncount_postblockcount)
# 38,394,895

# analyze latest version of all posts

##########
# text
##########
summary(posts_versioncount_postblockcount$LastTextBlockCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.000   1.000   1.822   2.000 256.000 

TextBlockCount <- ifelse(posts_versioncount_postblockcount$LastTextBlockCount>6, 6, posts_versioncount_postblockcount$LastTextBlockCount)
TextBlockCountTable <- table(TextBlockCount)

TextBlockCountTable["0"]/nrow(posts_versioncount_postblockcount)*100
# 1.003602

##########
# code
##########
summary(posts_versioncount_postblockcount$LastCodeBlockCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   1.000   1.109   2.000 256.000 

CodeBlockCount <- ifelse(posts_versioncount_postblockcount$LastCodeBlockCount>6, 6, posts_versioncount_postblockcount$LastCodeBlockCount)
CodeBlockCountTable <- table(CodeBlockCount)

CodeBlockCountTable["0"]/nrow(posts_versioncount_postblockcount)*100
# 36.55571

##########
# Plots
##########

boxplot(posts_versioncount_postblockcount$LastTextBlockCount, posts_versioncount_postblockcount$LastCodeBlockCount, outline=FALSE)

# plot histogram + boxplot
quartz(type="pdf", file="figures/postblockcount_latest.pdf", width=14, height=5) # prevents unicode issues in pdf
#pdf("figures/postblockcount_latest.pdf", width=14, height=5)
par(
  bg="white",
  #mar = c(3, 3, 3, 1)+0.1, # subplot margins (bottom, left, top, right)
  #  omi = c(0.0, 0.0, 0.0, 0.0),  # outer margins in inches (bottom, left, top, right)
  mfrow = c(1, 2),
  #pin = (width, height)
  #mfcol # draw in columns
  # increase font size
  cex=1.3,
  cex.main=1.3,
  cex.sub=1,
  cex.lab=1,
  cex.axis=1
)

hist(TextBlockCount, 
     main="Text blocks in latest version  (n=38,394,895)", 
     freq=TRUE,
     xlab="",
     ylab="",
     border="white",
     col="white",
     #labels=c(rep("", 10), "Selected"),
     xlim=c(-1,6),
     ylim=c(0, 20000000),
     breaks=-1:6,
     xaxt="n",
     yaxt="n"
)
for (y in seq(0, 20000000, by=5000000)) {
  segments(x0=-1.5, y0=y, x1=6, y1=y, lty=1, lwd=1, col=gray_lighter)
}
hist(TextBlockCount,
     add=TRUE,
     main="", 
     freq=TRUE,
     xlab="x",
     ylab="y",
     border=gray_dark,
     col=gray_lighter,
     #labels=c(rep("", 10), "Selected"),
     xlim=c(-1,6),
     ylim=c(0, 20000000),
     breaks=-1:6,
     xaxt="n",
     yaxt="n"
)
boxplot(TextBlockCount-0.5,
        add=TRUE,
        outline=FALSE,
        horizontal=TRUE,
        ylim=c(0,6),
        log="",
        col="white",
        # https://stackoverflow.com/a/28890111
        lwd=2,
        medlwd=2,
        #staplelty=0,
        whisklty=1,
        #staplelty=0,
        whiskcol=gray_darker,
        medcol=gray_darker,
        boxcol=gray_darker,
        staplecol=gray_darker,
        boxwex=1800000,
        axes=FALSE
        #xaxt="n"
        #yaxt="n"
)
# median
abline(v=0.5, lty=1, lwd=2, col=gray_darker) 
# axes
axis(1, at=seq(-0.5, 5.5, by=1), labels=c(seq(0, 5, by=1), "\u2265 6"))
axis(2, at=seq(0, 20000000, by=5000000), labels=c("0", "5m", "10m", "15m", "20m"), las=2)


hist(CodeBlockCount, 
     main="Code blocks in latest version (n=38,394,895)", 
     freq=TRUE,
     xlab="",
     ylab="",
     border="white",
     col="white",
     #labels=c(rep("", 10), "Selected"),
     xlim=c(-1,6),
     ylim=c(0, 20000000),
     breaks=-1:6,
     xaxt="n",
     yaxt="n"
)
for (y in seq(0, 20000000, by=5000000)) {
  segments(x0=-1.5, y0=y, x1=6, y1=y, lty=1, lwd=1, col=gray_lighter)
}
hist(CodeBlockCount,
     add=TRUE,
     main="", 
     freq=TRUE,
     xlab="x",
     ylab="y",
     border=gray_dark,
     col=gray_lighter,
     #labels=c(rep("", 10), "Selected"),
     xlim=c(-1,6),
     ylim=c(0, 20000000),
     breaks=-1:6,
     xaxt="n",
     yaxt="n"
)
boxplot(CodeBlockCount-0.5,
        add=TRUE,
        outline=FALSE,
        horizontal=TRUE,
        ylim=c(0,6),
        log="",
        col="white",
        # https://stackoverflow.com/a/28890111
        lwd=2,
        medlwd=2,
        #staplelty=0,
        whisklty=1,
        #staplelty=0,
        whiskcol=gray_darker,
        medcol=gray_darker,
        boxcol=gray_darker,
        staplecol=gray_darker,
        boxwex=1800000,
        axes=FALSE
        #xaxt="n"
        #yaxt="n"
)
# median
abline(v=0.5, lty=1, lwd=2, col=gray_darker) 
# axes
axis(1, at=seq(-0.5, 5.5, by=1), labels=c(seq(0, 5, by=1), "\u2265 6"))
axis(2, at=seq(0, 20000000, by=5000000), labels=c("0", "5m", "10m", "15m", "20m"), las=2)

dev.off()




##############################
# compare first and last version of all posts with edits (versioncount > 0)
##############################
posts_with_edits <- posts_versioncount_postblockcount[posts_versioncount_postblockcount$VersionCount > 0,]

##########
# text
##########
summary(posts_with_edits$FirstTextBlockCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.000   1.000   1.698   2.000 327.000 

sd(posts_with_edits$FirstTextBlockCount)
# 1.104623

summary(posts_with_edits$LastTextBlockCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.000   1.000   1.822   2.000 256.000 

sd(posts_with_edits$LastTextBlockCount)
# 1.214417

wilcox.test(posts_with_edits$FirstTextBlockCount, posts_with_edits$LastTextBlockCount, alternative="two.sided", paired=T, correct=T)
# V = 8.4289e+11, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

#cliff.delta(posts_with_edits$FirstTextBlockCount, posts_with_edits$LastTextBlockCount)
# too slow...

cohen.d(posts_with_edits$LastTextBlockCount, # "treatment"
        posts_with_edits$FirstTextBlockCount, # "control"
        paired=TRUE)
# d estimate: 0.2071768 (small)
# 95 percent confidence interval:
#   inf sup 
# NA  NA 
# Warning message:
#   In n1 * n2 : NAs produced by integer overflow


##########
# code
##########
summary(posts_with_edits$FirstCodeBlockCount)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0000   0.0000   1.0000   0.9726   1.0000 326.0000 

sd(posts_with_edits$FirstCodeBlockCount)
# 1.13207

summary(posts_with_edits$LastCodeBlockCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   1.000   1.109   2.000 256.000 

sd(posts_with_edits$LastCodeBlockCount)
# 1.249302

wilcox.test(posts_with_edits$FirstCodeBlockCount, posts_with_edits$LastCodeBlockCount, alternative="two.sided", paired=T, correct=T)
# V = 5.7747e+11, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cohen.d(posts_with_edits$LastCodeBlockCount, # "treatment"
        posts_with_edits$FirstCodeBlockCount, # "control"
        paired=TRUE)
# d estimate: 0.227515 (small)
# 95 percent confidence interval:
#   inf sup 
# NA  NA 
# Warning message:
#   In n1 * n2 : NAs produced by integer overflow


##########
# questions vs. answers
##########

questions <- posts_versioncount_postblockcount[posts_versioncount_postblockcount$PostTypeId == 1,]

summary(questions$LastTextBlockCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.000   2.000   2.139   3.000 256.000 
sd(questions$LastTextBlockCount)
# 1.346668

summary(questions$LastCodeBlockCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   1.000   1.348   2.000 256.000 
sd(questions$LastCodeBlockCount)
# 1.371103

answers <- posts_versioncount_postblockcount[posts_versioncount_postblockcount$PostTypeId == 2,]

summary(answers$LastTextBlockCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.000   1.000   1.618   2.000  89.000
sd(answers$LastTextBlockCount)
# 1.073218

summary(answers$LastCodeBlockCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  1.0000  0.9554  1.0000 88.0000 
sd(answers$LastCodeBlockCount)
# 1.138441


# difference text

wilcox.test(answers$LastTextBlockCount,
            questions$LastTextBlockCount,
            alternative="two.sided",
            paired=F, correct=T)
# W = 1.3244e+14, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cohen.d(answers$LastTextBlockCount, # "treatment"
        questions$LastTextBlockCount, # "control"
        paired=FALSE)
# d estimate: -0.4385735 (small)
# 95 percent confidence interval:
#   inf sup 
# NA  NA 

# difference code

wilcox.test(answers$LastCodeBlockCount,
            questions$LastCodeBlockCount,
            alternative="two.sided",
            paired=F, correct=T)
# W = 1.4527e+14, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cohen.d(answers$LastCodeBlockCount, # "treatment"
        questions$LastCodeBlockCount, # "control"
        paired=FALSE)
# d estimate: -0.3179121 (small)
# 95 percent confidence interval:
#   inf sup 
# NA  NA 


##########
# java vs. others
##########

# post ids of Java questions
java_questions <- fread("data/java_questions.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(java_questions) <- c("PostId", "PostTypeId")
# post ids of Java answers
java_answers <- fread("data/java_answers.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(java_answers) <- c("PostId", "PostTypeId")
# merge post ids
java_post_ids <- c(java_questions$PostId, java_answers$PostId)

java <- posts_versioncount_postblockcount[posts_versioncount_postblockcount$PostId %in% java_post_ids,]
others <- posts_versioncount_postblockcount[!(posts_versioncount_postblockcount$PostId %in% java_post_ids),]


summary(java$LastTextBlockCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.000   1.000   1.815   2.000  96.000
sd(java$LastTextBlockCount)
# 1.229142

summary(java$LastCodeBlockCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   1.000   1.089   2.000  96.000 
sd(java$LastCodeBlockCount)
# 1.267344


# difference text

wilcox.test(java$LastTextBlockCount,
            others$LastTextBlockCount,
            alternative="two.sided",
            paired=F, correct=T)
# W = 6.2548e+13, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cohen.d(java$LastTextBlockCount, # "treatment"
        others$LastTextBlockCount, # "control"
        paired=FALSE)
# d estimate: -0.006347346 (negligible)
# 95 percent confidence interval:
#   inf sup 
# NA  NA 


# difference code

wilcox.test(java$LastCodeBlockCount,
            others$LastCodeBlockCount,
            alternative="two.sided",
            paired=F, correct=T)
# W = 6.2064e+13, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cohen.d(java$LastCodeBlockCount, # "treatment"
        others$LastCodeBlockCount, # "control"
        paired=FALSE)
# d estimate: -0.01770614 (negligible)
# 95 percent confidence interval:
#   inf sup 
# NA  NA 
