setwd("F:/Git/github/r-scripts/analysis/") # please update path
#setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)
library(effsize)

# use defined colors
source("../colors.R")

# posts with version count + first/last post history id
posts_versioncount <- fread("data/posts_versioncount.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(posts_versioncount) <- c("PostId", "PostTypeId", "VersionCount", "FirstPostHistoryId", "LastPostHistoryId")

# text block length
posts_textblock_length <- fread("data/textblock_length.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(posts_textblock_length) <- c("PostId", "PostHistoryId", "PostBlockVersionId", "Length", "LineCount")

# code block length
posts_codeblock_length <- fread("data/codeblock_length.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(posts_codeblock_length) <- c("PostId", "PostHistoryId", "PostBlockVersionId", "Length", "LineCount")

# retrieve text blocks in first version
textblocks_first_version <- posts_textblock_length[posts_textblock_length$PostHistoryId %in% posts_versioncount$FirstPostHistoryId]

# retrieve text blocks in last version
textblocks_last_version <- posts_textblock_length[posts_textblock_length$PostHistoryId %in% posts_versioncount$LastPostHistoryId]

# retrieve code blocks in first version
codeblocks_first_version <- posts_codeblock_length[posts_codeblock_length$PostHistoryId %in% posts_versioncount$FirstPostHistoryId]

# retrieve code blocks in last version
codeblocks_last_version <- posts_codeblock_length[posts_codeblock_length$PostHistoryId %in% posts_versioncount$LastPostHistoryId]


##########
# text
##########
nrow(textblocks_last_version)
# 69,940,599

summary(textblocks_last_version$LineCount)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 1.000    1.000    2.000    2.451    3.000 1608.000 
sd(textblocks_last_version$LineCount)
# 3.13954

summary(textblocks_last_version$Length)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0    62.0   153.0   247.5   319.0 29830.0 
sd(textblocks_last_version$Length)
# 319.1321


##########
# code
##########
nrow(codeblocks_last_version)
# 42,568,011
summary(codeblocks_last_version$LineCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1       2       5      12      13    2576 
sd(codeblocks_last_version$LineCount)
# 23.38394

summary(codeblocks_last_version$Length)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0    82.0   194.0   455.9   458.0 30010.0 
sd(codeblocks_last_version$Length)
# 989.2887


##########
# plots
##########

boxplot(textblocks_last_version$Length, codeblocks_last_version$Length, outline=FALSE)

# plot boxplots
#quartz(type="pdf", file="figures/exact_matches_so_filter_histograms.pdf", width=12, height=10) # prevents unicode issues in pdf
pdf("figures/postblocklength_latest.pdf", width=6, height=5)
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
boxplot(textblocks_last_version$LineCount,
        codeblocks_last_version$LineCount,
        names=c("text blocks", "code blocks"),
        outline=FALSE,
        col=gray_lighter,
        main="Length of post blocks in last version"
)
title(ylab="Line count", font.lab=1)

dev.off()

##########
# first vs. last version
##########

# text
wilcox.test(textblocks_last_version$LineCount,
            textblocks_first_version$LineCount,
            alternative="two.sided",
            paired=F, correct=T)
# W = 2.2852e+15, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cohen.d(textblocks_last_version$LineCount, # "treatment"
        textblocks_first_version$LineCount, # "control"
        paired=FALSE)
# d estimate: 0.0001946091 (negligible)
# 95 percent confidence interval:
#   inf sup 
# NA  NA 

wilcox.test(textblocks_last_version$Length,
            textblocks_first_version$Length,
            alternative="two.sided",
            paired=F, correct=T)
# W = 2.2704e+15, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cohen.d(textblocks_last_version$Length, # "treatment"
        textblocks_first_version$Length, # "control"
        paired=FALSE)
# d estimate: 0.01040951 (negligible)
# 95 percent confidence interval:
#   inf sup 
# NA  NA 


# code
wilcox.test(codeblocks_last_version$LineCount,
            codeblocks_first_version$LineCount,
            alternative="two.sided",
            paired=F, correct=T)
# W = 8.0157e+14, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cohen.d(codeblocks_last_version$LineCount, # "treatment"
        codeblocks_first_version$LineCount, # "control"
        paired=FALSE)
# d estimate: 0.01230378 (negligible)
# 95 percent confidence interval:
#   inf sup 
# NA  NA 

wilcox.test(codeblocks_last_version$Length,
            codeblocks_first_version$Length,
            alternative="two.sided",
            paired=F, correct=T)
# W = 8.028e+14, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cohen.d(codeblocks_last_version$Length, # "treatment"
        codeblocks_first_version$Length, # "control"
        paired=FALSE)
# d estimate: 0.01506673 (negligible)
# 95 percent confidence interval:
#   inf sup 
# NA  NA
