setwd("F:/Git/github/r-scripts/analysis/") # please update path
#setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)
library(effsize)

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

# analyze latest version of all posts
summary(posts_versioncount_postblockcount$LastTextBlockCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.000   1.000   1.822   2.000 256.000 

summary(posts_versioncount_postblockcount$LastCodeBlockCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   1.000   1.109   2.000 256.000 

boxplot(posts_versioncount_postblockcount$LastTextBlockCount, posts_versioncount_postblockcount$LastCodeBlockCount, outline=FALSE)

# compare first an last version of all posts with edits (versioncount > 0)
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

cliff.delta(posts_with_edits$FirstTextBlockCount, posts_with_edits$LastTextBlockCount)
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

