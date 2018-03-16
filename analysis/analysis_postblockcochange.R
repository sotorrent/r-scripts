setwd("E:/Git/github/r-scripts/analysis/") # please update path
#setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)

# textblock edits
textblock_edits <- fread("data/textblock_edits.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(textblock_edits) <- c("PostId", "PostHistoryId", "TextBlockEdits")

# codeblock edits
codeblock_edits <- fread("data/codeblock_edits.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(codeblock_edits) <- c("PostId", "PostHistoryId", "CodeBlockEdits")

# merge data to analyze co-change
postblock_edits <- merge(textblock_edits, codeblock_edits, by=c("PostId", "PostHistoryId"), all.x=TRUE, all.y=TRUE)
postblock_edits$TextBlockEdits[is.na(postblock_edits$TextBlockEdits)] <- 0
postblock_edits$CodeBlockEdits[is.na(postblock_edits$CodeBlockEdits)] <- 0

nrow(postblock_edits)
# 59,999,462
n <- length(unique(postblock_edits$PostHistoryId))
n
# 59,999,462

both_changed <- postblock_edits[postblock_edits$TextBlockEdits>0 & postblock_edits$CodeBlockEdits>0,]
nrow(both_changed)
# 29,600,395
nrow(both_changed)/n*100
# 49.33443

only_text_changed <- postblock_edits[postblock_edits$TextBlockEdits>0 & postblock_edits$CodeBlockEdits==0,]
nrow(only_text_changed)
# 26,736,309
nrow(only_text_changed)/n*100
# 44.56091

only_code_changed <- postblock_edits[postblock_edits$TextBlockEdits==0 & postblock_edits$CodeBlockEdits>0,]
nrow(only_code_changed)
# 3,662,758
nrow(only_code_changed)/n*100
# 6.104651

# text
summary(postblock_edits$TextBlockEdits)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.000   1.000   1.511   2.000 327.000 
sd(postblock_edits$TextBlockEdits)
# 1.058451

# code
summary(postblock_edits$CodeBlockEdits)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.0000   0.0000   1.0000   0.8508   1.0000 326.0000 
sd(postblock_edits$CodeBlockEdits)
# 1.050919

