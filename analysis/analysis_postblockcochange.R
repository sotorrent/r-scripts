setwd("F:/Git/github/r-scripts/analysis/") # please update path
#setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)

# text and code block co-change
text_and_code_edits <- fread("data/text_and_code_edits.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(text_and_code_edits) <- c("PostId", "PostHistoryId", "TextBlockEdits", "CodeBlockEdits")


