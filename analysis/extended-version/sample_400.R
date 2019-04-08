# set working directory (see https://stackoverflow.com/a/35842119)
dir = tryCatch({
  # script being sourced
  getSrcDirectory()[1]
}, error = function(e) {
  # script being run in RStudio
  dirname(rstudioapi::getActiveDocumentContext()$path)
})
setwd(dir)

library(data.table)
library(stringr)
library(commonmark)

# read data
sample_400 <- fread("data/EditedThreads_Sample400TextBlocks.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
nrow(sample_400)
# 865
length(unique(sample_400$PostId))
# 399 (one post deleted in the meantime)

for (i in 1:nrow(sample_400)) {
  sample_400[i]$TextBlockContent <- str_replace_all(sample_400[i]$TextBlockContent, "&#xD;&#xA;", "\n")
  sample_400[i]$TextBlockContent <- str_replace_all(sample_400[i]$TextBlockContent, "\"\"", "\"") # replace escaped double quotes
  sample_400[i]$TextBlockContent <- markdown_text(sample_400[i]$TextBlockContent)
}

write.table(sample_400, file=paste0("data/EditedThreads_Sample400TextBlocks_parsed.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

