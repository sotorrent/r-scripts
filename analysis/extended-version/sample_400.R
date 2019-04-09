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

output_dir <- "data/sample_400"
if (dir.exists(output_dir)) {
  unlink(output_dir, recursive=TRUE)
}
dir.create(output_dir)

# read text block content
sample_400_textblocks <- fread("data/Sample400_Posts_TextBlocks.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
nrow(sample_400_textblocks)
# 2049

# process and export text block content
for (i in 1:nrow(sample_400_textblocks)) {
  sample_400_textblocks[i]$TextBlockContent <- str_replace_all(sample_400_textblocks[i]$TextBlockContent, "&#xD;&#xA;", "\n") # replace escaped newlines
  sample_400_textblocks[i]$TextBlockContent <- str_replace_all(sample_400_textblocks[i]$TextBlockContent, "\"\"", "\"") # replace escaped double quotes
  sample_400_textblocks[i]$TextBlockContent <- str_replace_all(sample_400_textblocks[i]$TextBlockContent, "`[^`]+`[:blank:]*", "") # remove inline code
  sample_400_textblocks[i]$TextBlockContent <- str_replace_all(sample_400_textblocks[i]$TextBlockContent, "\\[([^\\]]+)\\]+\\((?:http|HTTP)[^\\)[:blank:]]+\\)", "\\1") # replace Markdown links with anchor text 
  sample_400_textblocks[i]$TextBlockContent <- str_replace_all(sample_400_textblocks[i]$TextBlockContent, "!?\\[([^\\]]+)\\]+\\[[^\\][:space:]]+\\]", "\\1") # replace reference-style Markdown links with anchor text
  # apply twice because they may be nested, e.g. [![Snapshot what I did!][1]][1]
  sample_400_textblocks[i]$TextBlockContent <- str_replace_all(sample_400_textblocks[i]$TextBlockContent, "!?\\[([^\\]]+)\\]+\\[[^\\][:space:]]+\\]", "\\1") # replace reference-style Markdown links with anchor text
  sample_400_textblocks[i]$TextBlockContent <- str_replace_all(sample_400_textblocks[i]$TextBlockContent, "\\[[^\\]]+\\]+:[:blank:]+(?:http|HTTP)[^\\)[:space:]]+", "") # remove Markdown link references
  sample_400_textblocks[i]$TextBlockContent <- str_replace_all(sample_400_textblocks[i]$TextBlockContent, "(?:http|HTTP)[^[:space:]]+[:blank:]*", "") # remove remaining links
  sample_400_textblocks[i]$TextBlockContent <- markdown_text(sample_400_textblocks[i]$TextBlockContent)

  post_type = ""
  if (sample_400_textblocks[i]$PostId == sample_400_textblocks[i]$ParentId) { # question
    post_type = "q"
  } else {
    post_type = "a"
  }
  file_path <- paste0(output_dir, "/", sample_400_textblocks[i]$ParentId, "-", sample_400_textblocks[i]$PostId, "-", post_type, "-", sample_400_textblocks[i]$PostHistoryId, ".txt") 
  write(sample_400_textblocks[i]$TextBlockContent, file=file_path)  
}

write.table(sample_400_textblocks, file=paste0("data/Sample400_Posts_TextBlocks_processed.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

# read comments
sample_400_comments <- fread("data/Sample400_Posts_Comments.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
nrow(sample_400_comments)
# 2255

# process and export text block content
for (i in 1:nrow(sample_400_comments)) {
  sample_400_comments[i]$CommentContent <- str_replace_all(sample_400_comments[i]$CommentContent, "&#xD;&#xA;", "\n") # replace escaped newlines
  sample_400_comments[i]$CommentContent <- str_replace_all(sample_400_comments[i]$CommentContent, "\"\"", "\"") # replace escaped double quotes
  sample_400_comments[i]$CommentContent <- str_replace_all(sample_400_comments[i]$CommentContent, "`[^`]+`[:blank:]*", "") # remove inline code
  sample_400_comments[i]$CommentContent <- str_replace_all(sample_400_comments[i]$CommentContent, "\\[([^\\]]+)\\]+\\((?:http|HTTP)[^\\)[:blank:]]+\\)", "\\1") # replace Markdown links with anchor text 
  sample_400_comments[i]$CommentContent <- str_replace_all(sample_400_comments[i]$CommentContent, "!?\\[([^\\]]+)\\]+\\[[^\\][:space:]]+\\]", "\\1") # replace reference-style Markdown links with anchor text
  # apply twice because they may be nested, e.g. [![Snapshot what I did!][1]][1]
  sample_400_comments[i]$CommentContent <- str_replace_all(sample_400_comments[i]$CommentContent, "!?\\[([^\\]]+)\\]+\\[[^\\][:space:]]+\\]", "\\1") # replace reference-style Markdown links with anchor text
  sample_400_comments[i]$CommentContent <- str_replace_all(sample_400_comments[i]$CommentContent, "\\[[^\\]]+\\]+:[:blank:]+(?:http|HTTP)[^\\)[:space:]]+", "") # remove Markdown link references
  sample_400_comments[i]$CommentContent <- str_replace_all(sample_400_comments[i]$CommentContent, "(?:http|HTTP)[^[:space:]]+[:blank:]*", "") # remove remaining links
  sample_400_comments[i]$CommentContent <- markdown_text(sample_400_comments[i]$CommentContent)
  
  file_path <- paste0(output_dir, "/", sample_400_comments[i]$ParentId, "-", sample_400_comments[i]$PostId, "-c-", sample_400_comments[i]$CommentId, ".txt") 
  write(sample_400_comments[i]$CommentContent, file=file_path)  
}

write.table(sample_400_comments, file=paste0("data/Sample400_Posts_Comments_processed.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
