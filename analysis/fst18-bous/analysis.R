# setwd("") # please update path

library(data.table)

# links from GH files to SO Java posts (including Score and CommentCount of posts)
postreferencegh_java <- fread("data/PostReferenceGH_Java.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(postreferencegh_java) <- c("FileId", "RepoName", "Branch", "Path", "FileExt", "Size", "Copies", "PostId", "PostTypeId", "Score", "CommentCount", "SOUrl", "GHUrl")

# links collected from SO Java posts
postversionurl_java <- fread("data/PostVersionUrl_Java.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(postversionurl_java) <- c("Id", "PostId", "PostTypeId", "PostHistoryId", "PostBlockVersionId", "Url")

# TODO: quantitative analysis, draw sample(s) for qualitative analysis

# write sample to CSV files
write.table(dataframe, file="data/sample.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
