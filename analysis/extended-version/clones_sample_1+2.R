# set working directory (see https://stackoverflow.com/a/35842119)
dir = tryCatch({
  # script being sourced
  getSrcDirectory()[1]
}, error = function(e) {
  # script being run in RStudio
  dirname(rstudioapi::getActiveDocumentContext()$path)
})
setwd(dir)

# load libraries
library(data.table)
library(sqldf)


# read metadata
clones_sample <- fread("data/CodeClonesSample2.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
# prevent problems with SQLDF and integer64
clones_sample$ContentNormalizedHash <- as.character(clones_sample$ContentNormalizedHash)

hash_values <- unique(clones_sample$ContentNormalizedHash)
length(hash_values)
# 1014

# write metadata for each cloned code block
for (hash_value in hash_values) {
  clones <- sqldf(paste0("SELECT PostId, PostTypeId, ParentId, CreationDate FROM clones_sample WHERE ContentNormalizedHash=", hash_value))
  write.table(clones, file=paste0("export_sample2/", hash_value, ".csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
}


# read content
clones_sample_data <- fread("data/CodeClonesSample2Data.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
clones_sample_data$ContentNormalizedHash <- as.character(clones_sample_data$ContentNormalizedHash)
names(clones_sample_data) <- c("HashValue", "LineCount", "ThreadCount", "Content")

nrow(clones_sample_data)
# 1014

# write content for each cloned code block
for (hash_value in hash_values) {
    write.table(clones_sample_data[clones_sample_data$HashValue == hash_value,c("Content")], file=paste0("export_sample2/", hash_value, "_content.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
}

# write index file
write.table(clones_sample_data[order(-clones_sample_data$ThreadCount, -clones_sample_data$LineCount), c("HashValue", "ThreadCount", "LineCount")],
            file="export_sample2/index.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")


# read SO links
clones_so_links <- fread("data/CodeClonesSample2LinksSOExport.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
# prevent problems with SQLDF and integer64
clones_so_links$ContentNormalizedHash <- as.character(clones_so_links$ContentNormalizedHash)

nrow(clones_so_links)
# 416

length(unique(clones_so_links$ContentNormalizedHash))
# 252

# write SO links for each cloned code block
for (hash_value in hash_values) {
  so_links <- sqldf(paste0("SELECT LinkedPostId, LinkedPostTypeId, PostCount FROM clones_so_links WHERE ContentNormalizedHash=", hash_value, " ORDER BY PostCount DESC"))
  write.table(so_links, file=paste0("export_sample2/", hash_value, "_so-links.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
}


# read other links
clones_links <- fread("data/CodeClonesSample2LinksNonSOExport.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
# prevent problems with SQLDF and integer64
clones_links$ContentNormalizedHash <- as.character(clones_links$ContentNormalizedHash)

nrow(clones_links)
# 3,316

length(unique(clones_links$ContentNormalizedHash))
# 814

# write other links for each cloned code block
for (hash_value in hash_values) {
  links <- sqldf(paste0("SELECT Url, PostCount FROM clones_links WHERE ContentNormalizedHash=", hash_value, " ORDER BY PostCount DESC"))
  write.table(links, file=paste0("export_sample2/", hash_value, "_links.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
}

