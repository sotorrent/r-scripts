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
#library(random)
library(sqldf)

# read data
clones <- fread("data/CodeBlocksComparisonFiltered.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
# prevent problems with SQLDF and integer64
clones$ContentNormalizedHash <- as.character(clones$ContentNormalizedHash)
n <- nrow(clones)
n
# 29,978

table(clones$ThreadCount)[1:10]
#     2     3     4     5     6     7     8     9    10    11 
# 26283  2482   636   248   121    54    39    38    17    16 

clones$Sample <- rep("", n)
sample_ids <- integer()
n_per_language <- 5
language_columns <- 10:23

for (i in language_columns) {
  language <- names(clones)[i]
  print(language)
  column_filter <- language_columns[language_columns!=i]
  other_languages_filter <- apply(subset(clones, select=column_filter), 1, function(row) {
    for (column in column_filter) {
      if (row[column] == TRUE) {
        return(TRUE)
      }
      return(FALSE)
    }
  })
  row_filter <- subset(clones, select=i) == TRUE & other_languages_filter == FALSE
  filted_ids <- which(row_filter)
  print(length(filted_ids))
  new_sample_ids <- filted_ids[sample(1:length(filted_ids), min(n_per_language, length(filted_ids)))]
  clones[new_sample_ids]$Sample <- language
  sample_ids <- c(sample_ids, new_sample_ids)
}

# [1] "Java"
# [1] 6200
# [1] "JavaScript"
# [1] 11117
# [1] "PHP"
# [1] 4957
# [1] "HTMLCSS"
# [1] 6839
# [1] "CSharp"
# [1] 3811
# [1] "Python"
# [1] 1782
# [1] "SQL"
# [1] 2720
# [1] "Swift"
# [1] 373
# [1] "CPP"
# [1] 947
# [1] "ObjectiveC"
# [1] 631
# [1] "C"
# [1] 703
# [1] "Ruby"
# [1] 643
# [1] "VBA"
# [1] 616
# [1] "R"
# [1] 384

# add URLs for threads
#clones$PostUrls <- rep("", nrow(clones))
#for (i in sample_ids) {
#  post_ids <- strsplit(clones[i]$PostIds, ";", fixed = TRUE)
#  clones[i]$PostUrls <- paste(sapply(post_ids, function(post_id) {
#    return(paste0("https://stackoverflow.com/q/", post_id))
#  }), sep="", collapse=" ")
#}

# reorder columns
clones <- subset(clones, select=c(25,1:24))

sampled_snippets <- clones[sample_ids]

write.table(sampled_snippets, file="data/CodeBlocksComparisonFilteredSample1.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")


##################################
### write data for online tool ###
##################################

# read sample
clones <- fread("data/CodeBlocksComparisonFilteredSample1.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
# prevent problems with SQLDF and integer64
clones$ContentNormalizedHash <- as.character(clones$ContentNormalizedHash)
n <- nrow(clones)
n
# 70

# write posts containing clone
for (i in 1:n) {
  post_ids <- strsplit(clones[i]$PostIds, ";", fixed = TRUE)
  parent_ids <- strsplit(clones[i]$ParentIds, ";", fixed = TRUE)
  owner_user_ids <- strsplit(clones[i]$OwnerUserIds, ";", fixed = TRUE)
  creation_dates <- strsplit(clones[i]$CreationDates, ";", fixed = TRUE)
  scores <- strsplit(clones[i]$Scores, ";", fixed = TRUE)
  
  clone_data <- data.frame(
    post_ids,
    parent_ids,
    owner_user_ids,
    creation_dates,
    scores,
    stringsAsFactors=FALSE
  )
  names(clone_data) <- c("PostId", "ParentId", "OwnerUserId", "CreationDate", "Score")
  
  # sort posts chronologically
  clone_data <- clone_data[order(clone_data$CreationDate),]
  
  # write metadata
  write.table(clone_data, file=paste0("export_sample3/", clones[i]$ContentNormalizedHash, ".csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

  # write content
  write.table(clones[i, c("Content")], file=paste0("export_sample3/", clones[i]$ContentNormalizedHash, "_content.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
}

# write index file
index <- clones[,c("ContentNormalizedHash", "Sample", "ThreadCount", "LineCount")]
names(index) <- c("HashValue", "Language", "ThreadCount", "LineCount")

write.table(index, file="export_sample3/index.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")


# read SO links
clones_so_links <- fread("data/CodeBlocksComparisonLinksSOExport.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
# prevent problems with SQLDF and integer64
clones_so_links$ContentNormalizedHash <- as.character(clones_so_links$ContentNormalizedHash)
nrow(clones_so_links)
# 8,446

# write SO links for each cloned code block
for (hash_value in clones$ContentNormalizedHash) {
  so_links <- sqldf(paste0("SELECT LinkedPostId, LinkedPostTypeId, PostCount FROM clones_so_links WHERE ContentNormalizedHash=", hash_value, " ORDER BY PostCount DESC"))
  write.table(so_links, file=paste0("export_sample3/", hash_value, "_so-links.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
}


# read other links
clones_links <- fread("data/CodeBlocksComparisonLinksNonSOExport.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
# prevent problems with SQLDF and integer64
clones_links$ContentNormalizedHash <- as.character(clones_links$ContentNormalizedHash)
nrow(clones_links)
# 44,817

# write other links for each cloned code block
for (hash_value in clones$ContentNormalizedHash) {
  links <- sqldf(paste0("SELECT Url, PostCount FROM clones_links WHERE ContentNormalizedHash=", hash_value, " ORDER BY PostCount DESC"))
  write.table(links, file=paste0("export_sample3/", hash_value, "_links.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
}
