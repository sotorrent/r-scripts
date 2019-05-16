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
language_columns <- 7:20

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
# [1] 6146
# [1] "JavaScript"
# [1] 11075
# [1] "PHP"
# [1] 4951
# [1] "HTMLCSS"
# [1] 6836
# [1] "CSharp"
# [1] 3828
# [1] "Python"
# [1] 1793
# [1] "SQL"
# [1] 2720
# [1] "Swift"
# [1] 367
# [1] "CPP"
# [1] 1153
# [1] "ObjectiveC"
# [1] 642
# [1] "C"
# [1] 714
# [1] "Ruby"
# [1] 626
# [1] "VBA"
# [1] 576
# [1] "R"
# [1] 375

# add URLs for threads
clones$PostUrls <- rep("", nrow(clones))
for (i in sample_ids) {
  post_ids <- strsplit(clones[i]$PostIds, " ", fixed = TRUE)
  clones[i]$PostUrls <- paste(sapply(post_ids, function(post_id) {
    return(paste0("https://stackoverflow.com/q/", post_id))
  }), sep="", collapse=" ")
}

# reorder columns
clones <- subset(clones, select=c(22,1,23,2:21))

write.table(clones[sample_ids], file="data/CodeBlocksComparisonFilteredSample1.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
