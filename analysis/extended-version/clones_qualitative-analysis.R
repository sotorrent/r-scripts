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
# 74,600

clones$Sample <- rep("", n)
clones <- subset(clones, select=c(26,1:25))
sample_ids <- integer()
n_per_language <- 5
for (i in 11:24) {
  language <- names(clones)[i]
  print(language)
  filter <- clones[,i:i] == TRUE
  filted_ids <- which(filter)
  print(length(filted_ids))
  #sample_ids <- c(sample_ids, filted_ids[randomSequence(min=1, max=length(filted_ids), col=1, check=TRUE)[,1]])
  new_sample_ids <- filted_ids[sample(1:length(filted_ids), n_per_language)]
  clones[new_sample_ids]$Sample <- language
  sample_ids <- c(sample_ids, new_sample_ids)
}

write.table(clones[sample_ids], file="data/CodeBlocksComparisonFilteredSample1.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
