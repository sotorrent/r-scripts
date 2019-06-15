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

# read data
mutual_references <- fread("data/MutualReferences.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
nrow(mutual_references)
# 973

unique_files <- unique.data.frame(mutual_references[,c("FileId", "FileExt")])
nrow(unique_files)
# 861
unique_files$Category <- as.character(rep(NA, 861))

# sample 25 per category
n <- 25

documentation <- unique_files[unique_files$FileExt %in% c(".md", ".adoc", "N/A", ".rst", ".txt", ".markdown"),]
nrow(documentation)
# 517
documentation <- documentation[sample(1:517, n),]
nrow(documentation)
# 25
documentation$Category <- rep("Documentation", n)

java <- unique_files[unique_files$FileExt %in% c(".java"),]
nrow(java)
# 63
java <- java[sample(1:63, n),]
nrow(java)
# 25
java$Category <- rep("Java", n)

javascript <- unique_files[unique_files$FileExt %in% c(".js"),]
nrow(javascript)
# 52
javascript <- javascript[sample(1:52, n),]
nrow(javascript)
# 25
javascript$Category <- rep("JavaScript", n)

objective_c <- unique_files[unique_files$FileExt %in% c(".m"),]
nrow(objective_c)
# 22 -> drop, because less than 25

python <- unique_files[unique_files$FileExt %in% c(".py"),]
nrow(python)
# 28
python <- python[sample(1:28, n),]
nrow(python)
# 25
python$Category <- rep("Python", n)

c_sharp <- unique_files[unique_files$FileExt %in% c(".cs"),]
nrow(c_sharp)
# 26
c_sharp <- c_sharp[sample(1:26, n),]
nrow(c_sharp)
# 25
c_sharp$Category <- rep("C#", n)

selected_file_ids <- data.frame(rbind(documentation, java, javascript, python, c_sharp))
nrow(selected_file_ids)
# 125

mutual_references <- mutual_references[mutual_references$FileId %in% selected_file_ids$FileId,]
nrow(mutual_references)
# 128

mutual_references$Category <- as.character(rep(NA, 128))
mutual_references[mutual_references$FileExt %in% c(".md", ".adoc", "N/A", ".rst", ".txt", ".markdown"),]$Category <- "Documentation"
mutual_references[mutual_references$FileExt %in% c(".java"),]$Category <- "Java"
mutual_references[mutual_references$FileExt %in% c(".js"),]$Category <- "JavaScript"
mutual_references[mutual_references$FileExt %in% c(".py"),]$Category <- "Python"
mutual_references[mutual_references$FileExt %in% c(".cs"),]$Category <- "C#"

mutual_references <- mutual_references[,c("Category", "FileExt", "FileId", "GHUrl", "SOUrl", "PostId", "Repo")]

mutual_references <- sqldf("SELECT * FROM mutual_references ORDER BY Category, FileExt, FileId")

# export sample
write.table(mutual_references, file=paste0("data/mutual-references-sample.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
