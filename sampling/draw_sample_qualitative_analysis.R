setwd("") # Pfad bitte anpassen

library(data.table)
data <- fread("data/EditedThreads.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
names(data) <- c("PostId")
nrow(data)
# 9,756,316

# draw sample of question post ids
sample <- data[sample(nrow(data), 400),]

# write sampled posts to CSV files
write.table(sample, file="data/EditedThreads_Sample400.csv", sep=";", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
