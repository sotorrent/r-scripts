setwd("") # Pfad bitte anpassen

library(data.table)
data <- fread("data/PostId_VersionCount_SO_17-06.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
names(data) <- c("PostId", "PostTypeId", "VersionCount")
nrow(data)
# 36,062,267
data_java <- fread("data/PostId_VersionCount_SO_Java_17-06.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
names(data_java) <- c("PostId", "PostTypeId", "VersionCount")
nrow(data_java)
# 5,410,059

# only consider posts with at least two versions
data_filtered <- data[data$VersionCount>1,]
data_java_filtered <- data_java[data_java$VersionCount>1,]

# draw one sample with many versions
summary(data_filtered$VersionCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.00    2.00    2.00    2.57    3.00  754.00 
quantile(data_filtered$VersionCount, c(0.75, 0.8, 0.85, 0.9, 0.95))
# 75% 80% 85% 90% 95% 
# 3   3   3   4   5
quantile(data_filtered$VersionCount, c(0.95, 0.96, 0.97, 0.98, 0.99))
# 95% 96% 97% 98% 99% 
# 5   5   5   6   7
data_filtered$VersionCount[data_filtered$VersionCount>50]
data_filtered_many_versions <- data[data$VersionCount>=7,] # 99% quantile

# all posts, large sample (may intersect, just used for robustness testing)
nrow(data_filtered)

#for (i in 1:10) {
#  sample <- data_filtered[sample(nrow(data_filtered), 10000),]
#  filename <- paste0("PostId_VersionCount_SO_17-06_sample_10000_", i, ".csv")
#  write.table(sample, file=filename, sep=";", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
#}

# all posts, sample with posts having most versions (just used for robustness testing)
#data_filtered_most_versions <- data_filtered[base::order(data_filtered$VersionCount, data_filtered$PostId, decreasing=TRUE),]
#sample_most_versions <- data_filtered_most_versions[1:100, c("PostId", "PostTypeId")]
#write.table(sample_most_versions, file="PostId_VersionCount_SO_17-06_sample_100_most_versions.csv", sep=";", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")


# read and exclude posts that are already present in previous sample(s)

# all posts, small sample
nrow(data_filtered)
# 12,962,337
sample_1 <- fread("data/PostId_VersionCount_SO_17-06_sample_100_1.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c(""))
nrow(sample_1)
# 100
data_filtered_s1 <- data_filtered[!(data_filtered$PostId %in% sample_1$PostId)]
nrow(data_filtered_s1)
# 12,962,237
sample_2 <- fread("data/PostId_VersionCount_SO_17-06_sample_100_2.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c(""))
nrow(sample_2)
# 100
data_filtered_s2 <- data_filtered_s1[!(data_filtered_s1$PostId %in% sample_2$PostId)]
nrow(data_filtered_s2)
# 12,962,137
data_filtered <- data_filtered_s2

# Java posts
nrow(data_java_filtered)
# 1,988,375
sample_1 <- fread("data/PostId_VersionCount_SO_Java_17-06_sample_100_1.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c(""))
nrow(sample_1)
# 100
data_java_filtered_s1 <- data_java_filtered[!(data_java_filtered$PostId %in% sample_1$PostId)]
nrow(data_java_filtered_s1)
# 1,988,275
sample_2 <- fread("data/PostId_VersionCount_SO_Java_17-06_sample_100_2.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c(""))
nrow(sample_2)
# 100
data_java_filtered_s2 <- data_java_filtered_s1[!(data_java_filtered_s1$PostId %in% sample_2$PostId)]
nrow(data_java_filtered_s2)
# 1,988,175
data_java_filtered <- data_java_filtered_s2

# posts with many versions
nrow(data_filtered_many_versions)
# 141,694
sample_1 <- fread("data/PostId_VersionCount_SO_17-06_sample_100_1+.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c(""))
nrow(sample_1)
# 100
data_filtered_many_versions_s1 <- data_filtered_many_versions[!(data_filtered_many_versions$PostId %in% sample_1$PostId)]
nrow(data_filtered_many_versions_s1)
# 141,594
sample_2 <- fread("data/PostId_VersionCount_SO_17-06_sample_100_2+.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c(""))
nrow(sample_2)
# 100
data_filtered_many_versions_s2 <- data_filtered_many_versions_s1[!(data_filtered_many_versions_s1$PostId %in% sample_2$PostId)]
nrow(data_filtered_many_versions_s2)
# 141,494
data_filtered_many_versions <- data_filtered_many_versions_s2

# draw new sample
sample <- data_filtered[sample(nrow(data_filtered), 100),]
sample_java <- data_java_filtered[sample(nrow(data_java_filtered), 100),]
sample_many <- data_filtered_many_versions[sample(nrow(data_filtered_many_versions), 100),]

# write sampled posts to CSV files
write.table(sample, file="PostId_VersionCount_SO_17-06_sample_100_3.csv", sep=";", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
write.table(sample_java, file="PostId_VersionCount_SO_Java_17-06_sample_100_3.csv", sep=";", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
write.table(sample_many, file="PostId_VersionCount_SO_17-06_sample_100_3+.csv", sep=";", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
