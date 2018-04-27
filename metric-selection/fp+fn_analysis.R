#setwd("F:/Git/github/r-scripts/metric-selection/") # Pfad bitte anpassen
setwd("/Users/sebastian/git/github/r-scripts/metric-selection/")

# load functions
source("functions.R")

# read results of first run with all metrics
library(data.table)

# samples randomly drawn from all SO posts
sample_100_1 <- fread("default/PostId_VersionCount_SO_17-06_sample_100_1_per_post.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2 <- fread("default/PostId_VersionCount_SO_17-06_sample_100_2_per_post.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random <- rbind(sample_100_1, sample_100_2)
sample_random <- sample_random[,c("PostId", "FalsePositivesText", "FalseNegativesText", "FalsePositivesCode", "FalseNegativesCode")]
rm(sample_100_1, sample_100_2)

# samples randomly drawn from all SO posts with at least seven versions (99% quantile of version count of all posts)
sample_100_1_99 <- fread("default/PostId_VersionCount_SO_17-06_sample_100_1+_per_post.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2_99 <- fread("default/PostId_VersionCount_SO_17-06_sample_100_2+_per_post.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random_99 <- rbind(sample_100_1_99, sample_100_2_99)
sample_random_99 <- sample_random_99[,c("PostId", "FalsePositivesText", "FalseNegativesText", "FalsePositivesCode", "FalseNegativesCode")]
rm(sample_100_1_99, sample_100_2_99)

# samples randomly drawn from all Java SO posts (tagged with <java> or <android>) with at least two versions
sample_java_100_1 <- fread("default/PostId_VersionCount_SO_Java_17-06_sample_100_1_per_post.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_java_100_2 <- fread("default/PostId_VersionCount_SO_Java_17-06_sample_100_2_per_post.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_java_random <- rbind(sample_java_100_1, sample_java_100_2)
sample_java_random <- sample_java_random[,c("PostId", "FalsePositivesText", "FalseNegativesText", "FalsePositivesCode", "FalseNegativesCode")]
rm(sample_java_100_1, sample_java_100_2)

# sample with multiple possible connections (to test matching strategy)
sample_multiple_possible_links <- fread("default/PostId_VersionCount_SO_17-06_sample_100_multiple_possible_links_per_post.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_multiple_possible_links <- sample_multiple_possible_links[,c("PostId", "FalsePositivesText", "FalseNegativesText", "FalsePositivesCode", "FalseNegativesCode")]

fp_text <- rbind(
  sample_random[sample_random$FalsePositivesText>0,],
  sample_random_99[sample_random_99$FalsePositivesText>0,],
  sample_java_random[sample_java_random$FalsePositivesText>0,]
)

fn_text <- rbind(
  sample_random[sample_random$FalseNegativesText>0,],
  sample_random_99[sample_random_99$FalseNegativesText>0,],
  sample_java_random[sample_java_random$FalseNegativesText>0,]
)

fp_code <- rbind(
  sample_random[sample_random$FalsePositivesCode>0,],
  sample_random_99[sample_random_99$FalsePositivesCode>0,],
  sample_java_random[sample_java_random$FalsePositivesCode>0,]
)

fn_code <- rbind(
  sample_random[sample_random$FalseNegativesCode>0,],
  sample_random_99[sample_random_99$FalseNegativesCode>0,],
  sample_java_random[sample_java_random$FalseNegativesCode>0,]
)

fp_text_posts <- unique(fp_text$PostId)
fn_text_posts <- unique(fn_text$PostId)
fp_code_posts <- unique(fp_code$PostId)
fn_code_posts <- unique(fn_code$PostId)

# write post ids of false postives/negatives to separate CSV files
write.table(fp_text_posts, file="fp+fn/fp_text.csv", sep=";", col.names=FALSE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
write.table(fn_text_posts, file="fp+fn/fn_text.csv", sep=";", col.names=FALSE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
write.table(fp_code_posts, file="fp+fn/fp_code.csv", sep=";", col.names=FALSE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
write.table(fn_code_posts, file="fp+fn/fn_code.csv", sep=";", col.names=FALSE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

