# TODO set relative path
setwd("C:\\Users\\Lorik\\Desktop\\5. Semester\\Master-Arbeit\\2017-12-04_samples_comparison-all") # Pfad bitte anpassen


############################################################################################
# parse all files "..._per_post.csv" and create an aggregated one "PostId_VersionCount_SO_17-06_sample_700_aggregated.csv"

library(data.table)

data_1_perPost <- fread("PostId_VersionCount_SO_17-06_sample_100_1_per_post.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_1plus_perPost <- fread("PostId_VersionCount_SO_17-06_sample_100_1+_per_post.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_2_perPost <- fread("PostId_VersionCount_SO_17-06_sample_100_2_per_post.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_2plus_perPost <- fread("PostId_VersionCount_SO_17-06_sample_100_2+_per_post.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_multiple_possible_links_perPost <- fread("PostId_VersionCount_SO_17-06_sample_100_multiple_possible_links_per_post.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_unclear_matching_perPost <- fread("PostId_VersionCount_SO_17_06_sample_unclear_matching_per_post.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_java_1_perPost <- fread("PostId_VersionCount_SO_Java_17-06_sample_100_1_per_post.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_java_2_perPost <- fread("PostId_VersionCount_SO_Java_17-06_sample_100_2_per_post.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))

# https://stackoverflow.com/a/23933873
data_perPost <- rbindlist(list(data_1_perPost, data_1plus_perPost), use.names=TRUE, fill=TRUE)
data_perPost <- rbindlist(list(data_perPost, data_2_perPost), use.names=TRUE, fill=TRUE)
data_perPost <- rbindlist(list(data_perPost, data_2plus_perPost), use.names=TRUE, fill=TRUE)
data_perPost <- rbindlist(list(data_perPost, data_multiple_possible_links_perPost), use.names=TRUE, fill=TRUE)
data_perPost <- rbindlist(list(data_perPost, data_unclear_matching_perPost), use.names=TRUE, fill=TRUE)
data_perPost <- rbindlist(list(data_perPost, data_java_1_perPost), use.names=TRUE, fill=TRUE)
data_perPost <- rbindlist(list(data_perPost, data_java_2_perPost), use.names=TRUE, fill=TRUE)

#names(data_perPost) <- c("Sample", "MetricType", "Metric", "Threshold", "PostId", "PostVersionCount", "PostBlockVersionCount", "PossibleConnections", "RuntimeText", "TextBlockVersionCount", "PossibleConnectionsText", "TruePositivesText", "TrueNegativesText", "FalsePositivesText", "FalseNegativesText", "FailedPredecessorComparisonsText", "RuntimeCode", "CodeBlockVersionCount", "PossibleConnectionsCode", "TruePositivesCode", "TrueNegativesCode", "FalsePositivesCode", "FalseNegativesCode", "FailedPredecessorComparisonsCode")

# https://stackoverflow.com/a/10608587
write.table(data_perPost, "PostId_VersionCount_SO_17-06_sample_700_perPost.csv", sep=";", row.names = F)
############################################################################################




############################################################################################
# validation
NROW(unique(data_perPost$Metric)) # 134
NROW(unique(data_perPost$PostId)) # 700
NROW(unique(data_perPost$Threshold)) # 11
nrow(data_perPost) # 1031800 = 134 * 700 * 11

data_null <- data_perPost[!complete.cases(data_perPost),] # https://www.statmethods.net/input/missingdata.html
nrow(data_null) # 0
############################################################################################




############################################################################################
# Get the posts that have false-positives for metrics of type EQUAL
# text
data_EQUAL_falsePositives_text <- data_perPost[data_perPost$MetricType == 'EQUAL' & data_perPost$FalsePositivesText != 0 & data_perPost$Threshold > 0]
unique(data_EQUAL_falsePositives_text$PostId)

# code
data_EQUAL_falsePositives_code <- data_perPost[data_perPost$MetricType == 'EQUAL' & data_perPost$FalsePositivesCode != 0 & data_perPost$Threshold > 0]
unique(data_EQUAL_falsePositives_code$PostId)
############################################################################################