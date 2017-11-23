# TODO set relative path
setwd("C:\\Users\\Lorik\\Desktop\\5. Semester\\Master-Arbeit\\2017-11-22_sample_comparison_sebastian-5") # Pfad bitte anpassen


############################################################################################
# parse all files "..._per_post.csv" and create an aggregated one "PostId_VersionCount_SO_17-06_sample_100_aggregated.csv"

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

names(data_perPost) <- c("Sample", "MetricType", "Metric", "Threshold", "PostId", "PostVersionCount", "PostBlockVersionCount", "PossibleConnections", "RuntimeText", "TextBlockVersionCount", "PossibleConnectionsText", "TruePositivesText", "TrueNegativesText", "FalsePositivesText", "FalseNegativesText", "FailedPredecessorComparisonsText", "RuntimeCode", "CodeBlockVersionCount", "PossibleConnectionsCode", "TruePositivesCode", "TrueNegativesCode", "FalsePositivesCode", "FalseNegativesCode", "FailedPredecessorComparisonsCode")

# https://stackoverflow.com/a/10608587
write.table(data_perPost, "PostId_VersionCount_SO_17-06_sample_100_aggregated_perPost.csv", sep=";", row.names = F)






data_1_perVersion <- fread("PostId_VersionCount_SO_17-06_sample_100_1_per_version.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_1plus_perVersion <- fread("PostId_VersionCount_SO_17-06_sample_100_1+_per_version.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_2_perVersion <- fread("PostId_VersionCount_SO_17-06_sample_100_2_per_version.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_2plus_perVersion <- fread("PostId_VersionCount_SO_17-06_sample_100_2+_per_version.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_multiple_possible_links_perVersion <- fread("PostId_VersionCount_SO_17-06_sample_100_multiple_possible_links_per_version.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_unclear_matching_perVersion <- fread("PostId_VersionCount_SO_17_06_sample_unclear_matching_per_version.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_java_1_perVersion <- fread("PostId_VersionCount_SO_Java_17-06_sample_100_1_per_version.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_java_2_perVersion <- fread("PostId_VersionCount_SO_Java_17-06_sample_100_2_per_version.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))

# https://stackoverflow.com/a/23933873
data_perVersion <- rbindlist(list(data_1_perVersion, data_1plus_perVersion), use.names=TRUE, fill=TRUE)
data_perVersion <- rbindlist(list(data_perVersion, data_2_perVersion), use.names=TRUE, fill=TRUE)
data_perVersion <- rbindlist(list(data_perVersion, data_2plus_perVersion), use.names=TRUE, fill=TRUE)
data_perVersion <- rbindlist(list(data_perVersion, data_multiple_possible_links_perVersion), use.names=TRUE, fill=TRUE)
data_perVersion <- rbindlist(list(data_perVersion, data_unclear_matching_perVersion), use.names=TRUE, fill=TRUE)
data_perVersion <- rbindlist(list(data_perVersion, data_java_1_perVersion), use.names=TRUE, fill=TRUE)
data_perVersion <- rbindlist(list(data_perVersion, data_java_2_perVersion), use.names=TRUE, fill=TRUE)

names(data_perVersion) <- c("Sample", "MetricType", "Metric", "Threshold", "PostId", "PostHistoryId", "PossibleConnections", "RuntimeText", "TextBlockCount", "PossibleConnectionsText", "TruePositivesText", "TrueNegativesText", "FalsePositivesText", "FalseNegativesText", "FailedPredecessorComparisonsText", "RuntimeCode", "CodeBlockCount", "PossibleConnectionsCode", "TruePositivesCode", "TrueNegativesCode", "FalsePositivesCode", "FalseNegativesCode", "FailedPredecessorComparisonsCode")

# https://stackoverflow.com/a/10608587
write.table(data_perVersion, "PostId_VersionCount_SO_17-06_sample_100_aggregated_perVersion.csv", sep=";", row.names = F)


############################################################################################



# Posts that have false positives in TEXT although they use a equals method.
unique(data_perPost$PostId[
  (data_perPost$Metric == "equals" | data_perPost$Metric == "equalsNormalized" | data_perPost$Metric == "tokenEquals" | data_perPost$Metric == "tokenEqualsNormalized")
  & data_perPost$FalsePositivesText != 0])

# Posts that have false positives in CODE although they use a equals method.
  unique(data_perPost$PostId[
    (data_perPost$Metric == "equals" | data_perPost$Metric == "equalsNormalized" | data_perPost$Metric == "tokenEquals" | data_perPost$Metric == "tokenEqualsNormalized")
    & data_perPost$FalsePositivesCode != 0])


############################################################################################
# validation

nrow(data) # 89180 + 86450 + 89180 + 88270 + 91000 + 15470 + 88270 + 89180 = 637000

data_null <- data[!complete.cases(data),] # https://www.statmethods.net/input/missingdata.html
nrow(data_null) # 35616

data_notNull <- na.omit(data) # https://www.statmethods.net/input/missingdata.html
nrow(data_notNull) # 601384

# 637000 = 35616 + 601384
############################################################################################



