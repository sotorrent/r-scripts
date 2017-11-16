# TODO set relative path
setwd("C:\\Users\\Lorik\\Documents\\GitHub\\r-scripts\\metric_comparison_r_scripts\\2017-11-14_sample_comparison_sebastian-4") # Pfad bitte anpassen


############################################################################################
# parse all files "..._per_post.csv" and create an aggregated one "PostId_VersionCount_SO_17-06_sample_100_aggregated.csv"

library(data.table)
data_1 <- fread("PostId_VersionCount_SO_17-06_sample_100_1_per_post.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_1plus <- fread("PostId_VersionCount_SO_17-06_sample_100_1+_per_post.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_2 <- fread("PostId_VersionCount_SO_17-06_sample_100_2_per_post.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_2plus <- fread("PostId_VersionCount_SO_17-06_sample_100_2+_per_post.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_multiple_possible_links <- fread("PostId_VersionCount_SO_17-06_sample_100_multiple_possible_links_per_post.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_unclear_matching <- fread("PostId_VersionCount_SO_17_06_sample_unclear_matching_per_post.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_java_1 <- fread("PostId_VersionCount_SO_Java_17-06_sample_100_1_per_post.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data_java_2 <- fread("PostId_VersionCount_SO_Java_17-06_sample_100_2_per_post.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))

# https://stackoverflow.com/a/23933873
data <- rbindlist(list(data_1, data_1plus), use.names=TRUE, fill=TRUE)
data <- rbindlist(list(data, data_2), use.names=TRUE, fill=TRUE)
data <- rbindlist(list(data, data_2plus), use.names=TRUE, fill=TRUE)
data <- rbindlist(list(data, data_multiple_possible_links), use.names=TRUE, fill=TRUE)
data <- rbindlist(list(data, data_unclear_matching), use.names=TRUE, fill=TRUE)
data <- rbindlist(list(data, data_java_1), use.names=TRUE, fill=TRUE)
data <- rbindlist(list(data, data_java_2), use.names=TRUE, fill=TRUE)

names(data) <- c("Sample", "Metric", "Threshold", "PostId", "PostVersionCount", "PostBlockVersionCount", "PossibleConnections", "RuntimeTextTotal", "RuntimeTextCPU", "RuntimeTextUser", "TextBlockVersionCount", "PossibleConnectionsText", "TruePositivesText", "TrueNegativesText", "FalsePositivesText", "FalseNegativesText", "RuntimeCodeTotal", "RuntimeCodeCPU", "RuntimeCodeUser", "CodeBlockVersionCount", "PossibleConnectionsCode", "TruePositivesCode", "TrueNegativesCode", "FalsePositivesCode", "FalseNegativesCode")

# https://stackoverflow.com/a/10608587
# write.table(data, "PostId_VersionCount_SO_17-06_sample_100_aggregated.csv", sep=";", row.names = F)
############################################################################################




############################################################################################
# validation

nrow(data) # 89180 + 86450 + 89180 + 88270 + 91000 + 15470 + 88270 + 89180 = 637000

data_null <- data[!complete.cases(data),] # https://www.statmethods.net/input/missingdata.html
nrow(data_null) # 35616

data_notNull <- na.omit(data) # https://www.statmethods.net/input/missingdata.html
nrow(data_notNull) # 601384

# 637000 = 35616 + 601384
############################################################################################



