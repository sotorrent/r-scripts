#setwd("F:/Git/github/r-scripts/metric-selection/comparison-by-sample") # Pfad bitte anpassen
setwd("/Users/sebastian/git/github/r-scripts/metric-selection/comparison-by-sample")

merge_samples <- function(sample1, sample2) {
  sample_merged <- merge(sample1, sample2, by=c("MetricType", "Metric", "Threshold"))
  sample_merged$Runtime <- sample_merged$Runtime.x + sample_merged$Runtime.y
  sample_merged$PostCount <- sample_merged$PostCount.x + sample_merged$PostCount.y
  sample_merged$PostVersionCount <- sample_merged$PostVersionCount.x + sample_merged$PostVersionCount.y
  sample_merged$PostBlockVersionCount <- sample_merged$PostBlockVersionCount.x + sample_merged$PostBlockVersionCount.y
  sample_merged$PossibleComparisons <- sample_merged$PossibleComparisons.x + sample_merged$PossibleComparisons.y
  # text
  sample_merged$TextBlockVersionCount <- sample_merged$TextBlockVersionCount.x + sample_merged$TextBlockVersionCount.y
  sample_merged$PossibleComparisonsText <- sample_merged$PossibleComparisonsText.x + sample_merged$PossibleComparisonsText.y
  sample_merged$TruePositivesText <- sample_merged$TruePositivesText.x + sample_merged$TruePositivesText.y
  sample_merged$TrueNegativesText <- sample_merged$TrueNegativesText.x + sample_merged$TrueNegativesText.y
  sample_merged$FalsePositivesText <- sample_merged$FalsePositivesText.x + sample_merged$FalsePositivesText.y
  sample_merged$FalseNegativesText <- sample_merged$FalseNegativesText.x + sample_merged$FalseNegativesText.y
  sample_merged$FailuresText <- sample_merged$FailuresText.x + sample_merged$FailuresText.y
  # code
  sample_merged$CodeBlockVersionCount <- sample_merged$CodeBlockVersionCount.x + sample_merged$CodeBlockVersionCount.y
  sample_merged$PossibleComparisonsCode <- sample_merged$PossibleComparisonsCode.x + sample_merged$PossibleComparisonsCode.y
  sample_merged$TruePositivesCode <- sample_merged$TruePositivesCode.x + sample_merged$TruePositivesCode.y
  sample_merged$TrueNegativesCode <- sample_merged$TrueNegativesCode.x + sample_merged$TrueNegativesCode.y
  sample_merged$FalsePositivesCode <- sample_merged$FalsePositivesCode.x + sample_merged$FalsePositivesCode.y
  sample_merged$FalseNegativesCode <- sample_merged$FalseNegativesCode.x + sample_merged$FalseNegativesCode.y
  sample_merged$FailuresCode <- sample_merged$FailuresCode.x + sample_merged$FailuresCode.y
  
  return(filter_columns(sample_merged))
}

filter_columns <- function(df) {
  return(df[,c(
    "MetricType", "Metric", "Threshold", "Runtime",
    "PostCount", "PostVersionCount", "PostBlockVersionCount", "PossibleComparisons",
    "TextBlockVersionCount", "PossibleComparisonsText", "TruePositivesText", "TrueNegativesText", "FalsePositivesText", "FalseNegativesText", "FailuresText",
    "CodeBlockVersionCount", "PossibleComparisonsCode", "TruePositivesCode", "TrueNegativesCode", "FalsePositivesCode", "FalseNegativesCode", "FailuresCode"
  )])
}
 
matthews_correlation <- function(TP, TN, FP, FN) {
  TP <- as.numeric(TP)
  TN <- as.numeric(TN)
  FP <- as.numeric(FP)
  FN <- as.numeric(FN)
  numerator <- (TP * TN) - (FP * FN);
  denominator <- (TP + FP) * (TP + FN) * (TN + FP) * (TN + FN);
  filter <- denominator != 0
  denominator[filter] <- sqrt(denominator[filter])
  denominator[!filter] <- 1
  return(numerator/denominator)
} 

add_matthews_correlation <- function(df) {
  df$MatthewsCorrelationText <- matthews_correlation(df$TruePositivesText, df$TrueNegativesText,
                                                     df$FalsePositivesText, df$FalseNegativesText)
  df$MatthewsCorrelationCode <- matthews_correlation(df$TruePositivesCode, df$TrueNegativesCode,
                                                     df$FalsePositivesCode, df$FalseNegativesCode)
  return(df)
}

  
# read results of first run with all metrics
library(data.table)

# samples randomly drawn from all SO posts
sample_100_1 <- fread("all/PostId_VersionCount_SO_17-06_sample_100_1_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2 <- fread("all/PostId_VersionCount_SO_17-06_sample_100_2_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random <- merge_samples(sample_100_1, sample_100_2)
rm(sample_100_1, sample_100_2)

# samples randomly drawn from all SO posts with at least seven versions (99% quantile of version count of all posts)
sample_100_1_99 <- fread("all/PostId_VersionCount_SO_17-06_sample_100_1+_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2_99 <- fread("all/PostId_VersionCount_SO_17-06_sample_100_2+_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random_99 <- merge_samples(sample_100_1_99, sample_100_2_99)
rm(sample_100_1_99, sample_100_2_99)

# samples randomly drawn from all Java SO posts (tagged with <java> or <android>) with at least two versions
sample_java_100_1 <- fread("all/PostId_VersionCount_SO_Java_17-06_sample_100_1_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_java_100_2 <- fread("all/PostId_VersionCount_SO_Java_17-06_sample_100_2_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_java_random <- merge_samples(sample_java_100_1, sample_java_100_2)
rm(sample_java_100_1, sample_java_100_2)

# sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_unclear_matching <- fread("all/PostId_VersionCount_SO_17_06_sample_unclear_matching_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_unclear_matching <- filter_columns(sample_unclear_matching)

# sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_multiple_possible_links <- fread("all/PostId_VersionCount_SO_17-06_sample_100_multiple_possible_links_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_multiple_possible_links <- filter_columns(sample_multiple_possible_links)


# calculate Matthews correlation and retrieve 99% quantile for text and code

# sample randomly drawn from all SO posts
sample_random <- add_matthews_correlation(sample_random)
# text
setorderv(sample_random, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))
MatthewsCorrelationText_95 <- quantile(sample_random$MatthewsCorrelationText, 0.95)
sample_random_text_candidates <- sample_random[sample_random$MatthewsCorrelationText >= MatthewsCorrelationText_95,]
# code
setorderv(sample_random, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
MatthewsCorrelationCode_95 <- quantile(sample_random$MatthewsCorrelationCode, 0.95)
sample_random_code_candidates <- sample_random[sample_random$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

# sample randomly drawn from all SO posts with at least seven versions (99% quantile of version count of all posts)
sample_random_99 <- add_matthews_correlation(sample_random_99)
# text
setorderv(sample_random_99, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))
MatthewsCorrelationText_95 <- quantile(sample_random_99$MatthewsCorrelationText, 0.95)
sample_random_99_text_candidates <- sample_random_99[sample_random_99$MatthewsCorrelationText >= MatthewsCorrelationText_95,]
# code
setorderv(sample_random_99, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
MatthewsCorrelationCode_95 <- quantile(sample_random_99$MatthewsCorrelationCode, 0.95)
sample_random_99_code_candidates <- sample_random_99[sample_random_99$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

# sample randomly drawn from all Java SO posts (tagged with <java> or <android>) with at least two versions
sample_java_random <- add_matthews_correlation(sample_java_random)
# text
setorderv(sample_java_random, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))
MatthewsCorrelationText_95 <- quantile(sample_java_random$MatthewsCorrelationText, 0.95)
sample_java_random_text_candidates <- sample_java_random[sample_java_random$MatthewsCorrelationText >= MatthewsCorrelationText_95,]
# code
setorderv(sample_java_random, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
MatthewsCorrelationCode_95 <- quantile(sample_java_random$MatthewsCorrelationCode, 0.95)
sample_java_random_code_candidates <- sample_java_random[sample_java_random$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

# sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_unclear_matching <- add_matthews_correlation(sample_unclear_matching)
# text
setorderv(sample_unclear_matching, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))
MatthewsCorrelationText_95 <- quantile(sample_unclear_matching$MatthewsCorrelationText, 0.95)
sample_unclear_matching_text_candidates <- sample_unclear_matching[sample_unclear_matching$MatthewsCorrelationText >= MatthewsCorrelationText_95,]
# code
setorderv(sample_unclear_matching, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
MatthewsCorrelationCode_95 <- quantile(sample_unclear_matching$MatthewsCorrelationCode, 0.95)
sample_unclear_matching_code_candidates <- sample_unclear_matching[sample_unclear_matching$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

# sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_multiple_possible_links <- add_matthews_correlation(sample_multiple_possible_links)
# text
setorderv(sample_multiple_possible_links, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))
MatthewsCorrelationText_95 <- quantile(sample_multiple_possible_links$MatthewsCorrelationText, 0.95)
sample_multiple_possible_links_text_candidates <- sample_multiple_possible_links[sample_multiple_possible_links$MatthewsCorrelationText >= MatthewsCorrelationText_95,]
# code
setorderv(sample_multiple_possible_links, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
MatthewsCorrelationCode_95 <- quantile(sample_multiple_possible_links$MatthewsCorrelationCode, 0.95)
sample_multiple_possible_links_code_candidates <- sample_multiple_possible_links[sample_multiple_possible_links$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

# text
length(intersect(unique(sample_random_text_candidates$Metric), unique(sample_random_99_text_candidates$Metric)))
# 34
length(intersect(unique(sample_random_text_candidates$Metric), unique(sample_java_random_text_candidates$Metric)))
# 27 
length(intersect(unique(sample_random_text_candidates$Metric), unique(sample_unclear_matching_text_candidates$Metric)))
# 25
length(intersect(unique(sample_random_text_candidates$Metric), unique(sample_multiple_possible_links_text_candidates$Metric)))
# 18
length(intersect(
  intersect(unique(sample_random_text_candidates$Metric), unique(sample_java_random_text_candidates$Metric)),
  intersect(unique(sample_unclear_matching_text_candidates$Metric), unique(sample_multiple_possible_links_text_candidates$Metric))
))
# 5
length(intersect(
  intersect(unique(sample_random_text_candidates$Metric), unique(sample_java_random_text_candidates$Metric)),
  unique(sample_unclear_matching_text_candidates$Metric)
))
# 14

# code
length(intersect(unique(sample_random_code_candidates$Metric), unique(sample_random_99_code_candidates$Metric)))
# 17
length(intersect(unique(sample_random_code_candidates$Metric), unique(sample_java_random_code_candidates$Metric)))
# 24
length(intersect(unique(sample_random_code_candidates$Metric), unique(sample_unclear_matching_code_candidates$Metric)))
# 19
length(intersect(unique(sample_random_code_candidates$Metric), unique(sample_multiple_possible_links_code_candidates$Metric)))
# 26
length(intersect(
  intersect(unique(sample_random_code_candidates$Metric), unique(sample_java_random_code_candidates$Metric)),
  intersect(unique(sample_unclear_matching_code_candidates$Metric), unique(sample_multiple_possible_links_code_candidates$Metric))
))
# 7
length(intersect(
  intersect(unique(sample_random_code_candidates$Metric), unique(sample_java_random_code_candidates$Metric)),
  unique(sample_unclear_matching_code_candidates$Metric)
))
# 13

