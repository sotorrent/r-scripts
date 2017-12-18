setwd("F:/Git/github/r-scripts/metric-selection/comparison-by-sample") # Pfad bitte anpassen
#setwd("/Users/sebastian/git/github/r-scripts/metric-selection/comparison-by-sample")

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
  
  sample_merged <- sample_merged[,c(
    "MetricType", "Metric", "Threshold", "Runtime",
    "PostCount", "PostVersionCount", "PostBlockVersionCount", "PossibleComparisons",
    "TextBlockVersionCount", "PossibleComparisonsText", "TruePositivesText", "TrueNegativesText", "FalsePositivesText", "FalseNegativesText", "FailuresText",
    "CodeBlockVersionCount", "PossibleComparisonsCode", "TruePositivesCode", "TrueNegativesCode", "FalsePositivesCode", "FalseNegativesCode", "FailuresCode"
  )]
  
  return(sample_merged)
}
  
  
# read results of first run with all metrics
library(data.table)
sample_unclear_matching <- fread("all/PostId_VersionCount_SO_17_06_sample_unclear_matching_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)

# samples randomly drawn from all SO posts
sample_100_1 <- fread("all/PostId_VersionCount_SO_17-06_sample_100_1_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2 <- fread("all/PostId_VersionCount_SO_17-06_sample_100_2_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random <- merge_samples(sample_100_1, sample_100_2)

