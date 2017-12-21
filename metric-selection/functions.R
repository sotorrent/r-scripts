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

recall <- function(TP, FN) {
  TP <- as.numeric(TP)
  FN <- as.numeric(FN)
  return(TP / (TP + FN))
}

inverse_recall <- function(TN, FP) {
  TN <- as.numeric(TN)
  FP <- as.numeric(FP)
  return(TN / (TN + FP))
}
  
add_recall <- function(df) {
  df$RecallText <- recall(df$TruePositivesText, df$FalseNegativesText)
  df$RecallCode <- recall(df$TruePositivesCode, df$FalseNegativesCode)
  df$InverseRecallText <- inverse_recall(df$TrueNegativesText, df$FalsePositivesText)
  df$InverseRecallCode <- inverse_recall(df$TrueNegativesCode, df$FalsePositivesCode)
  return(df)
}