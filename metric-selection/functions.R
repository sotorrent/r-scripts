library(data.table)

merge_samples <- function(sample1, sample2) {
  sample_merged <- merge(sample1, sample2, by=c("MetricType", "Metric", "Threshold"))
  sample_merged$Runtime <- as.numeric(sample_merged$Runtime.x) + as.numeric(sample_merged$Runtime.y)
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

merge_samples_combined <- function(sample1, sample2) {
  sample_merged <- merge(sample1, sample2, by=c("MetricTypeText", "MetricText", "ThresholdText", "MetricTypeTextBackup", "MetricTextBackup", "ThresholdTextBackup", "MetricTypeCode", "MetricCode", "ThresholdCode", "MetricTypeCodeBackup", "MetricCodeBackup", "ThresholdCodeBackup"))
  sample_merged$Runtime <- as.numeric(sample_merged$Runtime.x) + as.numeric(sample_merged$Runtime.y)
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
  
  return(filter_columns_combined(sample_merged))
}

filter_columns <- function(df) {
  return(df[,c(
    "MetricType", "Metric", "Threshold", "Runtime",
    "PostCount", "PostVersionCount", "PostBlockVersionCount", "PossibleComparisons",
    "TextBlockVersionCount", "PossibleComparisonsText", "TruePositivesText", "TrueNegativesText", "FalsePositivesText", "FalseNegativesText", "FailuresText",
    "CodeBlockVersionCount", "PossibleComparisonsCode", "TruePositivesCode", "TrueNegativesCode", "FalsePositivesCode", "FalseNegativesCode", "FailuresCode"
  )])
}

filter_columns_combined <- function(df) {
  return(df[,c(
    "MetricTypeText", "MetricText", "ThresholdText", "MetricTypeTextBackup", "MetricTextBackup", "ThresholdTextBackup", "MetricTypeCode", "MetricCode", "ThresholdCode", "MetricTypeCodeBackup", "MetricCodeBackup", "ThresholdCodeBackup",
    "Runtime",
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

read_metrics_evaluation_per_post <- function(iteration, sample_name) {
  default_metrics_path <- paste0(sample_name, "-", iteration)
  
  # samples randomly drawn from all SO posts
  sample_100_1 <- fread(paste0(default_metrics_path, "/PostId_VersionCount_SO_17-06_sample_100_1_per_post.csv"), header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
  sample_100_2 <- fread(paste0(default_metrics_path, "/PostId_VersionCount_SO_17-06_sample_100_2_per_post.csv"), header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
  sample_random <<- rbind(sample_100_1, sample_100_2)
  sample_random <<- sample_random[,c("PostId", "FalsePositivesText", "FalseNegativesText", "FalsePositivesCode", "FalseNegativesCode")]
  
  # samples randomly drawn from all SO posts with at least seven versions (99% quantile of version count of all posts)
  sample_100_1_99 <- fread(paste0(default_metrics_path, "/PostId_VersionCount_SO_17-06_sample_100_1+_per_post.csv"), header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
  sample_100_2_99 <- fread(paste0(default_metrics_path, "/PostId_VersionCount_SO_17-06_sample_100_2+_per_post.csv"), header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
  sample_random_99 <<- rbind(sample_100_1_99, sample_100_2_99)
  sample_random_99 <<- sample_random_99[,c("PostId", "FalsePositivesText", "FalseNegativesText", "FalsePositivesCode", "FalseNegativesCode")]
  
  # samples randomly drawn from all Java SO posts (tagged with <java> or <android>) with at least two versions
  sample_java_100_1 <- fread(paste0(default_metrics_path, "/PostId_VersionCount_SO_Java_17-06_sample_100_1_per_post.csv"), header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
  sample_java_100_2 <- fread(paste0(default_metrics_path, "/PostId_VersionCount_SO_Java_17-06_sample_100_2_per_post.csv"), header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
  sample_java_random <<- rbind(sample_java_100_1, sample_java_100_2)
  sample_java_random <<- sample_java_random[,c("PostId", "FalsePositivesText", "FalseNegativesText", "FalsePositivesCode", "FalseNegativesCode")]
  
  # sample with multiple possible connections (to test matching strategy)
  sample_multiple_possible_links <<- fread(paste0(default_metrics_path, "/PostId_VersionCount_SO_17-06_sample_100_multiple_possible_links_per_post.csv"), header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
  sample_multiple_possible_links <<- sample_multiple_possible_links[,c("PostId", "FalsePositivesText", "FalseNegativesText", "FalsePositivesCode", "FalseNegativesCode")]
}

retrieve_fp_fn <- function(iteration) {
  fp_fn_path <- paste0("fp+fn", "-", iteration)
  
  fp_text <<- rbind(
    sample_random[sample_random$FalsePositivesText>0,],
    sample_random_99[sample_random_99$FalsePositivesText>0,],
    sample_java_random[sample_java_random$FalsePositivesText>0,]
  )
  
  fn_text <<- rbind(
    sample_random[sample_random$FalseNegativesText>0,],
    sample_random_99[sample_random_99$FalseNegativesText>0,],
    sample_java_random[sample_java_random$FalseNegativesText>0,]
  )
  
  fp_code <<- rbind(
    sample_random[sample_random$FalsePositivesCode>0,],
    sample_random_99[sample_random_99$FalsePositivesCode>0,],
    sample_java_random[sample_java_random$FalsePositivesCode>0,]
  )
  
  fn_code <<- rbind(
    sample_random[sample_random$FalseNegativesCode>0,],
    sample_random_99[sample_random_99$FalseNegativesCode>0,],
    sample_java_random[sample_java_random$FalseNegativesCode>0,]
  )
  
  fp_text_posts <<- unique(fp_text$PostId)
  fn_text_posts <<- unique(fn_text$PostId)
  fp_code_posts <<- unique(fp_code$PostId)
  fn_code_posts <<- unique(fn_code$PostId)
  f_text  <<- unique(c(fp_text_posts, fn_text_posts))
  f_code <<- unique(c(fp_code_posts, fn_code_posts))
  
  # write post ids of false postives/negatives to separate CSV files
  write.table(fp_text_posts, file=paste0(fp_fn_path, "/fp_text.csv"), sep=";", col.names=FALSE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
  write.table(fn_text_posts, file=paste0(fp_fn_path, "/fn_text.csv"), sep=";", col.names=FALSE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
  write.table(fp_code_posts, file=paste0(fp_fn_path, "/fp_code.csv"), sep=";", col.names=FALSE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
  write.table(fn_code_posts, file=paste0(fp_fn_path, "/fn_code.csv"), sep=";", col.names=FALSE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
  
  write.table(f_text, file=paste0(fp_fn_path, "/f_text.csv"), sep=";", col.names=FALSE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
  write.table(f_code, file=paste0(fp_fn_path, "/f_code.csv"), sep=";", col.names=FALSE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
}


read_metrics_evaluation_per_sample <- function(iteration, sample_name, combined=TRUE) {
  all_metrics_path <- paste0(sample_name, "-", iteration)
  
  # samples randomly drawn from all SO posts
  sample_100_1 <- fread(paste0(all_metrics_path, "/PostId_VersionCount_SO_17-06_sample_100_1_per_sample.csv"), header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
  sample_100_2 <- fread(paste0(all_metrics_path, "/PostId_VersionCount_SO_17-06_sample_100_2_per_sample.csv"), header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
  if (combined) {
    sample_random <<- merge_samples_combined(sample_100_1, sample_100_2)
  } else {
    sample_random <<- merge_samples(sample_100_1, sample_100_2)
  }
  
  # samples randomly drawn from all SO posts with at least seven versions (99% quantile of version count of all posts)
  sample_100_1_99 <- fread(paste0(all_metrics_path, "/PostId_VersionCount_SO_17-06_sample_100_1+_per_sample.csv"), header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
  sample_100_2_99 <- fread(paste0(all_metrics_path, "/PostId_VersionCount_SO_17-06_sample_100_2+_per_sample.csv"), header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
  if (combined) {
    sample_random_99 <<- merge_samples_combined(sample_100_1_99, sample_100_2_99)
  } else {
    sample_random_99 <<- merge_samples(sample_100_1_99, sample_100_2_99)
  }
  
  # samples randomly drawn from all Java SO posts (tagged with <java> or <android>) with at least two versions
  sample_java_100_1 <- fread(paste0(all_metrics_path, "/PostId_VersionCount_SO_Java_17-06_sample_100_1_per_sample.csv"), header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
  sample_java_100_2 <- fread(paste0(all_metrics_path, "/PostId_VersionCount_SO_Java_17-06_sample_100_2_per_sample.csv"), header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
  if (combined) {
    sample_java_random <<- merge_samples_combined(sample_java_100_1, sample_java_100_2)
  } else {
    sample_java_random <<- merge_samples(sample_java_100_1, sample_java_100_2)
  }
  
  # sample in which we moved posts with unclear matching according to the comments added in the GT App
  sample_unclear_matching <- fread(paste0(all_metrics_path, "/PostId_VersionCount_SO_17_06_sample_unclear_matching_per_sample.csv"), header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
  if (combined) {
    sample_unclear_matching <<- filter_columns_combined(sample_unclear_matching)
  } else {
    sample_unclear_matching <<- filter_columns(sample_unclear_matching)
  }
  
  # sample with multiple possible connections (to test matching strategy)
  sample_multiple_possible_links <<- fread(paste0(all_metrics_path, "/PostId_VersionCount_SO_17-06_sample_100_multiple_possible_links_per_sample.csv"), header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
  
  if (combined) {
    sample_multiple_possible_links <<- filter_columns_combined(sample_multiple_possible_links)
  } else {
    sample_multiple_possible_links <<- filter_columns(sample_multiple_possible_links)
  }
}

merge_and_matthews_correlation <- function(combined=TRUE) {
  # merge relevant samples
  if (combined) {
    sample_candidates <<- merge_samples_combined(sample_random, sample_java_random)
    sample_candidates <<- merge_samples_combined(sample_candidates, sample_random_99)  
  } else {
    sample_candidates <<- merge_samples(sample_random, sample_java_random)
    sample_candidates <<- merge_samples(sample_candidates, sample_random_99)
  }

  # calculate Matthews correlation
  sample_candidates <<- add_matthews_correlation(sample_candidates)
  sample_random <<- add_matthews_correlation(sample_random)
  sample_java_random <<- add_matthews_correlation(sample_java_random)
  sample_random_99 <<- add_matthews_correlation(sample_random_99)
}