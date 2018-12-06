# set working directory (see https://stackoverflow.com/a/35842119)
dir = tryCatch({
  # script being sourced
  getSrcDirectory()[1]
}, error = function(e) {
  # script being run in RStudio
  dirname(rstudioapi::getActiveDocumentContext()$path)
})
setwd(dir)

source("functions.R")

library(data.table)


# read results of first run with all metrics

# samples randomly drawn from all SO posts
sample_100_1 <- fread("all-new/PostId_VersionCount_SO_17-06_sample_100_1_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2 <- fread("all-new/PostId_VersionCount_SO_17-06_sample_100_2_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random <- merge_samples_combined(sample_100_1, sample_100_2)
rm(sample_100_1, sample_100_2)

# samples randomly drawn from all SO posts with at least seven versions (99% quantile of version count of all posts)
sample_100_1_99 <- fread("all-new/PostId_VersionCount_SO_17-06_sample_100_1+_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2_99 <- fread("all-new/PostId_VersionCount_SO_17-06_sample_100_2+_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random_99 <- merge_samples_combined(sample_100_1_99, sample_100_2_99)
rm(sample_100_1_99, sample_100_2_99)

# samples randomly drawn from all Java SO posts (tagged with <java> or <android>) with at least two versions
sample_java_100_1 <- fread("all-new/PostId_VersionCount_SO_Java_17-06_sample_100_1_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_java_100_2 <- fread("all-new/PostId_VersionCount_SO_Java_17-06_sample_100_2_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_java_random <- merge_samples_combined(sample_java_100_1, sample_java_100_2)
rm(sample_java_100_1, sample_java_100_2)

# sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_unclear_matching <- fread("all-new/PostId_VersionCount_SO_17_06_sample_unclear_matching_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_unclear_matching <- filter_columns_combined(sample_unclear_matching)

# sample with multiple possible connections (to test matching strategy)
sample_multiple_possible_links <- fread("all-new/PostId_VersionCount_SO_17-06_sample_100_multiple_possible_links_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_multiple_possible_links <- filter_columns_combined(sample_multiple_possible_links)


#### calculate Matthews correlation and select candidates using 95% quantile for text and code


### sample randomly drawn from all SO posts
sample_random <- add_matthews_correlation(sample_random)

# analyze runtime
summary(sample_random$Runtime)
# Min.     1st Qu.      Median        Mean     3rd Qu.        Max. 
# 8182834   223423971   310652749   787078375   468136102 12663494809
Runtime_Q3 <- quantile(sample_random$Runtime, 0.75)

## text
# order
setorderv(sample_random, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze quality
summary(sample_random$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3656  0.6915  0.8091  0.7463  0.8577  0.9084 
boxplot(sample_random$MatthewsCorrelationText)

# select candidates
MatthewsCorrelationText_95 <- quantile(sample_random$MatthewsCorrelationText, 0.9)
sample_random_text_candidates <- sample_random[sample_random$MatthewsCorrelationText >= MatthewsCorrelationText_95,]

# backup metric
backup_candidates <- sample_random[sample_random$MetricTypeText == "EDIT" | grepl("token", sample_random$MetricText, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_95_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.9)
sample_random_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup & backup_candidates$Runtime <= Runtime_Q3,]

## code
# order
setorderv(sample_random, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# analyze quality
summary(sample_random$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6599  0.8624  0.9383  0.8970  0.916  0.9792
boxplot(sample_random$MatthewsCorrelationCode)

# select candidates
MatthewsCorrelationCode_95 <- quantile(sample_random$MatthewsCorrelationCode, 0.9)
sample_random_code_candidates <- sample_random[sample_random$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

# backup metric
backup_candidates <- sample_random[sample_random$MetricTypeCode == "EDIT" | grepl("token", sample_random$MetricCode, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_95_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.9)
sample_random_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup & backup_candidates$Runtime <= Runtime_Q3,]


### sample randomly drawn from all SO posts with at least seven versions (99% quantile of version count of all posts)
sample_random_99 <- add_matthews_correlation(sample_random_99)

# analyze runtime
summary(sample_random_99$Runtime)
# Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# 197468486   8203734960  11487859587  25206955934  17596325198 362320100616 
Runtime_Q3 <- quantile(sample_random_99$Runtime, 0.75)

## text
# order
setorderv(sample_random_99, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze quality
summary(sample_random_99$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3984  0.6376  0.7763  0.7185  0.8285  0.8832 
boxplot(sample_random_99$MatthewsCorrelationText)

# select candidates
MatthewsCorrelationText_95 <- quantile(sample_random_99$MatthewsCorrelationText, 0.9)
sample_random_99_text_candidates <- sample_random_99[sample_random_99$MatthewsCorrelationText >= MatthewsCorrelationText_95,]

# backup metric
backup_candidates <- sample_random_99[sample_random_99$MetricTypeText == "EDIT" | grepl("token", sample_random_99$MetricText, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_95_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.9)
sample_random_99_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup & backup_candidates$Runtime <= Runtime_Q3,]

## code
# order
setorderv(sample_random_99, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# analyze quality
summary(sample_random_99$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5133  0.7786  0.8996  0.8352  0.9321  0.9628
boxplot(sample_random_99$MatthewsCorrelationCode)

# select candidates
MatthewsCorrelationCode_95 <- quantile(sample_random_99$MatthewsCorrelationCode, 0.9)
sample_random_99_code_candidates <- sample_random_99[sample_random_99$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

# backup metric
backup_candidates <- sample_random_99[sample_random_99$MetricTypeCode == "EDIT" | grepl("token", sample_random_99$MetricCode, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_95_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.9)
sample_random_99_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup & backup_candidates$Runtime <= Runtime_Q3,]


### sample randomly drawn from all Java SO posts (tagged with <java> or <android>) with at least two versions
sample_java_random <- add_matthews_correlation(sample_java_random)

# analyze runtime
summary(sample_java_random$Runtime)
# Min.     1st Qu.      Median        Mean     3rd Qu.        Max. 
# 19614757   651001234   907995053  1878502349  1233621914 36540435445 
Runtime_Q3 <- quantile(sample_java_random$Runtime, 0.75)

## text
# order
setorderv(sample_java_random, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze quality
summary(sample_java_random$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3958  0.6698  0.7745  0.7291  0.8212  0.8902
boxplot(sample_java_random$MatthewsCorrelationText)

# select candidates
MatthewsCorrelationText_95 <- quantile(sample_java_random$MatthewsCorrelationText, 0.9)
sample_java_random_text_candidates <- sample_java_random[sample_java_random$MatthewsCorrelationText >= MatthewsCorrelationText_95,]

# backup metric
backup_candidates <- sample_java_random[sample_java_random$MetricTypeText == "EDIT" | grepl("token", sample_java_random$MetricText, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_95_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.9)
sample_java_random_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup & backup_candidates$Runtime <= Runtime_Q3,]

## code
# order
setorderv(sample_java_random, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# analyze quality
summary(sample_java_random$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6291  0.8359  0.9105  0.8812  0.912  0.9755 
boxplot(sample_java_random$MatthewsCorrelationCode)

# select candidates
MatthewsCorrelationCode_95 <- quantile(sample_java_random$MatthewsCorrelationCode, 0.9)
sample_java_random_code_candidates <- sample_java_random[sample_java_random$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

# backup metric
backup_candidates <- sample_java_random[sample_java_random$MetricTypeCode == "EDIT" | grepl("token", sample_java_random$MetricCode, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_95_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.9)
sample_java_random_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup & backup_candidates$Runtime <= Runtime_Q3,]


### sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_unclear_matching <- add_matthews_correlation(sample_unclear_matching)

# analyze runtime
summary(sample_unclear_matching$Runtime)
# Min.     1st Qu.      Median        Mean     3rd Qu.        Max. 
# 20462868  1546121768  2183522018  3059756377  3025247059 29855452384 
Runtime_Q3 <- quantile(sample_unclear_matching$Runtime, 0.75)

## text
# order
setorderv(sample_unclear_matching, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze quality
summary(sample_unclear_matching$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2654  0.6110  0.7232  0.6862  0.7828  0.8804 
boxplot(sample_unclear_matching$MatthewsCorrelationText)

# select candidates
MatthewsCorrelationText_95 <- quantile(sample_unclear_matching$MatthewsCorrelationText, 0.9)
sample_unclear_matching_text_candidates <- sample_unclear_matching[sample_unclear_matching$MatthewsCorrelationText >= MatthewsCorrelationText_95,]

# backup metric
backup_candidates <- sample_unclear_matching[sample_unclear_matching$MetricTypeText == "EDIT" | grepl("token", sample_unclear_matching$MetricText, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_95_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.9)
sample_unclear_matching_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup & backup_candidates$Runtime <= Runtime_Q3,]


## code
# order
setorderv(sample_unclear_matching, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# analyze quality
summary(sample_unclear_matching$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5545  0.6959  0.7537  0.7390  0.7950  0.8671
boxplot(sample_unclear_matching$MatthewsCorrelationCode)

# select candidates
MatthewsCorrelationCode_95 <- quantile(sample_unclear_matching$MatthewsCorrelationCode, 0.9)
sample_unclear_matching_code_candidates <- sample_unclear_matching[sample_unclear_matching$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

# backup metric
backup_candidates <- sample_unclear_matching[sample_unclear_matching$MetricTypeCode == "EDIT" | grepl("token", sample_unclear_matching$MetricCode, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_95_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.9)
sample_unclear_matching_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup & backup_candidates$Runtime <= Runtime_Q3,]



### sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_multiple_possible_links <- add_matthews_correlation(sample_multiple_possible_links)

# analyze runtime
summary(sample_multiple_possible_links$Runtime)
# Min.     1st Qu.      Median        Mean     3rd Qu.        Max. 
# 31017599  1556218244  2122806975  3463719660  3061548129 28593309190 
Runtime_Q3 <- quantile(sample_multiple_possible_links$Runtime, 0.75)

## text
# order
setorderv(sample_multiple_possible_links, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze quality
summary(sample_multiple_possible_links$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5965  0.7579  0.8618  0.8237  0.9052  0.905 
boxplot(sample_multiple_possible_links$MatthewsCorrelationText)

# select candidates
MatthewsCorrelationText_95 <- quantile(sample_multiple_possible_links$MatthewsCorrelationText, 0.9)
sample_multiple_possible_links_text_candidates <- sample_multiple_possible_links[sample_multiple_possible_links$MatthewsCorrelationText >= MatthewsCorrelationText_95,]

# backup metric
backup_candidates <- sample_multiple_possible_links[sample_multiple_possible_links$MetricTypeText == "EDIT" | grepl("token", sample_multiple_possible_links$MetricText, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_95_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.9)
sample_multiple_possible_links_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup & backup_candidates$Runtime <= Runtime_Q3,]


## code
# order
setorderv(sample_multiple_possible_links, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# analyze quality
summary(sample_multiple_possible_links$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.7687  0.9250  0.956  0.9302  0.9662  0.9926 
boxplot(sample_multiple_possible_links$MatthewsCorrelationCode)

# select candidates
MatthewsCorrelationCode_95 <- quantile(sample_multiple_possible_links$MatthewsCorrelationCode, 0.9)
sample_multiple_possible_links_code_candidates <- sample_multiple_possible_links[sample_multiple_possible_links$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

# backup metric
backup_candidates <- sample_multiple_possible_links[sample_multiple_possible_links$MetricTypeCode == "EDIT" | grepl("token", sample_multiple_possible_links$MetricCode, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_95_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.9)
sample_multiple_possible_links_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup & backup_candidates$Runtime <= Runtime_Q3,]


# DECISION: For second run, select metrics that are in 90% quantile of sample_random, sample_java_random, and sample_random_99 for text or code,
#           with runtime <= Q_3 for backup metrics(edit-based metrics slowed down the evaluation run enourmously)

## best metrics

# text
candidates_text <- intersect(
  intersect(unique(sample_random_text_candidates$MetricText), unique(sample_java_random_text_candidates$MetricText)),
  unique(sample_random_99_text_candidates$MetricText)
)
length(candidates_text)
# 52


# code
candidates_code <- intersect(
  intersect(unique(sample_random_code_candidates$MetricCode), unique(sample_java_random_code_candidates$MetricCode)),
  unique(sample_random_99_code_candidates$MetricCode)
)
length(candidates_code)
# 16

# both
candidates <- unique(c(candidates_text, candidates_code))
length(candidates)
# 61
candidates
# [1] "cosineTwoGramNormalizedNormalizedTermFrequency"   
# [2] "twoGramDice"                                      
# [3] "winnowingTwoGramDiceNormalized"                   
# [4] "cosineTwoGramNormalizedBool"                      
# [5] "cosineFourGramNormalizedTermFrequency"            
# [6] "twoGramJaccard"                                   
# [7] "twoGramJaccardNormalized"                         
# [8] "winnowingTwoGramJaccard"                          
# [9] "winnowingTwoGramJaccardNormalized"                
# [10] "twoGramDiceNormalizedPadding"                     
# [11] "twoGramDiceNormalized"                            
# [12] "winnowingThreeGramLongestCommonSubsequence"       
# [13] "winnowingThreeGramOptimalAlignment"               
# [14] "threeGramDiceNormalized"                          
# [15] "manhattanThreeGramNormalized"                     
# [16] "threeGramJaccard"                                 
# [17] "threeGramJaccardNormalized"                       
# [18] "threeGramJaccardNormalizedPadding"                
# [19] "cosineThreeGramNormalizedBool"                    
# [20] "winnowingThreeGramDice"                           
# [21] "manhattanTwoShingleNormalized"                    
# [22] "cosineThreeGramNormalizedNormalizedTermFrequency" 
# [23] "cosineFourGramNormalizedNormalizedTermFrequency"  
# [24] "winnowingTwoGramDice"                             
# [25] "cosineTokenNormalizedNormalizedTermFrequency"     
# [26] "cosineTwoShingleNormalizedBool"                   
# [27] "cosineFourGramNormalizedBool"                     
# [28] "cosineFiveGramNormalizedBool"                     
# [29] "cosineFiveGramNormalizedNormalizedTermFrequency"  
# [30] "fourGramDice"                                     
# [31] "fiveGramDice"                                     
# [32] "fourGramDiceNormalized"                           
# [33] "fiveGramDiceNormalized"                           
# [34] "fourGramOverlapNormalizedPadding"                 
# [35] "manhattanFourGramNormalized"                      
# [36] "twoGramJaccardNormalizedPadding"                  
# [37] "threeGramDiceNormalizedPadding"                   
# [38] "threeGramDice"                                    
# [39] "fourGramJaccard"                                  
# [40] "fourGramDiceNormalizedPadding"                    
# [41] "cosineTwoShingleNormalizedTermFrequency"          
# [42] "fiveGramJaccard"                                  
# [43] "cosineTokenNormalizedBool"                        
# [44] "twoShingleDiceNormalized"                         
# [45] "manhattanFiveGramNormalized"                      
# [46] "winnowingThreeGramJaccard"                        
# [47] "fiveGramDiceNormalizedPadding"                    
# [48] "fiveGramOverlapNormalizedPadding"                 
# [49] "fourGramJaccardNormalized"                        
# [50] "fiveGramJaccardNormalized"                        
# [51] "cosineTwoShingleNormalizedNormalizedTermFrequency"
# [52] "cosineThreeGramNormalizedTermFrequency"           
# [53] "tokenDice"                                        
# [54] "tokenJaccard"                                     
# [55] "tokenDiceNormalized"                              
# [56] "tokenJaccardNormalized"                           
# [57] "manhattanTokenNormalized"                         
# [58] "cosineTokenNormalizedTermFrequency"               
# [59] "fourGramJaccardNormalizedPadding"                 
# [60] "fiveGramJaccardNormalizedPadding"                 
# [61] "twoShingleJaccardNormalized"                      


## backup metrics

# text
candidates_backup_text <- intersect(
  intersect(unique(sample_random_text_candidates_backup$MetricText), unique(sample_java_random_text_candidates_backup$MetricText)),
  unique(sample_random_99_text_candidates_backup$MetricText)
)
length(candidates_backup_text)
# 7

# code
candidates_backup_code <- intersect(
  intersect(unique(sample_random_code_candidates_backup$MetricCode), unique(sample_java_random_code_candidates_backup$MetricCode)),
  unique(sample_random_99_code_candidates_backup$MetricCode)
)
length(candidates_backup_code)
# 5

# both
candidates_backup <- unique(c(candidates_backup_text, candidates_backup_code))
length(candidates_backup)
# 7
candidates_backup
# [1] "tokenDice"                                   
# [2] "cosineTokenNormalizedNormalizedTermFrequency"
# [3] "tokenJaccard"                                
# [4] "cosineTokenNormalizedBool"                   
# [5] "tokenDiceNormalized"                         
# [6] "tokenJaccardNormalized"                      
# [7] "manhattanTokenNormalized"    


###############################################################################################


### second run with selected metrics ###

# samples randomly drawn from all SO posts
sample_100_1 <- fread("selected-new/PostId_VersionCount_SO_17-06_sample_100_1_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2 <- fread("selected-new/PostId_VersionCount_SO_17-06_sample_100_2_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random <- merge_samples_combined(sample_100_1, sample_100_2)
rm(sample_100_1, sample_100_2)

# samples randomly drawn from all SO posts with at least seven versions (99% quantile of version count of all posts)
sample_100_1_99 <- fread("selected-new/PostId_VersionCount_SO_17-06_sample_100_1+_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2_99 <- fread("selected-new/PostId_VersionCount_SO_17-06_sample_100_2+_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random_99 <- merge_samples_combined(sample_100_1_99, sample_100_2_99)
rm(sample_100_1_99, sample_100_2_99)

# samples randomly drawn from selected Java SO posts (tagged with <java> or <android>) with at least two versions
sample_java_100_1 <- fread("selected-new/PostId_VersionCount_SO_Java_17-06_sample_100_1_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_java_100_2 <- fread("selected-new/PostId_VersionCount_SO_Java_17-06_sample_100_2_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_java_random <- merge_samples_combined(sample_java_100_1, sample_java_100_2)
rm(sample_java_100_1, sample_java_100_2)

# sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_unclear_matching <- fread("selected-new/PostId_VersionCount_SO_17_06_sample_unclear_matching_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_unclear_matching <- filter_columns_combined(sample_unclear_matching)

# sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_multiple_possible_links <- fread("selected-new/PostId_VersionCount_SO_17-06_sample_100_multiple_possible_links_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_multiple_possible_links <- filter_columns_combined(sample_multiple_possible_links)


# DECISION: Select metrics that are in 95% quantile of sample_random, sample_java_random, and sample_random_99 for text/code

# merge relevant samples
sample_candidates <- merge_samples_combined(sample_random, sample_java_random)
sample_candidates <- merge_samples_combined(sample_candidates, sample_random_99)
# calculate Matthews correlation
sample_candidates <- add_matthews_correlation(sample_candidates)
sample_random <- add_matthews_correlation(sample_random)
sample_java_random <- add_matthews_correlation(sample_java_random)
sample_random_99 <- add_matthews_correlation(sample_random_99)

## text

# order samples
setorderv(sample_random, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))
setorderv(sample_java_random, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))
setorderv(sample_random_99, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# select candidates
MatthewsCorrelationText_95 <- quantile(sample_random$MatthewsCorrelationText, 0.95)
sample_random_text_candidates <- sample_random[sample_random$MatthewsCorrelationText >= MatthewsCorrelationText_95,]

MatthewsCorrelationText_95 <- quantile(sample_java_random$MatthewsCorrelationText, 0.95)
sample_java_random_text_candidates <- sample_java_random[sample_java_random$MatthewsCorrelationText >= MatthewsCorrelationText_95,]

MatthewsCorrelationText_95 <- quantile(sample_random_99$MatthewsCorrelationText, 0.95)
sample_random_99_text_candidates <- sample_random_99[sample_random_99$MatthewsCorrelationText >= MatthewsCorrelationText_95,]

# backup metric
backup_candidates <- sample_random[sample_random$MetricTypeText == "EDIT" | grepl("token", sample_random$MetricText, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_95_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.95)
sample_random_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup,]

backup_candidates <- sample_java_random[sample_java_random$MetricTypeText == "EDIT" | grepl("token", sample_java_random$MetricText, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_95_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.95)
sample_java_random_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup,]

backup_candidates <- sample_random_99[sample_random_99$MetricTypeText == "EDIT" | grepl("token", sample_random_99$MetricText, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_95_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.95)
sample_random_99_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup,]


# final candidates
candidates_text <- intersect(
  intersect(unique(sample_random_text_candidates$MetricText), unique(sample_java_random_text_candidates$MetricText)),
  unique(sample_random_99_text_candidates$MetricText)
)
length(candidates_text)
# 25
candidates_text
# [1] "cosineTwoGramNormalizedNormalizedTermFrequency"   
# [2] "cosineThreeGramNormalizedNormalizedTermFrequency" 
# [3] "cosineThreeGramNormalizedBool"                    
# [4] "manhattanThreeGramNormalized"                     
# [5] "cosineFourGramNormalizedNormalizedTermFrequency"  
# [6] "manhattanFourGramNormalized"                      
# [7] "cosineFourGramNormalizedBool"                     
# [8] "cosineFiveGramNormalizedBool"                     
# [9] "fourGramDiceNormalizedPadding"                    
# [10] "fourGramJaccardNormalizedPadding"                 
# [11] "fourGramDice"                                     
# [12] "fiveGramDice"                                     
# [13] "fourGramJaccard"                                  
# [14] "fiveGramJaccard"                                  
# [15] "threeGramDiceNormalized"                          
# [16] "fourGramDiceNormalized"                           
# [17] "fiveGramDiceNormalized"                           
# [18] "threeGramJaccardNormalized"                       
# [19] "fourGramJaccardNormalized"                        
# [20] "twoShingleDiceNormalized"                         
# [21] "twoShingleJaccardNormalized"                      
# [22] "manhattanTwoShingleNormalized"                    
# [23] "cosineTwoShingleNormalizedNormalizedTermFrequency"
# [24] "cosineTwoShingleNormalizedTermFrequency"          
# [25] "cosineTwoShingleNormalizedBool"   

summary(sample_candidates$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4032  0.6791  0.8029  0.7520  0.8542  0.8840 

setorderv(sample_candidates, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))
sample_candidates_text <- sample_candidates[sample_candidates$MetricText %in% candidates_text,]

MatthewsCorrelationText_99 <- quantile(sample_candidates_text$MatthewsCorrelationText, 0.99)
MatthewsCorrelationText_99
# 99% 
# 0.8812886

sample_candidates_text[sample_candidates_text$MatthewsCorrelationText>=MatthewsCorrelationText_99, c("MetricText", "ThresholdText", "MatthewsCorrelationText", "Runtime")]
#                      MetricText ThresholdText MatthewsCorrelationText     Runtime
# 1:       fiveGramDiceNormalized          0.04               0.8840052 13902473845
# 2: cosineFiveGramNormalizedBool          0.05               0.8839202 22758535743
# 3:                 fiveGramDice          0.04               0.8838388 12034661521
# 4:              fiveGramJaccard          0.02               0.8838388 13733218548
# 5: cosineFiveGramNormalizedBool          0.03               0.8836873 22562328502
# 6: cosineFiveGramNormalizedBool          0.04               0.8836873 22709502049
# 7: cosineFiveGramNormalizedBool          0.06               0.8829495 22692362426
# 8:       fourGramDiceNormalized          0.05               0.8828646 13562650146
# 9:       fiveGramDiceNormalized          0.03               0.8828646 13669176751
# 10:                 fiveGramDice          0.03               0.8827059 12007111762
# 11: cosineFiveGramNormalizedBool          0.08               0.8820735 22733251324
# 12:                 fourGramDice          0.08               0.8819814 11725227207
# 13:                 fiveGramDice          0.05               0.8819814 12149066965
# 14: cosineFiveGramNormalizedBool          0.07               0.8819814 22780266663
# 15: cosineFiveGramNormalizedBool          0.14               0.8818780 22943880535
# 16:    fourGramJaccardNormalized          0.03               0.8818082 14888361132
# 17:  manhattanFourGramNormalized          0.05               0.8818082 21441765884
# 18: cosineFourGramNormalizedBool          0.17               0.8817583 21749765816
# 19:  manhattanFourGramNormalized          0.04               0.8817271 21402819562
# 20:                 fourGramDice          0.06               0.8816498 11672687473
# 21:              fourGramJaccard          0.03               0.8816498 13234364419
# 22: cosineFourGramNormalizedBool          0.07               0.8816498 21896497941
# 23:                 fourGramDice          0.05               0.8815762 11781944569
# 24: cosineFourGramNormalizedBool          0.16               0.8814194 21748786935
# 25: cosineFiveGramNormalizedBool          0.11               0.8814194 22647503662
# 26: cosineFiveGramNormalizedBool          0.10               0.8813133 23017541253


# backup metric
backup_candidates_text <- intersect(
  intersect(unique(sample_random_text_candidates_backup$MetricText), unique(sample_java_random_text_candidates_backup$MetricText)),
  unique(sample_random_99_text_candidates_backup$MetricText)
)
length(backup_candidates_text)
# 4
backup_candidates_text
# [1] "cosineTokenNormalizedNormalizedTermFrequency"
# [2] "cosineTokenNormalizedBool"                   
# [3] "tokenDiceNormalized"                         
# [4] "tokenJaccardNormalized"

setorderv(sample_candidates, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))
sample_candidates_backup_text <- sample_candidates[sample_candidates$MetricText %in% backup_candidates_text,]

MatthewsCorrelationText_99_backup <- quantile(sample_candidates_backup_text$MatthewsCorrelationText, 0.99)
MatthewsCorrelationText_99_backup
# 99% 
# 0.8798426 

sample_candidates_backup_text[sample_candidates_backup_text$MatthewsCorrelationText>=MatthewsCorrelationText_99_backup, c("MetricText", "ThresholdText", "MatthewsCorrelationText", "Runtime")]
#                   MetricText ThresholdText MatthewsCorrelationText    Runtime
# 1: cosineTokenNormalizedBool          0.19               0.8812106 9275689314
# 2: cosineTokenNormalizedBool          0.16               0.8809240 9142165190
# 3: cosineTokenNormalizedBool          0.15               0.8808357 9227731991
# 4: cosineTokenNormalizedBool          0.18               0.8801522 9276384771
# 5: cosineTokenNormalizedBool          0.14               0.8798658 9248218885


## code

# order samples
setorderv(sample_random, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
setorderv(sample_java_random, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
setorderv(sample_random_99, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# select candidates
MatthewsCorrelationCode_95 <- quantile(sample_random$MatthewsCorrelationCode, 0.95)
sample_random_code_candidates <- sample_random[sample_random$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

MatthewsCorrelationCode_95 <- quantile(sample_java_random$MatthewsCorrelationCode, 0.95)
sample_java_random_code_candidates <- sample_java_random[sample_java_random$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

MatthewsCorrelationCode_95 <- quantile(sample_random_99$MatthewsCorrelationCode, 0.95)
sample_random_99_code_candidates <- sample_random_99[sample_random_99$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

# backup metric
backup_candidates <- sample_random[sample_random$MetricTypeCode == "EDIT" | grepl("token", sample_random$MetricCode, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_95_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.95)
sample_random_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup,]

backup_candidates <- sample_java_random[sample_java_random$MetricTypeCode == "EDIT" | grepl("token", sample_java_random$MetricCode, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_95_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.95)
sample_java_random_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup,]

backup_candidates <- sample_random_99[sample_random_99$MetricTypeCode == "EDIT" | grepl("token", sample_random_99$MetricCode, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_95_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.95)
sample_random_99_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup,]


# final candidates
candidates_code <- intersect(
  intersect(unique(sample_random_code_candidates$MetricCode), unique(sample_java_random_code_candidates$MetricCode)),
  unique(sample_random_99_code_candidates$MetricCode)
)
length(candidates_code)
# 5
candidates_code
# [1] "tokenDice"                
# [2] "tokenJaccard"             
# [3] "tokenDiceNormalized"      
# [4] "tokenJaccardNormalized"   
# [5] "cosineTokenNormalizedBool" 

summary(sample_candidates$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5517  0.8254  0.9164  0.8704  0.9464  0.9639 

setorderv(sample_candidates, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
sample_candidates_code <- sample_candidates[sample_candidates$MetricCode %in% candidates_code,]

MatthewsCorrelationCode_99 <- quantile(sample_candidates_code$MatthewsCorrelationCode, 0.99)
MatthewsCorrelationCode_99
# 99% 
# 0.9616897

sample_candidates_code[sample_candidates_code$MatthewsCorrelationCode>=MatthewsCorrelationCode_99, c("MetricCode", "ThresholdCode", "MatthewsCorrelationCode", "Runtime")]
#                   MetricCode ThresholdCode MatthewsCorrelationCode    Runtime
# 1:       tokenDiceNormalized          0.10               0.9638847 7286795340
# 2:       tokenDiceNormalized          0.08               0.9627729 7399154375
# 3:       tokenDiceNormalized          0.09               0.9627729 7472382012
# 4:    tokenJaccardNormalized          0.05               0.9627729 7601886261
# 5:       tokenDiceNormalized          0.12               0.9616897 7245929933
# 6:       tokenDiceNormalized          0.11               0.9616897 7347308745
# 7:    tokenJaccardNormalized          0.06               0.9616897 7577012153
# 8: cosineTokenNormalizedBool          0.10               0.9616897 9180838103


# backup metric
backup_candidates_code <- intersect(
  intersect(unique(sample_random_code_candidates_backup$MetricCode), unique(sample_java_random_code_candidates_backup$MetricCode)),
  unique(sample_random_99_code_candidates_backup$MetricCode)
)
length(backup_candidates_code)
# 3
backup_candidates_code
# [1] "tokenDiceNormalized"      
# [2] "tokenJaccardNormalized"   
# [3] "cosineTokenNormalizedBool" 

setorderv(sample_candidates, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
sample_candidates_backup_code <- sample_candidates[sample_candidates$MetricCode %in% backup_candidates_code,]

MatthewsCorrelationCode_99_backup <- quantile(sample_candidates_backup_code$MatthewsCorrelationCode, 0.99)
MatthewsCorrelationCode_99_backup
# 99% 
# 0.9627513

sample_candidates_backup_code[sample_candidates_backup_code$MatthewsCorrelationCode>=MatthewsCorrelationCode_99_backup, c("MetricCode", "ThresholdCode", "MatthewsCorrelationCode", "Runtime")]
#                MetricCode ThresholdCode MatthewsCorrelationCode    Runtime
# 1:    tokenDiceNormalized          0.10               0.9638847 7286795340
# 2:    tokenDiceNormalized          0.08               0.9627729 7399154375
# 3:    tokenDiceNormalized          0.09               0.9627729 7472382012
# 4: tokenJaccardNormalized          0.05               0.9627729 7601886261

### check results of best metrics in sample_unclear_matching

# calculate Matthews correlation
sample_unclear_matching <- add_matthews_correlation(sample_unclear_matching)

summary(sample_unclear_matching$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3112  0.7106  0.7585  0.7402  0.7978  0.8907

summary(sample_unclear_matching$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5545  0.7349  0.7795  0.7628  0.8042  0.8671 


###############################################################################################


### third run with combined metrics ###

# samples randomly drawn from all SO posts
sample_100_1 <- fread("combined-new/PostId_VersionCount_SO_17-06_sample_100_1_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2 <- fread("combined-new/PostId_VersionCount_SO_17-06_sample_100_2_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random <- merge_samples_combined(sample_100_1, sample_100_2)
rm(sample_100_1, sample_100_2)

# samples randomly drawn from all SO posts with at least seven versions (99% quantile of version count of all posts)
sample_100_1_99 <- fread("combined-new/PostId_VersionCount_SO_17-06_sample_100_1+_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2_99 <- fread("combined-new/PostId_VersionCount_SO_17-06_sample_100_2+_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random_99 <- merge_samples_combined(sample_100_1_99, sample_100_2_99)
rm(sample_100_1_99, sample_100_2_99)

# samples randomly drawn from selected Java SO posts (tagged with <java> or <android>) with at least two versions
sample_java_100_1 <- fread("combined-new/PostId_VersionCount_SO_Java_17-06_sample_100_1_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_java_100_2 <- fread("combined-new/PostId_VersionCount_SO_Java_17-06_sample_100_2_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_java_random <- merge_samples_combined(sample_java_100_1, sample_java_100_2)
rm(sample_java_100_1, sample_java_100_2)

# sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_unclear_matching <- fread("combined-new/PostId_VersionCount_SO_17_06_sample_unclear_matching_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_unclear_matching <- filter_columns_combined(sample_unclear_matching)

# sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_multiple_possible_links <- fread("combined-new/PostId_VersionCount_SO_17-06_sample_100_multiple_possible_links_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_multiple_possible_links <- filter_columns_combined(sample_multiple_possible_links)

# merge relevant samples
sample_candidates <- merge_samples_combined(sample_random, sample_java_random)
sample_candidates <- merge_samples_combined(sample_candidates, sample_random_99)
# calculate Matthews correlation
sample_candidates <- add_matthews_correlation(sample_candidates)
sample_random <- add_matthews_correlation(sample_random)
sample_java_random <- add_matthews_correlation(sample_java_random)
sample_random_99 <- add_matthews_correlation(sample_random_99)

# order candidates
## text
setorderv(sample_candidates, c("MatthewsCorrelationText", "Runtime"), c(-1,1))
sample_candidates[c(1:3),c("MatthewsCorrelationText", "Runtime", "MetricText", "ThresholdText", "MetricTextBackup", "ThresholdTextBackup", "MetricCode", "ThresholdCode", "MetricCodeBackup", "ThresholdCodeBackup")]
#    MatthewsCorrelationText    Runtime   MetricText ThresholdText          MetricTextBackup ThresholdTextBackup          MetricCode ThresholdCode       MetricCodeBackup ThresholdCodeBackup
# 1:               0.8877625 9071289176 fiveGramDice          0.04 cosineTokenNormalizedBool                0.19 tokenDiceNormalized          0.09 tokenJaccardNormalized                0.05
# 2:               0.8877625 9072770029 fiveGramDice          0.04 cosineTokenNormalizedBool                0.19 tokenDiceNormalized          0.08    tokenDiceNormalized                0.08
# 3:               0.8877625 9077716296 fiveGramDice          0.04 cosineTokenNormalizedBool                0.19 tokenDiceNormalized          0.11    tokenDiceNormalized                0.08

## code
setorderv(sample_candidates, c("MatthewsCorrelationCode", "Runtime"), c(-1,1))
sample_candidates[c(1:3),c("MatthewsCorrelationCode", "Runtime", "MetricText", "ThresholdText", "MetricTextBackup", "ThresholdTextBackup", "MetricCode", "ThresholdCode", "MetricCodeBackup", "ThresholdCodeBackup")]
#    MatthewsCorrelationCode    Runtime   MetricText ThresholdText          MetricTextBackup ThresholdTextBackup          MetricCode ThresholdCode    MetricCodeBackup ThresholdCodeBackup
# 1:               0.9638847 8888194603 fourGramDice          0.06 cosineTokenNormalizedBool                0.16 tokenDiceNormalized           0.1 tokenDiceNormalized                0.08
# 2:               0.9638847 8892330801 fourGramDice          0.06 cosineTokenNormalizedBool                0.19 tokenDiceNormalized           0.1 tokenDiceNormalized                0.08
# 3:               0.9638847 8903428059 fourGramDice          0.05 cosineTokenNormalizedBool                0.14 tokenDiceNormalized           0.1 tokenDiceNormalized                0.08

## text and code
setorderv(sample_candidates, c("MatthewsCorrelationText", "MatthewsCorrelationCode", "Runtime"), c(-1,-1,1))
sample_candidates[c(1:3),c("MatthewsCorrelationText", "MatthewsCorrelationCode", "Runtime", "MetricText", "ThresholdText", "MetricTextBackup", "ThresholdTextBackup", "MetricCode", "ThresholdCode", "MetricCodeBackup", "ThresholdCodeBackup")]
# MatthewsCorrelationText MatthewsCorrelationCode    Runtime   MetricText ThresholdText          MetricTextBackup ThresholdTextBackup          MetricCode ThresholdCode       MetricCodeBackup ThresholdCodeBackup
# 1:               0.8877625               0.9638847 9119245953 fiveGramDice          0.04 cosineTokenNormalizedBool                0.19 tokenDiceNormalized           0.1 tokenJaccardNormalized                0.05
# 2:               0.8877625               0.9638847 9139957331 fiveGramDice          0.04 cosineTokenNormalizedBool                0.19 tokenDiceNormalized           0.1    tokenDiceNormalized                0.10
# 3:               0.8877625               0.9638847 9164390872 fiveGramDice          0.04 cosineTokenNormalizedBool                0.19 tokenDiceNormalized           0.1    tokenDiceNormalized                0.09

# OBSERVATION: Tradeoff between MatthewsCorrelationText and MatthewsCorrelationCode (because other metric is relevant for context)


# order occording to sum of MatthewsCorrelationText and MatthewsCorrelationCode
sample_candidates$MatthewsCorrelationSum <- sample_candidates$MatthewsCorrelationText + sample_candidates$MatthewsCorrelationCode
setorderv(sample_candidates, c("MatthewsCorrelationSum", "Runtime"), c(-1,1))
sample_candidates[c(1:10),c("MatthewsCorrelationText", "MatthewsCorrelationCode", "Runtime", "MetricText", "ThresholdText", "MetricTextBackup", "ThresholdTextBackup", "MetricCode", "ThresholdCode", "MetricCodeBackup", "ThresholdCodeBackup")]
#     MatthewsCorrelationText MatthewsCorrelationCode     Runtime             MetricText ThresholdText          MetricTextBackup ThresholdTextBackup          MetricCode ThresholdCode       MetricCodeBackup ThresholdCodeBackup
#  1:               0.8877625               0.9638847  9119245953           fiveGramDice          0.04 cosineTokenNormalizedBool                0.19 tokenDiceNormalized           0.1 tokenJaccardNormalized                0.05
#  2:               0.8877625               0.9638847  9139957331           fiveGramDice          0.04 cosineTokenNormalizedBool                0.19 tokenDiceNormalized           0.1    tokenDiceNormalized                0.10
#  3:               0.8877625               0.9638847  9164390872           fiveGramDice          0.04 cosineTokenNormalizedBool                0.19 tokenDiceNormalized           0.1    tokenDiceNormalized                0.09
#  4:               0.8877625               0.9638847  9369853927           fiveGramDice          0.04 cosineTokenNormalizedBool                0.19 tokenDiceNormalized           0.1    tokenDiceNormalized                0.08
#  5:               0.8877625               0.9638847 10069005939        fiveGramJaccard          0.02 cosineTokenNormalizedBool                0.19 tokenDiceNormalized           0.1    tokenDiceNormalized                0.10
#  6:               0.8877625               0.9638847 10070470728        fiveGramJaccard          0.02 cosineTokenNormalizedBool                0.19 tokenDiceNormalized           0.1 tokenJaccardNormalized                0.05
#  7:               0.8877625               0.9638847 10073689014        fiveGramJaccard          0.02 cosineTokenNormalizedBool                0.19 tokenDiceNormalized           0.1    tokenDiceNormalized                0.09
#  8:               0.8877625               0.9638847 10103416959        fiveGramJaccard          0.02 cosineTokenNormalizedBool                0.19 tokenDiceNormalized           0.1    tokenDiceNormalized                0.08
#  9:               0.8867775               0.9638847 10269247487 fourGramDiceNormalized          0.05 cosineTokenNormalizedBool                0.15 tokenDiceNormalized           0.1    tokenDiceNormalized                0.10
# 10:               0.8867775               0.9638847 10272535843 fourGramDiceNormalized          0.05 cosineTokenNormalizedBool                0.18 tokenDiceNormalized           0.1    tokenDiceNormalized                0.08

# DECISION: Choose option 2, since backup code metric will never be used. Sample quality, almost same runtime.
