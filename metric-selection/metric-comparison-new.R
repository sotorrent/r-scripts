# set working directory (see https://stackoverflow.com/a/35842119)
dir = tryCatch({
  # script being sourced
  getSrcDirectory()[1]
}, error = function(e) {
  # script being run in RStudio
  dirname(rstudioapi::getActiveDocumentContext()$path)
})
setwd(dir)

# load functions
source("functions.R")


# read results of first run with all metrics
library(data.table)

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

## text
# order
setorderv(sample_random, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze
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
sample_random_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup,]

## code
# order
setorderv(sample_random, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# analyze
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
sample_random_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup,]


### sample randomly drawn from all SO posts with at least seven versions (99% quantile of version count of all posts)
sample_random_99 <- add_matthews_correlation(sample_random_99)

## text
# order
setorderv(sample_random_99, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze
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
sample_random_99_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup,]

## code
# order
setorderv(sample_random_99, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# analyze
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
sample_random_99_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup,]


### sample randomly drawn from all Java SO posts (tagged with <java> or <android>) with at least two versions
sample_java_random <- add_matthews_correlation(sample_java_random)

## text
# order
setorderv(sample_java_random, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze
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
sample_java_random_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup,]

## code
# order
setorderv(sample_java_random, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# analyze
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
sample_java_random_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup,]


### sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_unclear_matching <- add_matthews_correlation(sample_unclear_matching)

## text
# order
setorderv(sample_unclear_matching, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze
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
sample_unclear_matching_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup,]


## code
# order
setorderv(sample_unclear_matching, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# analyze
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
sample_unclear_matching_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup,]



### sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_multiple_possible_links <- add_matthews_correlation(sample_multiple_possible_links)

## text
# order
setorderv(sample_multiple_possible_links, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze
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
sample_multiple_possible_links_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup,]


## code
# order
setorderv(sample_multiple_possible_links, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# analyze
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
sample_multiple_possible_links_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup,]


# DECISION: For second run, select metrics that are in 90% quantile of sample_random, sample_java_random, and sample_random_99 for text or code

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
# 8

# both
candidates_backup <- unique(c(candidates_backup_text, candidates_backup_code))
length(candidates_backup)
# 10
candidates_backup
# [1] "tokenDice"                                   
# [2] "cosineTokenNormalizedNormalizedTermFrequency"
# [3] "tokenJaccard"                                
# [4] "cosineTokenNormalizedBool"                   
# [5] "tokenDiceNormalized"                         
# [6] "tokenJaccardNormalized"                      
# [7] "manhattanTokenNormalized"                    
# [8] "levenshtein"                                 
# [9] "optimalAlignment"                            
# [10] "damerauLevenshtein"  
