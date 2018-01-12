setwd("F:/Git/github/r-scripts/metric-selection/") # Pfad bitte anpassen
#setwd("/Users/sebastian/git/github/r-scripts/metric-selection/")

# load functions
source("functions.R")

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


#### calculate Matthews correlation and select candidates using 95% quantile for text and code


### sample randomly drawn from all SO posts
sample_random <- add_matthews_correlation(sample_random)

## text
# order
setorderv(sample_random, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze
summary(sample_random$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3776  0.7232  0.8492  0.7709  0.8831  0.9365 
boxplot(sample_random$MatthewsCorrelationText)

# select candidates
MatthewsCorrelationText_95 <- quantile(sample_random$MatthewsCorrelationText, 0.95)
sample_random_text_candidates <- sample_random[sample_random$MatthewsCorrelationText >= MatthewsCorrelationText_95,]

# backup metric
backup_candidates <- sample_random[sample_random$MetricType == "EDIT" | grepl("token", sample_random$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_95_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.95)
sample_random_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup,]

## code
# order
setorderv(sample_random, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# analyze
summary(sample_random$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6518  0.8504  0.8901  0.8668  0.9109  0.9455 
boxplot(sample_random$MatthewsCorrelationCode)

# select candidates
MatthewsCorrelationCode_95 <- quantile(sample_random$MatthewsCorrelationCode, 0.95)
sample_random_code_candidates <- sample_random[sample_random$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

# backup metric
backup_candidates <- sample_random[sample_random$MetricType == "EDIT" | grepl("token", sample_random$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_95_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.95)
sample_random_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup,]


### sample randomly drawn from all SO posts with at least seven versions (99% quantile of version count of all posts)
sample_random_99 <- add_matthews_correlation(sample_random_99)

## text
# order
setorderv(sample_random_99, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze
summary(sample_random_99$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3944  0.6415  0.7496  0.7005  0.7951  0.8517 
boxplot(sample_random_99$MatthewsCorrelationText)

# select candidates
MatthewsCorrelationText_95 <- quantile(sample_random_99$MatthewsCorrelationText, 0.95)
sample_random_99_text_candidates <- sample_random_99[sample_random_99$MatthewsCorrelationText >= MatthewsCorrelationText_95,]

# backup metric
backup_candidates <- sample_random_99[sample_random_99$MetricType == "EDIT" | grepl("token", sample_random_99$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_95_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.95)
sample_random_99_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup,]

## code
# order
setorderv(sample_random_99, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# analyze
summary(sample_random_99$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5114  0.7692  0.8539  0.8066  0.8897  0.9289
boxplot(sample_random_99$MatthewsCorrelationCode)

# select candidates
MatthewsCorrelationCode_95 <- quantile(sample_random_99$MatthewsCorrelationCode, 0.95)
sample_random_99_code_candidates <- sample_random_99[sample_random_99$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

# backup metric
backup_candidates <- sample_random_99[sample_random_99$MetricType == "EDIT" | grepl("token", sample_random_99$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_95_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.95)
sample_random_99_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup,]


### sample randomly drawn from all Java SO posts (tagged with <java> or <android>) with at least two versions
sample_java_random <- add_matthews_correlation(sample_java_random)

## text
# order
setorderv(sample_java_random, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze
summary(sample_java_random$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3744  0.6646  0.7468  0.7030  0.7839  0.8417 
boxplot(sample_java_random$MatthewsCorrelationText)

# select candidates
MatthewsCorrelationText_95 <- quantile(sample_java_random$MatthewsCorrelationText, 0.95)
sample_java_random_text_candidates <- sample_java_random[sample_java_random$MatthewsCorrelationText >= MatthewsCorrelationText_95,]

# backup metric
backup_candidates <- sample_java_random[sample_java_random$MetricType == "EDIT" | grepl("token", sample_java_random$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_95_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.95)
sample_java_random_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup,]

## code
# order
setorderv(sample_java_random, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# analyze
summary(sample_java_random$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6312  0.8272  0.8653  0.8422  0.8893  0.9273 
boxplot(sample_java_random$MatthewsCorrelationCode)

# select candidates
MatthewsCorrelationCode_95 <- quantile(sample_java_random$MatthewsCorrelationCode, 0.95)
sample_java_random_code_candidates <- sample_java_random[sample_java_random$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

# backup metric
backup_candidates <- sample_java_random[sample_java_random$MetricType == "EDIT" | grepl("token", sample_java_random$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_95_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.95)
sample_java_random_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup,]


### sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_unclear_matching <- add_matthews_correlation(sample_unclear_matching)

## text
# order
setorderv(sample_unclear_matching, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze
summary(sample_unclear_matching$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2432  0.6101  0.7141  0.6830  0.7731  0.8508
boxplot(sample_unclear_matching$MatthewsCorrelationText)

# select candidates
MatthewsCorrelationText_95 <- quantile(sample_unclear_matching$MatthewsCorrelationText, 0.95)
sample_unclear_matching_text_candidates <- sample_unclear_matching[sample_unclear_matching$MatthewsCorrelationText >= MatthewsCorrelationText_95,]

# backup metric
backup_candidates <- sample_unclear_matching[sample_unclear_matching$MetricType == "EDIT" | grepl("token", sample_unclear_matching$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_95_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.95)
sample_unclear_matching_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup,]


## code
# order
setorderv(sample_unclear_matching, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# analyze
summary(sample_unclear_matching$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2559  0.6856  0.7460  0.7312  0.7884  0.8706
boxplot(sample_unclear_matching$MatthewsCorrelationCode)

# select candidates
MatthewsCorrelationCode_95 <- quantile(sample_unclear_matching$MatthewsCorrelationCode, 0.95)
sample_unclear_matching_code_candidates <- sample_unclear_matching[sample_unclear_matching$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

# backup metric
backup_candidates <- sample_unclear_matching[sample_unclear_matching$MetricType == "EDIT" | grepl("token", sample_unclear_matching$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_95_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.95)
sample_unclear_matching_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup,]



### sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_multiple_possible_links <- add_matthews_correlation(sample_multiple_possible_links)

## text
# order
setorderv(sample_multiple_possible_links, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze
summary(sample_multiple_possible_links$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5982  0.7623  0.8537  0.8159  0.8883  0.9423 
boxplot(sample_multiple_possible_links$MatthewsCorrelationText)

# select candidates
MatthewsCorrelationText_95 <- quantile(sample_multiple_possible_links$MatthewsCorrelationText, 0.95)
sample_multiple_possible_links_text_candidates <- sample_multiple_possible_links[sample_multiple_possible_links$MatthewsCorrelationText >= MatthewsCorrelationText_95,]

# backup metric
backup_candidates <- sample_multiple_possible_links[sample_multiple_possible_links$MetricType == "EDIT" | grepl("token", sample_multiple_possible_links$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_95_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.95)
sample_multiple_possible_links_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup,]


## code
# order
setorderv(sample_multiple_possible_links, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# analyze
summary(sample_multiple_possible_links$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.7600  0.8802  0.9041  0.8917  0.9229  0.9602 
boxplot(sample_multiple_possible_links$MatthewsCorrelationCode)

# select candidates
MatthewsCorrelationCode_95 <- quantile(sample_multiple_possible_links$MatthewsCorrelationCode, 0.95)
sample_multiple_possible_links_code_candidates <- sample_multiple_possible_links[sample_multiple_possible_links$MatthewsCorrelationCode >= MatthewsCorrelationCode_95,]

# backup metric
backup_candidates <- sample_multiple_possible_links[sample_multiple_possible_links$MetricType == "EDIT" | grepl("token", sample_multiple_possible_links$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_95_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.95)
sample_multiple_possible_links_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup,]



# DECISION: For second run, select metrics that are in 95% quantile of sample_random, sample_java_random, and sample_random_99 for text or code

## best metrics

# text
candidates_text <- intersect(
  intersect(unique(sample_random_text_candidates$Metric), unique(sample_java_random_text_candidates$Metric)),
  unique(sample_random_99_text_candidates$Metric)
)
length(candidates_text)
# 19


# code
candidates_code <- intersect(
  intersect(unique(sample_random_code_candidates$Metric), unique(sample_java_random_code_candidates$Metric)),
  unique(sample_random_99_code_candidates$Metric)
)
length(candidates_code)
# 10

# both
candidates <- unique(c(candidates_text, candidates_code))
length(candidates)
# 27
candidates
# [1] "twoGramJaccardNormalizedPadding"                 
# [2] "winnowingThreeGramDiceNormalized"                
# [3] "cosineTwoGramNormalizedBool"                     
# [4] "threeGramDice"                                   
# [5] "twoGramDiceNormalized"                           
# [6] "twoGramDiceNormalizedPadding"                    
# [7] "manhattanThreeGramNormalized"                    
# [8] "threeGramJaccard"                                
# [9] "winnowingFourGramDice"                           
# [10] "fourGramJaccard"                                 
# [11] "fiveGramDice"                                    
# [12] "winnowingThreeGramLongestCommonSubsequence"      
# [13] "fourGramDice"                                    
# [14] "threeGramJaccardNormalized"                      
# [15] "cosineThreeGramNormalizedBool"                   
# [16] "cosineThreeGramNormalizedNormalizedTermFrequency"
# [17] "cosineFourGramNormalizedNormalizedTermFrequency" 
# [18] "manhattanFiveGramNormalized"                     
# [19] "manhattanFourGramNormalized"                     
# [20] "winnowingTwoGramDiceNormalized"                  
# [21] "winnowingFiveGramDiceNormalized"                 
# [22] "winnowingFourGramDiceNormalized"                 
# [23] "threeGramDiceNormalized"                         
# [24] "threeGramDiceNormalizedPadding"                  
# [25] "fourGramDiceNormalizedPadding"                   
# [26] "threeGramJaccardNormalizedPadding"               
# [27] "fourGramJaccardNormalizedPadding" 

## backup metrics

# text
candidates_backup_text <- intersect(
  intersect(unique(sample_random_text_candidates_backup$Metric), unique(sample_java_random_text_candidates_backup$Metric)),
  unique(sample_random_99_text_candidates_backup$Metric)
)
length(candidates_backup_text)
# 3

# code
candidates_backup_code <- intersect(
  intersect(unique(sample_random_code_candidates_backup$Metric), unique(sample_java_random_code_candidates_backup$Metric)),
  unique(sample_random_99_code_candidates_backup$Metric)
)
length(candidates_backup_code)
# 1

# both
candidates_backup <- unique(c(candidates_backup_text, candidates_backup_code))
length(candidates_backup)
# 4
candidates_backup
# [1] "cosineTokenNormalizedTermFrequency"          
# [2] "cosineTokenNormalizedNormalizedTermFrequency"
# [3] "tokenJaccardNormalized"                      
# [4] "tokenDice" 


## best and backup metrics
candidates <- unique(c(candidates, candidates_backup))
length(candidates)
# 31
candidates
# [1] "twoGramJaccardNormalizedPadding"                 
# [2] "winnowingThreeGramDiceNormalized"                
# [3] "cosineTwoGramNormalizedBool"                     
# [4] "threeGramDice"                                   
# [5] "twoGramDiceNormalized"                           
# [6] "twoGramDiceNormalizedPadding"                    
# [7] "manhattanThreeGramNormalized"                    
# [8] "threeGramJaccard"                                
# [9] "winnowingFourGramDice"                           
# [10] "fourGramJaccard"                                 
# [11] "fiveGramDice"                                    
# [12] "winnowingThreeGramLongestCommonSubsequence"      
# [13] "fourGramDice"                                    
# [14] "threeGramJaccardNormalized"                      
# [15] "cosineThreeGramNormalizedBool"                   
# [16] "cosineThreeGramNormalizedNormalizedTermFrequency"
# [17] "cosineFourGramNormalizedNormalizedTermFrequency" 
# [18] "manhattanFiveGramNormalized"                     
# [19] "manhattanFourGramNormalized"                     
# [20] "winnowingTwoGramDiceNormalized"                  
# [21] "winnowingFiveGramDiceNormalized"                 
# [22] "winnowingFourGramDiceNormalized"                 
# [23] "threeGramDiceNormalized"                         
# [24] "threeGramDiceNormalizedPadding"                  
# [25] "fourGramDiceNormalizedPadding"                   
# [26] "threeGramJaccardNormalizedPadding"               
# [27] "fourGramJaccardNormalizedPadding"                
# [28] "cosineTokenNormalizedTermFrequency"              
# [29] "cosineTokenNormalizedNormalizedTermFrequency"    
# [30] "tokenJaccardNormalized"                          
# [31] "tokenDice" 



### second run with selected metrics ###

library(data.table)

# samples randomly drawn from all SO posts
sample_100_1 <- fread("selected/PostId_VersionCount_SO_17-06_sample_100_1_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2 <- fread("selected/PostId_VersionCount_SO_17-06_sample_100_2_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random <- merge_samples(sample_100_1, sample_100_2)
rm(sample_100_1, sample_100_2)

# samples randomly drawn from all SO posts with at least seven versions (99% quantile of version count of all posts)
sample_100_1_99 <- fread("selected/PostId_VersionCount_SO_17-06_sample_100_1+_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2_99 <- fread("selected/PostId_VersionCount_SO_17-06_sample_100_2+_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random_99 <- merge_samples(sample_100_1_99, sample_100_2_99)
rm(sample_100_1_99, sample_100_2_99)

# samples randomly drawn from selected Java SO posts (tagged with <java> or <android>) with at least two versions
sample_java_100_1 <- fread("selected/PostId_VersionCount_SO_Java_17-06_sample_100_1_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_java_100_2 <- fread("selected/PostId_VersionCount_SO_Java_17-06_sample_100_2_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_java_random <- merge_samples(sample_java_100_1, sample_java_100_2)
rm(sample_java_100_1, sample_java_100_2)

# sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_unclear_matching <- fread("selected/PostId_VersionCount_SO_17_06_sample_unclear_matching_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_unclear_matching <- filter_columns(sample_unclear_matching)

# sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_multiple_possible_links <- fread("selected/PostId_VersionCount_SO_17-06_sample_100_multiple_possible_links_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_multiple_possible_links <- filter_columns(sample_multiple_possible_links)


# DECISION: Select metrics that are in 95% quantile of sample_random, sample_java_random, and sample_random_99 for text/code

# merge relevant samples
sample_candidates <- merge_samples(sample_random, sample_java_random)
sample_candidates <- merge_samples(sample_candidates, sample_random_99)
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
backup_candidates <- sample_random[sample_random$MetricType == "EDIT" | grepl("token", sample_random$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_95_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.95)
sample_random_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup,]

backup_candidates <- sample_java_random[sample_java_random$MetricType == "EDIT" | grepl("token", sample_java_random$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_95_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.95)
sample_java_random_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup,]

backup_candidates <- sample_random_99[sample_random_99$MetricType == "EDIT" | grepl("token", sample_random_99$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_95_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.95)
sample_random_99_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_95_backup,]


# final candidates
candidates_text <- intersect(
  intersect(unique(sample_random_text_candidates$Metric), unique(sample_java_random_text_candidates$Metric)),
  unique(sample_random_99_text_candidates$Metric)
)
length(candidates_text)
# 12
candidates_text
# [1] "cosineTwoGramNormalizedBool"                     
# [2] "winnowingThreeGramDiceNormalized"                
# [3] "manhattanThreeGramNormalized"                    
# [4] "threeGramDice"                                   
# [5] "threeGramJaccard"                                
# [6] "fourGramDice"                                    
# [7] "cosineThreeGramNormalizedNormalizedTermFrequency"
# [8] "manhattanFiveGramNormalized"                     
# [9] "fiveGramDice"                                    
# [10] "manhattanFourGramNormalized"                     
# [11] "fourGramJaccard"                                 
# [12] "winnowingThreeGramLongestCommonSubsequence"      

summary(sample_candidates$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4058  0.6808  0.7874  0.7344  0.8207  0.8572 

setorderv(sample_candidates, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))
sample_candidates_text <- sample_candidates[sample_candidates$Metric %in% candidates_text,]

MatthewsCorrelationText_99 <- quantile(sample_candidates_text$MatthewsCorrelationText, 0.99)
MatthewsCorrelationText_99
# 99% 
# 0.8516912 

sample_candidates_text[sample_candidates_text$MatthewsCorrelationText>=MatthewsCorrelationText_99, c("Metric", "Threshold", "MatthewsCorrelationText", "Runtime")]
#                          Metric Threshold MatthewsCorrelationText   Runtime
# 1:  manhattanFourGramNormalized      0.17               0.8572117 343267912
# 2:                threeGramDice      0.30               0.8542778 194886037
# 3: manhattanThreeGramNormalized      0.21               0.8540757 316846492
# 4:  manhattanFourGramNormalized      0.16               0.8538118 345013616
# 5:                 fourGramDice      0.23               0.8528021 181355519
# 6:              fourGramJaccard      0.13               0.8528021 231721246
# 7: manhattanThreeGramNormalized      0.20               0.8528021 322299635
# 8: manhattanThreeGramNormalized      0.23               0.8525419 315863392
# 9: manhattanThreeGramNormalized      0.22               0.8525419 317249184
# 10:                 fourGramDice      0.22               0.8524111 181296507
# 11:  manhattanFourGramNormalized      0.18               0.8519267 343582326
# 12: manhattanThreeGramNormalized      0.24               0.8518902 316709954
# 13: manhattanThreeGramNormalized      0.25               0.8516930 317101257


# backup metric
backup_candidates_text <- intersect(
  intersect(unique(sample_random_text_candidates_backup$Metric), unique(sample_java_random_text_candidates_backup$Metric)),
  unique(sample_random_99_text_candidates_backup$Metric)
)
length(backup_candidates_text)
# 2
backup_candidates_text
# [1] "tokenJaccardNormalized"            
# [2] "cosineTokenNormalizedTermFrequency"

setorderv(sample_candidates, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))
sample_candidates_backup_text <- sample_candidates[sample_candidates$Metric %in% backup_candidates_text,]

MatthewsCorrelationText_99_backup <- quantile(sample_candidates_backup_text$MatthewsCorrelationText, 0.99)
MatthewsCorrelationText_99_backup
# 99% 
# 0.8388076 

sample_candidates_backup_text[sample_candidates_backup_text$MatthewsCorrelationText>=MatthewsCorrelationText_99_backup, c("Metric", "Threshold", "MatthewsCorrelationText", "Runtime")]
#                                 Metric Threshold MatthewsCorrelationText   Runtime
# 1: cosineTokenNormalizedTermFrequency      0.37               0.8390520 124949300
# 2:             tokenJaccardNormalized      0.19               0.8389551 101857303
# 3: cosineTokenNormalizedTermFrequency      0.36               0.8388111 124984235


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
backup_candidates <- sample_random[sample_random$MetricType == "EDIT" | grepl("token", sample_random$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_95_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.95)
sample_random_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup,]

backup_candidates <- sample_java_random[sample_java_random$MetricType == "EDIT" | grepl("token", sample_java_random$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_95_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.95)
sample_java_random_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup,]

backup_candidates <- sample_random_99[sample_random_99$MetricType == "EDIT" | grepl("token", sample_random_99$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_95_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.95)
sample_random_99_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_95_backup,]


# final candidates
candidates_code <- intersect(
  intersect(unique(sample_random_code_candidates$Metric), unique(sample_java_random_code_candidates$Metric)),
  unique(sample_random_99_code_candidates$Metric)
)
length(candidates_code)
# 14
candidates_code
# [1] "cosineThreeGramNormalizedNormalizedTermFrequency"
# [2] "winnowingFourGramDiceNormalized"                 
# [3] "cosineThreeGramNormalizedBool"                   
# [4] "threeGramDiceNormalized"                         
# [5] "threeGramDiceNormalizedPadding"                  
# [6] "fourGramDiceNormalizedPadding"                   
# [7] "threeGramJaccardNormalized"                      
# [8] "threeGramJaccardNormalizedPadding"               
# [9] "fourGramJaccardNormalizedPadding"                
# [10] "manhattanThreeGramNormalized"                    
# [11] "cosineFourGramNormalizedNormalizedTermFrequency" 
# [12] "threeGramDice"                                   
# [13] "fourGramDice"                                    
# [14] "fourGramJaccard" 

summary(sample_candidates$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5520  0.8172  0.8799  0.8409  0.9006  0.9209 

setorderv(sample_candidates, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
sample_candidates_code <- sample_candidates[sample_candidates$Metric %in% candidates_code,]

MatthewsCorrelationCode_99 <- quantile(sample_candidates_code$MatthewsCorrelationCode, 0.99)
MatthewsCorrelationCode_99
# 99% 
# 0.9187565

sample_candidates_code[sample_candidates_code$MatthewsCorrelationCode>=MatthewsCorrelationCode_99, c("Metric", "Threshold", "MatthewsCorrelationCode", "Runtime")]
#                              Metric Threshold MatthewsCorrelationCode   Runtime
# 1:   winnowingFourGramDiceNormalized      0.23               0.9208735 150771010
# 2:   winnowingFourGramDiceNormalized      0.25               0.9199509 151886364
# 3:   winnowingFourGramDiceNormalized      0.20               0.9196971 150684642
# 4:     fourGramDiceNormalizedPadding      0.31               0.9196971 218302293
# 5:     fourGramDiceNormalizedPadding      0.30               0.9195764 218382888
# 6:     fourGramDiceNormalizedPadding      0.28               0.9195764 218662133
# 7:     fourGramDiceNormalizedPadding      0.27               0.9195764 219420975
# 8:     fourGramDiceNormalizedPadding      0.26               0.9195764 219593381
# 9:     fourGramDiceNormalizedPadding      0.29               0.9195764 219882515
# 10:  fourGramJaccardNormalizedPadding      0.15               0.9195764 239930286
# 11:  fourGramJaccardNormalizedPadding      0.17               0.9195764 239944890
# 12:  fourGramJaccardNormalizedPadding      0.16               0.9195764 240105827
# 13: threeGramJaccardNormalizedPadding      0.20               0.9194600 228619576
# 14:   winnowingFourGramDiceNormalized      0.26               0.9190431 150800406
# 15:   winnowingFourGramDiceNormalized      0.24               0.9187732 150614417


# backup metric
backup_candidates_code <- intersect(
  intersect(unique(sample_random_code_candidates_backup$Metric), unique(sample_java_random_code_candidates_backup$Metric)),
  unique(sample_random_99_code_candidates_backup$Metric)
)
length(backup_candidates_code)
# 1
backup_candidates_code
# [1] "cosineTokenNormalizedNormalizedTermFrequency"   

setorderv(sample_candidates, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
sample_candidates_backup_code <- sample_candidates[sample_candidates$Metric %in% backup_candidates_code,]

MatthewsCorrelationCode_99_backup <- quantile(sample_candidates_backup_code$MatthewsCorrelationCode, 0.99)
MatthewsCorrelationCode_99_backup
# 99% 
# 0.910913

sample_candidates_backup_code[sample_candidates_backup_code$MatthewsCorrelationCode>=MatthewsCorrelationCode_99_backup, c("Metric", "Threshold", "MatthewsCorrelationCode", "Runtime")]
#                                           Metric Threshold MatthewsCorrelationCode   Runtime
# 1: cosineTokenNormalizedNormalizedTermFrequency      0.26               0.9118216 123140086
# 2: cosineTokenNormalizedNormalizedTermFrequency      0.27               0.9109130 125341607


### check results of best metrics in sample_unclear_matching

# calculate Matthews correlation
sample_unclear_matching <- add_matthews_correlation(sample_unclear_matching)

summary(sample_unclear_matching$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2432  0.6865  0.7487  0.7232  0.7837  0.8508 

summary(sample_unclear_matching$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2559  0.7177  0.7682  0.7568  0.8027  0.8723 


### third run with combined metrics ###

library(data.table)

# samples randomly drawn from all SO posts
sample_100_1 <- fread("combined/PostId_VersionCount_SO_17-06_sample_100_1_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2 <- fread("combined/PostId_VersionCount_SO_17-06_sample_100_2_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random <- merge_samples_combined(sample_100_1, sample_100_2)
rm(sample_100_1, sample_100_2)

# samples randomly drawn from all SO posts with at least seven versions (99% quantile of version count of all posts)
sample_100_1_99 <- fread("combined/PostId_VersionCount_SO_17-06_sample_100_1+_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2_99 <- fread("combined/PostId_VersionCount_SO_17-06_sample_100_2+_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random_99 <- merge_samples_combined(sample_100_1_99, sample_100_2_99)
rm(sample_100_1_99, sample_100_2_99)

# samples randomly drawn from selected Java SO posts (tagged with <java> or <android>) with at least two versions
sample_java_100_1 <- fread("combined/PostId_VersionCount_SO_Java_17-06_sample_100_1_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_java_100_2 <- fread("combined/PostId_VersionCount_SO_Java_17-06_sample_100_2_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_java_random <- merge_samples_combined(sample_java_100_1, sample_java_100_2)
rm(sample_java_100_1, sample_java_100_2)

# sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_unclear_matching <- fread("combined/PostId_VersionCount_SO_17_06_sample_unclear_matching_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_unclear_matching <- filter_columns_combined(sample_unclear_matching)

# sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_multiple_possible_links <- fread("combined/PostId_VersionCount_SO_17-06_sample_100_multiple_possible_links_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
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
#    MatthewsCorrelationText   Runtime
# 1:               0.8627442 682864383
# 2:               0.8627442 683195699
# 3:               0.8627442 684103796
#                     MetricText ThresholdText                   MetricTextBackup ThresholdTextBackup
# 1: manhattanFourGramNormalized          0.17 cosineTokenNormalizedTermFrequency                0.36
# 2: manhattanFourGramNormalized          0.17 cosineTokenNormalizedTermFrequency                0.37
# 3: manhattanFourGramNormalized          0.17 cosineTokenNormalizedTermFrequency                0.37
#                         MetricCode ThresholdCode                             MetricCodeBackup ThresholdCodeBackup
#                         MetricCode ThresholdCode                             MetricCodeBackup ThresholdCodeBackup
# 1: winnowingFourGramDiceNormalized          0.23 cosineTokenNormalizedNormalizedTermFrequency                0.26
# 2: winnowingFourGramDiceNormalized          0.24 cosineTokenNormalizedNormalizedTermFrequency                0.27
# 3: winnowingFourGramDiceNormalized          0.20 cosineTokenNormalizedNormalizedTermFrequency                0.27

## code
setorderv(sample_candidates, c("MatthewsCorrelationCode", "Runtime"), c(-1,1))
sample_candidates[c(1:3),c("MatthewsCorrelationCode", "Runtime", "MetricText", "ThresholdText", "MetricTextBackup", "ThresholdTextBackup", "MetricCode", "ThresholdCode", "MetricCodeBackup", "ThresholdCodeBackup")]
#    MatthewsCorrelationCode   Runtime
# 1:                0.921928 653977566
# 2:                0.921928 654250071
# 3:                0.921928 654367727
#                      MetricText ThresholdText                   MetricTextBackup ThresholdTextBackup
# 1: manhattanThreeGramNormalized          0.22 cosineTokenNormalizedTermFrequency                0.36
# 2: manhattanThreeGramNormalized          0.21 cosineTokenNormalizedTermFrequency                0.37
# 3: manhattanThreeGramNormalized          0.22 cosineTokenNormalizedTermFrequency                0.36
#                         MetricCode ThresholdCode                             MetricCodeBackup ThresholdCodeBackup
# 1: winnowingFourGramDiceNormalized           0.2 cosineTokenNormalizedNormalizedTermFrequency                0.27
# 2: winnowingFourGramDiceNormalized           0.2 cosineTokenNormalizedNormalizedTermFrequency                0.26
# 3: winnowingFourGramDiceNormalized           0.2 cosineTokenNormalizedNormalizedTermFrequency                0.26

## text and code
setorderv(sample_candidates, c("MatthewsCorrelationText", "MatthewsCorrelationCode", "Runtime"), c(-1,-1,1))
sample_candidates[c(1:3),c("MatthewsCorrelationText", "MatthewsCorrelationCode", "Runtime", "MetricText", "ThresholdText", "MetricTextBackup", "ThresholdTextBackup", "MetricCode", "ThresholdCode", "MetricCodeBackup", "ThresholdCodeBackup")]
#    MatthewsCorrelationText MatthewsCorrelationCode   Runtime     
# 1:               0.8627442               0.9208735 682864383
# 2:               0.8627442               0.9208735 690211171
# 3:               0.8627442               0.9208735 690465493
#                     MetricText ThresholdText                   MetricTextBackup ThresholdTextBackup 
# 1: manhattanFourGramNormalized          0.17 cosineTokenNormalizedTermFrequency                0.36
# 2: manhattanFourGramNormalized          0.17 cosineTokenNormalizedTermFrequency                0.36
# 3: manhattanFourGramNormalized          0.17 cosineTokenNormalizedTermFrequency                0.37
#                         MetricCode ThresholdCode                             MetricCodeBackup ThresholdCodeBackup
# 1: winnowingFourGramDiceNormalized          0.23 cosineTokenNormalizedNormalizedTermFrequency                0.26
# 2: winnowingFourGramDiceNormalized          0.23 cosineTokenNormalizedNormalizedTermFrequency                0.27
# 3: winnowingFourGramDiceNormalized          0.23 cosineTokenNormalizedNormalizedTermFrequency                0.27

# OBSERVATION: Tradeoff between MatthewsCorrelationText and MatthewsCorrelationCode (because other metric is relevant for context)

# order occording to sum of MatthewsCorrelationText and MatthewsCorrelationCode
sample_candidates$MatthewsCorrelationSum <- sample_candidates$MatthewsCorrelationText + sample_candidates$MatthewsCorrelationCode
setorderv(sample_candidates, c("MatthewsCorrelationSum", "Runtime"), c(-1,1))
sample_candidates[c(1:3),c("MatthewsCorrelationText", "MatthewsCorrelationCode", "Runtime", "MetricText", "ThresholdText", "MetricTextBackup", "ThresholdTextBackup", "MetricCode", "ThresholdCode", "MetricCodeBackup", "ThresholdCodeBackup")]
#    MatthewsCorrelationText MatthewsCorrelationCode   Runtime
# 1:               0.8627442               0.9208735 682864383
# 2:               0.8627442               0.9208735 690211171
# 3:               0.8627442               0.9208735 690465493
#                     MetricText ThresholdText                   MetricTextBackup ThresholdTextBackup
# 1: manhattanFourGramNormalized          0.17 cosineTokenNormalizedTermFrequency                0.36
# 2: manhattanFourGramNormalized          0.17 cosineTokenNormalizedTermFrequency                0.36
# 3: manhattanFourGramNormalized          0.17 cosineTokenNormalizedTermFrequency                0.37
#                         MetricCode ThresholdCode                             MetricCodeBackup ThresholdCodeBackup
# 1: winnowingFourGramDiceNormalized          0.23 cosineTokenNormalizedNormalizedTermFrequency                0.26
# 2: winnowingFourGramDiceNormalized          0.23 cosineTokenNormalizedNormalizedTermFrequency                0.27
# 3: winnowingFourGramDiceNormalized          0.23 cosineTokenNormalizedNormalizedTermFrequency                0.27


### fourth run with default metric ###

library(data.table)

# samples randomly drawn from all SO posts
sample_100_1 <- fread("default/PostId_VersionCount_SO_17-06_sample_100_1_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2 <- fread("default/PostId_VersionCount_SO_17-06_sample_100_2_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random <- merge_samples(sample_100_1, sample_100_2)
rm(sample_100_1, sample_100_2)

# samples randomly drawn from all SO posts with at least seven versions (99% quantile of version count of all posts)
sample_100_1_99 <- fread("default/PostId_VersionCount_SO_17-06_sample_100_1+_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_100_2_99 <- fread("default/PostId_VersionCount_SO_17-06_sample_100_2+_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_random_99 <- merge_samples(sample_100_1_99, sample_100_2_99)
rm(sample_100_1_99, sample_100_2_99)

# samples randomly drawn from selected Java SO posts (tagged with <java> or <android>) with at least two versions
sample_java_100_1 <- fread("default/PostId_VersionCount_SO_Java_17-06_sample_100_1_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_java_100_2 <- fread("default/PostId_VersionCount_SO_Java_17-06_sample_100_2_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_java_random <- merge_samples(sample_java_100_1, sample_java_100_2)
rm(sample_java_100_1, sample_java_100_2)

# sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_unclear_matching <- fread("default/PostId_VersionCount_SO_17_06_sample_unclear_matching_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_unclear_matching <- filter_columns(sample_unclear_matching)

# sample in which we moved posts with unclear matching according to the comments added in the GT App
sample_multiple_possible_links <- fread("default/PostId_VersionCount_SO_17-06_sample_100_multiple_possible_links_per_sample.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
sample_multiple_possible_links <- filter_columns(sample_multiple_possible_links)


# merge relevant samples
sample_candidates <- merge_samples(sample_random, sample_java_random)
sample_candidates <- merge_samples(sample_candidates, sample_random_99)
# calculate Matthews correlation
sample_candidates <- add_matthews_correlation(sample_candidates)
sample_random <- add_matthews_correlation(sample_random)
sample_java_random <- add_matthews_correlation(sample_java_random)
sample_random_99 <- add_matthews_correlation(sample_random_99)

sample_candidates[,c("Metric", "Threshold", "MatthewsCorrelationText", "MatthewsCorrelationCode", "Runtime")]
#     Metric Threshold MatthewsCorrelationText MatthewsCorrelationCode    Runtime
# 1: default         0               0.8627442               0.9208735 1510873600
