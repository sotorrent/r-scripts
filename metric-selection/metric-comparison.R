#setwd("F:/Git/github/r-scripts/metric-selection/comparison-by-sample") # Pfad bitte anpassen
setwd("/Users/sebastian/git/github/r-scripts/metric-selection/comparison-by-sample")

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
# 0.6518  0.8520  0.8903  0.8670  0.9109  0.9455 
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
# 0.3944  0.6415  0.7495  0.7002  0.7948  0.8517 
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
# 0.5114  0.7724  0.8539  0.8066  0.8897  0.9289 
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
# 0.3744  0.6646  0.7461  0.7028  0.7832  0.8417 
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
# 0.6312  0.8272  0.8653  0.8424  0.8893  0.9273 
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
# 0.2432  0.6101  0.7132  0.6830  0.7731  0.8508 
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
# 0.2559  0.6856  0.7453  0.7309  0.7884  0.8706 
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
# 0.5982  0.7623  0.8537  0.8160  0.8887  0.9423 
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
# 0.7600  0.8802  0.9041  0.8915  0.9229  0.9602 
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
# 9

# both
candidates <- unique(c(candidates_text, candidates_code))
length(candidates)
# 26
candidates
# [1] "twoGramJaccardNormalizedPadding"                 
# [2] "winnowingThreeGramDiceNormalized"                
# [3] "cosineTwoGramNormalizedBool"                     
# [4] "threeGramDice"                                   
# [5] "twoGramDiceNormalized"                           
# [6] "manhattanThreeGramNormalized"                    
# [7] "threeGramJaccard"                                
# [8] "winnowingFourGramDice"                           
# [9] "fiveGramDice"                                    
# [10] "fourGramJaccard"                                 
# [11] "winnowingThreeGramLongestCommonSubsequence"      
# [12] "fourGramDice"                                    
# [13] "threeGramJaccardNormalized"                      
# [14] "tokenJaccardNormalized"                          
# [15] "cosineThreeGramNormalizedBool"                   
# [16] "cosineThreeGramNormalizedNormalizedTermFrequency"
# [17] "cosineFourGramNormalizedNormalizedTermFrequency" 
# [18] "manhattanFiveGramNormalized"                     
# [19] "manhattanFourGramNormalized"                     
# [20] "winnowingFiveGramDiceNormalized"                 
# [21] "winnowingFourGramDiceNormalized"                 
# [22] "threeGramDiceNormalized"                         
# [23] "threeGramDiceNormalizedPadding"                  
# [24] "fourGramDiceNormalizedPadding"                   
# [25] "threeGramJaccardNormalizedPadding"               
# [26] "fourGramJaccardNormalizedPadding"  


## backup metrics

# text
candidates_backup_text <- intersect(
  intersect(unique(sample_random_text_candidates_backup$Metric), unique(sample_java_random_text_candidates_backup$Metric)),
  unique(sample_random_99_text_candidates_backup$Metric)
)
length(candidates_backup_text)
# 2

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
# 3
candidates_backup
# [1] "tokenJaccardNormalized"
# [2] "tokenDiceNormalized"   
# [3] "tokenDice"  


## best and backup metrics
candidates <- unique(c(candidates, candidates_backup))
length(candidates)
# 28
candidates
# [1] "twoGramJaccardNormalizedPadding"                 
# [2] "winnowingThreeGramDiceNormalized"                
# [3] "cosineTwoGramNormalizedBool"                     
# [4] "threeGramDice"                                   
# [5] "twoGramDiceNormalized"                           
# [6] "manhattanThreeGramNormalized"                    
# [7] "threeGramJaccard"                                
# [8] "winnowingFourGramDice"                           
# [9] "fiveGramDice"                                    
# [10] "fourGramJaccard"                                 
# [11] "winnowingThreeGramLongestCommonSubsequence"      
# [12] "fourGramDice"                                    
# [13] "threeGramJaccardNormalized"                      
# [14] "tokenJaccardNormalized"                          
# [15] "cosineThreeGramNormalizedBool"                   
# [16] "cosineThreeGramNormalizedNormalizedTermFrequency"
# [17] "cosineFourGramNormalizedNormalizedTermFrequency" 
# [18] "manhattanFiveGramNormalized"                     
# [19] "manhattanFourGramNormalized"                     
# [20] "winnowingFiveGramDiceNormalized"                 
# [21] "winnowingFourGramDiceNormalized"                 
# [22] "threeGramDiceNormalized"                         
# [23] "threeGramDiceNormalizedPadding"                  
# [24] "fourGramDiceNormalizedPadding"                   
# [25] "threeGramJaccardNormalizedPadding"               
# [26] "fourGramJaccardNormalizedPadding"                
# [27] "tokenDiceNormalized"                             
# [28] "tokenDice"


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


# DECISION: Select metrics that are in 99% quantile of sample_random, sample_java_random, and sample_random_99 for text or code

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
MatthewsCorrelationText_99 <- quantile(sample_random$MatthewsCorrelationText, 0.99)
sample_random_text_candidates <- sample_random[sample_random$MatthewsCorrelationText >= MatthewsCorrelationText_99,]

MatthewsCorrelationText_99 <- quantile(sample_java_random$MatthewsCorrelationText, 0.99)
sample_java_random_text_candidates <- sample_java_random[sample_java_random$MatthewsCorrelationText >= MatthewsCorrelationText_99,]

MatthewsCorrelationText_99 <- quantile(sample_random_99$MatthewsCorrelationText, 0.99)
sample_random_99_text_candidates <- sample_random_99[sample_random_99$MatthewsCorrelationText >= MatthewsCorrelationText_99,]

# backup metric
backup_candidates <- sample_random[sample_random$MetricType == "EDIT" | grepl("token", sample_random$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_99_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.99)
sample_random_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_99_backup,]

backup_candidates <- sample_java_random[sample_java_random$MetricType == "EDIT" | grepl("token", sample_java_random$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_99_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.99)
sample_java_random_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_99_backup,]

backup_candidates <- sample_random_99[sample_random_99$MetricType == "EDIT" | grepl("token", sample_random_99$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationText_99_backup <- quantile(backup_candidates$MatthewsCorrelationText, 0.99)
sample_random_99_text_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText >= MatthewsCorrelationText_99_backup,]


# final candidates
candidates_text <- intersect(
  intersect(unique(sample_random_text_candidates$Metric), unique(sample_java_random_text_candidates$Metric)),
  unique(sample_random_99_text_candidates$Metric)
)
length(candidates_text)
# 2
candidates_text
# [1] "threeGramDice"   
# [2] "threeGramJaccard"

summary(sample_candidates$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4058  0.6721  0.7858  0.7312  0.8206  0.8572 

setorderv(sample_candidates, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))
sample_candidates_text <- sample_candidates[sample_candidates$Metric %in% candidates_text,]
sample_candidates_text[1:3,c("Metric", "Threshold", "MatthewsCorrelationText", "Runtime")]
#              Metric Threshold MatthewsCorrelationText   Runtime
# 1:    threeGramDice      0.30               0.8542778 816896752
# 2: threeGramJaccard      0.18               0.8516771 924226085
# 3:    threeGramDice      0.29               0.8512588 820814447


# backup metric
backup_candidates_text <- intersect(
  intersect(unique(sample_random_text_candidates_backup$Metric), unique(sample_java_random_text_candidates_backup$Metric)),
  unique(sample_random_99_text_candidates_backup$Metric)
)
length(backup_candidates_text)
# 2
backup_candidates_text
# [1] "tokenDiceNormalized"   
# [2] "tokenJaccardNormalized"

setorderv(sample_candidates, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))
sample_candidates_backup_text <- sample_candidates[sample_candidates$Metric %in% backup_candidates_text,]
sample_candidates_backup_text[1:3,c("Metric", "Threshold", "MatthewsCorrelationText", "Runtime")]
# Metric Threshold MatthewsCorrelationText    Runtime
# 1:    tokenDiceNormalized      0.23               0.8508510 1104157371
# 2: tokenJaccardNormalized      0.13               0.8508510 1209240551
# 3:    tokenDiceNormalized      0.26               0.8503906 1093360816


## code

# order samples
setorderv(sample_random, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
setorderv(sample_java_random, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
setorderv(sample_random_99, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))

# select candidates
MatthewsCorrelationCode_99 <- quantile(sample_random$MatthewsCorrelationCode, 0.99)
sample_random_code_candidates <- sample_random[sample_random$MatthewsCorrelationCode >= MatthewsCorrelationCode_99,]

MatthewsCorrelationCode_99 <- quantile(sample_java_random$MatthewsCorrelationCode, 0.99)
sample_java_random_code_candidates <- sample_java_random[sample_java_random$MatthewsCorrelationCode >= MatthewsCorrelationCode_99,]

MatthewsCorrelationCode_99 <- quantile(sample_random_99$MatthewsCorrelationCode, 0.99)
sample_random_99_code_candidates <- sample_random_99[sample_random_99$MatthewsCorrelationCode >= MatthewsCorrelationCode_99,]

# backup metric
backup_candidates <- sample_random[sample_random$MetricType == "EDIT" | grepl("token", sample_random$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_99_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.99)
sample_random_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_99_backup,]

backup_candidates <- sample_java_random[sample_java_random$MetricType == "EDIT" | grepl("token", sample_java_random$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_99_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.99)
sample_java_random_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_99_backup,]

backup_candidates <- sample_random_99[sample_random_99$MetricType == "EDIT" | grepl("token", sample_random_99$Metric, ignore.case=TRUE, perl=TRUE),]
MatthewsCorrelationCode_99_backup <- quantile(backup_candidates$MatthewsCorrelationCode, 0.99)
sample_random_99_code_candidates_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode >= MatthewsCorrelationCode_99_backup,]


# final candidates
candidates_text <- intersect(
  intersect(unique(sample_random_code_candidates$Metric), unique(sample_java_random_code_candidates$Metric)),
  unique(sample_random_99_code_candidates$Metric)
)
length(candidates_code)
# 9
candidates_code
# [1] "winnowingFiveGramDiceNormalized"  
# [2] "winnowingFourGramDiceNormalized"  
# [3] "winnowingThreeGramDiceNormalized" 
# [4] "threeGramDiceNormalized"          
# [5] "threeGramDiceNormalizedPadding"   
# [6] "fourGramDiceNormalizedPadding"    
# [7] "threeGramJaccardNormalized"       
# [8] "threeGramJaccardNormalizedPadding"
# [9] "fourGramJaccardNormalizedPadding" 

summary(sample_candidates$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5520  0.8143  0.8797  0.8394  0.9008  0.9209 

setorderv(sample_candidates, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
sample_candidates_code <- sample_candidates[sample_candidates$Metric %in% candidates_code,]
sample_candidates_code[1:3,c("Metric", "Threshold", "MatthewsCorrelationCode", "Runtime")]
# Metric Threshold MatthewsCorrelationCode   Runtime
# 1: winnowingFourGramDiceNormalized      0.23               0.9208735 845210866
# 2: winnowingFourGramDiceNormalized      0.25               0.9199509 829410024
# 3: winnowingFourGramDiceNormalized      0.20               0.9196971 841075205


# backup metric
backup_candidates_code <- intersect(
  intersect(unique(sample_random_code_candidates_backup$Metric), unique(sample_java_random_code_candidates_backup$Metric)),
  unique(sample_random_99_code_candidates_backup$Metric)
)
length(backup_candidates_code)
# 2
backup_candidates_code
# [1] "tokenDiceNormalized"   
# [2] "tokenJaccardNormalized"

setorderv(sample_candidates, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
sample_candidates_backup_code <- sample_candidates[sample_candidates$Metric %in% backup_candidates_code,]
sample_candidates_backup_code[1:3,c("Metric", "Threshold", "MatthewsCorrelationCode", "Runtime")]
# Metric Threshold MatthewsCorrelationCode    Runtime
# 1:    tokenDiceNormalized      0.32               0.9162871 1089122626
# 2: tokenJaccardNormalized      0.19               0.9162871 1215511980
# 3:    tokenDiceNormalized      0.28               0.9161630 1089860835


### check results of best metrics in sample_unclear_matching

# calculate Matthews correlation
sample_unclear_matching <- add_matthews_correlation(sample_unclear_matching)

summary(sample_unclear_matching$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2432  0.6865  0.7517  0.7244  0.7852  0.8508

summary(sample_unclear_matching$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2559  0.7177  0.7672  0.7564  0.8026  0.8723 
