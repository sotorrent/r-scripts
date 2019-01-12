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

ITERATION <- 3

# read results of metric evaluation run
read_metrics_evaluation_per_sample(ITERATION, "all")


#### calculate Matthews correlation and select candidates using 95% quantile for text and code


### sample randomly drawn from all SO posts
sample_random <- add_matthews_correlation(sample_random)

# analyze runtime
summary(sample_random$Runtime)
#  Min.     1st Qu.      Median        Mean     3rd Qu.        Max. 
# 7882262   215241699   310820289   772560791   453956424 12810478246 
Runtime_Q3 <- quantile(sample_random$Runtime, 0.75)

## text
# order
setorderv(sample_random, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze quality
summary(sample_random$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3448  0.6707  0.8481  0.7709  0.9097  0.9930 
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
# 0.6462  0.8677  0.9518  0.9088  0.9792  1.0000
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
# 189346038   7963684801  11822558976  25616528832  17424680030 386150345961
Runtime_Q3 <- quantile(sample_random_99$Runtime, 0.75)

## text
# order
setorderv(sample_random_99, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze quality
summary(sample_random_99$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3670  0.6105  0.7741  0.7218  0.8512  0.9529
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
# 0.5005  0.7727  0.9080  0.8424  0.9532  0.9929 
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
#   Min.     1st Qu.      Median        Mean     3rd Qu.        Max. 
# 18728173   628294662   917454407  1867473476  1206680935 35548132847
Runtime_Q3 <- quantile(sample_java_random$Runtime, 0.75)

## text
# order
setorderv(sample_java_random, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze quality
summary(sample_java_random$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3708  0.6217  0.7734  0.7349  0.8783  0.9943 
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
# 0.6162  0.8522  0.9441  0.8998  0.9755  1.0000 
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
# 20571429  1447843728  2111581933  3049814151  2877635349 25509431128
Runtime_Q3 <- quantile(sample_unclear_matching$Runtime, 0.75)

## text
# order
setorderv(sample_unclear_matching, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze quality
summary(sample_unclear_matching$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3756  0.6377  0.7344  0.7029  0.7888  0.8975
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
# 0.5819  0.7166  0.7761  0.7575  0.8132  0.8737
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
# 29937591  1506081520  2188009577  3404026719  3071706028 22751443669
Runtime_Q3 <- quantile(sample_multiple_possible_links$Runtime, 0.75)

## text
# order
setorderv(sample_multiple_possible_links, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))

# analyze quality
summary(sample_multiple_possible_links$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5965  0.7602  0.8630  0.8243  0.9052  0.9573
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
# 0.7687  0.9271  0.9556  0.9308  0.9662  0.9926
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
# 46


# code
candidates_code <- intersect(
  intersect(unique(sample_random_code_candidates$MetricCode), unique(sample_java_random_code_candidates$MetricCode)),
  unique(sample_random_99_code_candidates$MetricCode)
)
length(candidates_code)
# 20

# both
candidates <- unique(c(candidates_text, candidates_code))
length(candidates)
# 53
candidates
# [1] "fourGramDice"                                     
# [2] "fiveGramDice"                                     
# [3] "fiveGramDiceNormalized"                           
# [4] "cosineThreeGramNormalizedNormalizedTermFrequency" 
# [5] "manhattanFourGramNormalized"                      
# [6] "twoShingleDiceNormalized"                         
# [7] "cosineTwoShingleNormalizedBool"                   
# [8] "cosineTwoShingleNormalizedTermFrequency"          
# [9] "cosineTwoShingleNormalizedNormalizedTermFrequency"
# [10] "manhattanFiveGramNormalized"                      
# [11] "cosineTokenNormalizedNormalizedTermFrequency"     
# [12] "threeGramDice"                                    
# [13] "threeGramDiceNormalized"                          
# [14] "fiveGramDiceNormalizedPadding"                    
# [15] "manhattanThreeGramNormalized"                     
# [16] "cosineThreeGramNormalizedBool"                    
# [17] "cosineFiveGramNormalizedBool"                     
# [18] "cosineFiveGramNormalizedNormalizedTermFrequency"  
# [19] "winnowingTwoGramDice"                             
# [20] "fourGramDiceNormalized"                           
# [21] "cosineFourGramNormalizedBool"                     
# [22] "cosineFourGramNormalizedNormalizedTermFrequency"  
# [23] "threeGramDiceNormalizedPadding"                   
# [24] "cosineTwoGramNormalizedNormalizedTermFrequency"   
# [25] "fourGramJaccard"                                  
# [26] "fiveGramJaccard"                                  
# [27] "fiveGramJaccardNormalized"                        
# [28] "threeGramJaccard"                                 
# [29] "cosineTokenNormalizedTermFrequency"               
# [30] "cosineTokenNormalizedBool"                        
# [31] "manhattanTokenNormalized"                         
# [32] "fourGramDiceNormalizedPadding"                    
# [33] "threeGramJaccardNormalizedPadding"                
# [34] "cosineTwoGramNormalizedBool"                      
# [35] "twoGramJaccardNormalizedPadding"                  
# [36] "fourGramJaccardNormalized"                        
# [37] "fourGramJaccardNormalizedPadding"                 
# [38] "fiveGramJaccardNormalizedPadding"                 
# [39] "threeGramJaccardNormalized"                       
# [40] "tokenDiceNormalized"                              
# [41] "tokenJaccardNormalized"                           
# [42] "winnowingTwoGramDiceNormalized"                   
# [43] "winnowingFourGramJaccardNormalized"               
# [44] "winnowingFourGramDiceNormalized"                  
# [45] "twoGramDiceNormalizedPadding"                     
# [46] "twoGramJaccardNormalized"                         
# [47] "tokenDice"                                        
# [48] "tokenJaccard"                                     
# [49] "manhattanTwoShingleNormalized"                    
# [50] "twoGramDice"                                      
# [51] "twoGramJaccard"                                   
# [52] "winnowingTwoGramJaccardNormalized"                
# [53] "twoGramDiceNormalized"          


## backup metrics

# text
candidates_backup_text <- intersect(
  intersect(unique(sample_random_text_candidates_backup$MetricText), unique(sample_java_random_text_candidates_backup$MetricText)),
  unique(sample_random_99_text_candidates_backup$MetricText)
)
length(candidates_backup_text)
# 8

# code
candidates_backup_code <- intersect(
  intersect(unique(sample_random_code_candidates_backup$MetricCode), unique(sample_java_random_code_candidates_backup$MetricCode)),
  unique(sample_random_99_code_candidates_backup$MetricCode)
)
length(candidates_backup_code)
# 7

# both
candidates_backup <- unique(c(candidates_backup_text, candidates_backup_code))
length(candidates_backup)
# 8
candidates_backup
# [1] "cosineTokenNormalizedNormalizedTermFrequency"
# [2] "cosineTokenNormalizedTermFrequency"          
# [3] "cosineTokenNormalizedBool"                   
# [4] "manhattanTokenNormalized"                    
# [5] "tokenDice"                                   
# [6] "tokenDiceNormalized"                         
# [7] "tokenJaccardNormalized"                      
# [8] "tokenJaccard" 


###############################################################################################


### second run with selected metrics ###

read_metrics_evaluation_per_sample(ITERATION, "selected")
merge_and_matthews_correlation()

# DECISION: Select metrics that are in 95% quantile of sample_random, sample_java_random, and sample_random_99 for text/code


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
# 16
candidates_text
# [1] "fourGramDice"                                   
# [2] "fiveGramDice"                                   
# [3] "fourGramJaccard"                                
# [4] "fiveGramDiceNormalized"                         
# [5] "fiveGramDiceNormalizedPadding"                  
# [6] "threeGramDice"                                  
# [7] "threeGramJaccard"                               
# [8] "threeGramDiceNormalized"                        
# [9] "fourGramDiceNormalized"                         
# [10] "fourGramJaccardNormalized"                      
# [11] "fiveGramJaccardNormalized"                      
# [12] "cosineThreeGramNormalizedBool"                  
# [13] "cosineFourGramNormalizedBool"                   
# [14] "cosineFourGramNormalizedNormalizedTermFrequency"
# [15] "cosineFiveGramNormalizedBool"                   
# [16] "cosineFiveGramNormalizedNormalizedTermFrequency"  

summary(sample_candidates$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3748  0.6489  0.8113  0.7646  0.9079  0.9793

setorderv(sample_candidates, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))
sample_candidates_text <- sample_candidates[sample_candidates$MetricText %in% candidates_text,]

MatthewsCorrelationText_99 <- quantile(sample_candidates_text$MatthewsCorrelationText, 0.99)
MatthewsCorrelationText_99
# 99% 
# 0.9694853

sample_candidates_text[sample_candidates_text$MatthewsCorrelationText>=MatthewsCorrelationText_99, c("MetricText", "ThresholdText", "MatthewsCorrelationText", "Runtime")]
#                       MetricText ThresholdText MatthewsCorrelationText     Runtime
#  1:                  fiveGramDice          0.04               0.9792927 10325876877
#  2:                  fourGramDice          0.06               0.9781243 10027900639
#  3:                  fiveGramDice          0.03               0.9781243 10298711228
#  4:                  fourGramDice          0.05               0.9780786 10029450246
#  5:               fourGramJaccard          0.03               0.9759358 11393259297
#  6:                  fiveGramDice          0.05               0.9750565 10315990600
#  7:                  fourGramDice          0.04               0.9747212  9967997293
#  8:                  fourGramDice          0.08               0.9740049  9962267800
#  9:                  fourGramDice          0.07               0.9738674  9865499502
# 10:                  fiveGramDice          0.02               0.9725230 10602254860
# 11:               fourGramJaccard          0.02               0.9725230 11365129056
# 12:               fourGramJaccard          0.04               0.9707061 11348894511
# 13:                  fourGramDice          0.03               0.9703248  9955487032
# 14:                  fiveGramDice          0.01               0.9703248 10360085547
# 15:                  fiveGramDice          0.06               0.9698277 10291304396
# 16: cosineThreeGramNormalizedBool          0.14               0.9696582 18883438732
# 17:     fourGramJaccardNormalized          0.03               0.9695056 13475070910


# backup metric
backup_candidates_text <- intersect(
  intersect(unique(sample_random_text_candidates_backup$MetricText), unique(sample_java_random_text_candidates_backup$MetricText)),
  unique(sample_random_99_text_candidates_backup$MetricText)
)
length(backup_candidates_text)
# 2
backup_candidates_text
# [1] "cosineTokenNormalizedNormalizedTermFrequency"
# [2] "cosineTokenNormalizedBool"  

setorderv(sample_candidates, c("MatthewsCorrelationText", "Runtime"), c(-1, 1))
sample_candidates_backup_text <- sample_candidates[sample_candidates$MetricText %in% backup_candidates_text,]

MatthewsCorrelationText_99_backup <- quantile(sample_candidates_backup_text$MatthewsCorrelationText, 0.99)
MatthewsCorrelationText_99_backup
# 99% 
# 0.9455093 

sample_candidates_backup_text[sample_candidates_backup_text$MatthewsCorrelationText>=MatthewsCorrelationText_99_backup, c("MetricText", "ThresholdText", "MatthewsCorrelationText", "Runtime")]
#                                      MetricText ThresholdText MatthewsCorrelationText    Runtime
# 1: cosineTokenNormalizedNormalizedTermFrequency          0.16               0.9490701 9014759306
# 2: cosineTokenNormalizedNormalizedTermFrequency          0.15               0.9478088 9005284067
# 3: cosineTokenNormalizedNormalizedTermFrequency          0.14               0.9455159 8920736440


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
# 8
candidates_code
# [1] "tokenDice"                                   
# [2] "tokenJaccard"                                
# [3] "tokenDiceNormalized"                         
# [4] "tokenJaccardNormalized"                      
# [5] "manhattanTokenNormalized"                    
# [6] "cosineTokenNormalizedNormalizedTermFrequency"
# [7] "cosineTokenNormalizedBool"                   
# [8] "manhattanTwoShingleNormalized"

summary(sample_candidates$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5393  0.8238  0.9309  0.8819  0.9731  0.9932

setorderv(sample_candidates, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
sample_candidates_code <- sample_candidates[sample_candidates$MetricCode %in% candidates_code,]

MatthewsCorrelationCode_99 <- quantile(sample_candidates_code$MatthewsCorrelationCode, 0.99)
MatthewsCorrelationCode_99
# 99% 
# 0.990949

sample_candidates_code[sample_candidates_code$MatthewsCorrelationCode>=MatthewsCorrelationCode_99, c("MetricCode", "ThresholdCode", "MatthewsCorrelationCode", "Runtime")]
#                                       MetricCode ThresholdCode MatthewsCorrelationCode    Runtime
#  1:                          tokenDiceNormalized          0.10               0.9932143 7306147136
#  2:                          tokenDiceNormalized          0.09               0.9920705 7251743520
#  3:                          tokenDiceNormalized          0.08               0.9920705 7300002847
#  4:                       tokenJaccardNormalized          0.05               0.9920705 7529239547
#  5:                          tokenDiceNormalized          0.12               0.9909490 7250358520
#  6:                          tokenDiceNormalized          0.11               0.9909490 7259104716
#  7:                       tokenJaccardNormalized          0.06               0.9909490 7470034283
#  8:                    cosineTokenNormalizedBool          0.10               0.9909490 8915778936
#  9: cosineTokenNormalizedNormalizedTermFrequency          0.07               0.9909490 8955687973
# 10: cosineTokenNormalizedNormalizedTermFrequency          0.08               0.9909490 9113187264
# 11: cosineTokenNormalizedNormalizedTermFrequency          0.09               0.9909490 9168843063


# backup metric
backup_candidates_code <- intersect(
  intersect(unique(sample_random_code_candidates_backup$MetricCode), unique(sample_java_random_code_candidates_backup$MetricCode)),
  unique(sample_random_99_code_candidates_backup$MetricCode)
)
length(backup_candidates_code)
# 4
backup_candidates_code
# [1] "tokenDiceNormalized"                         
# [2] "tokenJaccardNormalized"                      
# [3] "cosineTokenNormalizedNormalizedTermFrequency"
# [4] "cosineTokenNormalizedBool"

setorderv(sample_candidates, c("MatthewsCorrelationCode", "Runtime"), c(-1, 1))
sample_candidates_backup_code <- sample_candidates[sample_candidates$MetricCode %in% backup_candidates_code,]

MatthewsCorrelationCode_99_backup <- quantile(sample_candidates_backup_code$MatthewsCorrelationCode, 0.99)
MatthewsCorrelationCode_99_backup
# 99% 
# 0.990949

sample_candidates_backup_code[sample_candidates_backup_code$MatthewsCorrelationCode>=MatthewsCorrelationCode_99_backup, c("MetricCode", "ThresholdCode", "MatthewsCorrelationCode", "Runtime")]
#                                       MetricCode ThresholdCode MatthewsCorrelationCode    Runtime
#  1:                          tokenDiceNormalized          0.10               0.9932143 7306147136
#  2:                          tokenDiceNormalized          0.09               0.9920705 7251743520
#  3:                          tokenDiceNormalized          0.08               0.9920705 7300002847
#  4:                       tokenJaccardNormalized          0.05               0.9920705 7529239547
#  5:                          tokenDiceNormalized          0.12               0.9909490 7250358520
#  6:                          tokenDiceNormalized          0.11               0.9909490 7259104716
#  7:                       tokenJaccardNormalized          0.06               0.9909490 7470034283
#  8:                    cosineTokenNormalizedBool          0.10               0.9909490 8915778936
#  9: cosineTokenNormalizedNormalizedTermFrequency          0.07               0.9909490 8955687973
# 10: cosineTokenNormalizedNormalizedTermFrequency          0.08               0.9909490 9113187264
# 11: cosineTokenNormalizedNormalizedTermFrequency          0.09               0.9909490 9168843063

### check results of best metrics in sample_unclear_matching

# calculate Matthews correlation
sample_unclear_matching <- add_matthews_correlation(sample_unclear_matching)

summary(sample_unclear_matching$MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3792  0.7152  0.7691  0.7459  0.7962  0.8789

summary(sample_unclear_matching$MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5819  0.7572  0.8025  0.7824  0.8228  0.8737


###############################################################################################


### third run with combined metrics ###

read_metrics_evaluation_per_sample(ITERATION, "combined")
merge_and_matthews_correlation()

# order candidates
## text
setorderv(sample_candidates, c("MatthewsCorrelationText", "Runtime"), c(-1,1))
sample_candidates[c(1:3),c("MatthewsCorrelationText", "Runtime", "MetricText", "ThresholdText", "MetricTextBackup", "ThresholdTextBackup", "MetricCode", "ThresholdCode", "MetricCodeBackup", "ThresholdCodeBackup")]
#    MatthewsCorrelationText    Runtime   MetricText ThresholdText                             MetricTextBackup ThresholdTextBackup          MetricCode ThresholdCode                             MetricCodeBackup ThresholdCodeBackup
# 1:                0.983577 9005704046 fiveGramDice          0.04 cosineTokenNormalizedNormalizedTermFrequency                0.14 tokenDiceNormalized          0.09 cosineTokenNormalizedNormalizedTermFrequency                0.07
# 2:                0.983577 9009099467 fiveGramDice          0.04 cosineTokenNormalizedNormalizedTermFrequency                0.16 tokenDiceNormalized          0.11                          tokenDiceNormalized                0.09
# 3:                0.983577 9014116290 fiveGramDice          0.04 cosineTokenNormalizedNormalizedTermFrequency                0.16 tokenDiceNormalized          0.09                    cosineTokenNormalizedBool                0.10

## code
setorderv(sample_candidates, c("MatthewsCorrelationCode", "Runtime"), c(-1,1))
sample_candidates[c(1:3),c("MatthewsCorrelationCode", "Runtime", "MetricText", "ThresholdText", "MetricTextBackup", "ThresholdTextBackup", "MetricCode", "ThresholdCode", "MetricCodeBackup", "ThresholdCodeBackup")]
#    MatthewsCorrelationCode    Runtime   MetricText ThresholdText                             MetricTextBackup ThresholdTextBackup          MetricCode ThresholdCode                             MetricCodeBackup ThresholdCodeBackup
# 1:               0.9932143 8783941844 fourGramDice          0.06 cosineTokenNormalizedNormalizedTermFrequency                0.16 tokenDiceNormalized           0.1                    cosineTokenNormalizedBool                0.10
# 2:               0.9932143 8806200150 fourGramDice          0.06 cosineTokenNormalizedNormalizedTermFrequency                0.16 tokenDiceNormalized           0.1 cosineTokenNormalizedNormalizedTermFrequency                0.08
# 3:               0.9932143 8807386799 fourGramDice          0.06 cosineTokenNormalizedNormalizedTermFrequency                0.15 tokenDiceNormalized           0.1                       tokenJaccardNormalized                0.06
## text and code
setorderv(sample_candidates, c("MatthewsCorrelationText", "MatthewsCorrelationCode", "Runtime"), c(-1,-1,1))
sample_candidates[c(1:3),c("MatthewsCorrelationText", "MatthewsCorrelationCode", "Runtime", "MetricText", "ThresholdText", "MetricTextBackup", "ThresholdTextBackup", "MetricCode", "ThresholdCode", "MetricCodeBackup", "ThresholdCodeBackup")]
#    MatthewsCorrelationText MatthewsCorrelationCode    Runtime   MetricText ThresholdText                             MetricTextBackup ThresholdTextBackup          MetricCode ThresholdCode                             MetricCodeBackup ThresholdCodeBackup
# 1:                0.983577               0.9932143 9031747513 fiveGramDice          0.04 cosineTokenNormalizedNormalizedTermFrequency                0.14 tokenDiceNormalized           0.1                       tokenJaccardNormalized                0.06
# 2:                0.983577               0.9932143 9036811343 fiveGramDice          0.04 cosineTokenNormalizedNormalizedTermFrequency                0.16 tokenDiceNormalized           0.1                          tokenDiceNormalized                0.09
# 3:                0.983577               0.9932143 9037980774 fiveGramDice          0.04 cosineTokenNormalizedNormalizedTermFrequency                0.15 tokenDiceNormalized           0.1 cosineTokenNormalizedNormalizedTermFrequency                0.07
               
# OBSERVATION: Tradeoff between MatthewsCorrelationText and MatthewsCorrelationCode (because other metric is relevant for context)


# order occording to sum of MatthewsCorrelationText and MatthewsCorrelationCode
sample_candidates$MatthewsCorrelationSum <- sample_candidates$MatthewsCorrelationText + sample_candidates$MatthewsCorrelationCode
setorderv(sample_candidates, c("MatthewsCorrelationSum", "Runtime"), c(-1,1))
sample_candidates[c(1:10),c("MatthewsCorrelationText", "MatthewsCorrelationCode", "Runtime", "MetricText", "ThresholdText", "MetricTextBackup", "ThresholdTextBackup", "MetricCode", "ThresholdCode", "MetricCodeBackup", "ThresholdCodeBackup")]
#     MatthewsCorrelationText MatthewsCorrelationCode    Runtime   MetricText ThresholdText                             MetricTextBackup ThresholdTextBackup          MetricCode ThresholdCode                             MetricCodeBackup ThresholdCodeBackup
#  1:                0.983577               0.9932143 9031747513 fiveGramDice          0.04 cosineTokenNormalizedNormalizedTermFrequency                0.14 tokenDiceNormalized           0.1                       tokenJaccardNormalized                0.06
#  2:                0.983577               0.9932143 9036811343 fiveGramDice          0.04 cosineTokenNormalizedNormalizedTermFrequency                0.16 tokenDiceNormalized           0.1                          tokenDiceNormalized                0.09
#  3:                0.983577               0.9932143 9037980774 fiveGramDice          0.04 cosineTokenNormalizedNormalizedTermFrequency                0.15 tokenDiceNormalized           0.1 cosineTokenNormalizedNormalizedTermFrequency                0.07
#  4:                0.983577               0.9932143 9039895192 fiveGramDice          0.04 cosineTokenNormalizedNormalizedTermFrequency                0.16 tokenDiceNormalized           0.1                          tokenDiceNormalized                0.11
#  5:                0.983577               0.9932143 9046226492 fiveGramDice          0.04 cosineTokenNormalizedNormalizedTermFrequency                0.16 tokenDiceNormalized           0.1                       tokenJaccardNormalized                0.06
#  6:                0.983577               0.9932143 9047739497 fiveGramDice          0.04 cosineTokenNormalizedNormalizedTermFrequency                0.15 tokenDiceNormalized           0.1                          tokenDiceNormalized                0.09
#  7:                0.983577               0.9932143 9051830972 fiveGramDice          0.04 cosineTokenNormalizedNormalizedTermFrequency                0.15 tokenDiceNormalized           0.1                          tokenDiceNormalized                0.12
#  8:                0.983577               0.9932143 9054631873 fiveGramDice          0.04 cosineTokenNormalizedNormalizedTermFrequency                0.16 tokenDiceNormalized           0.1                       tokenJaccardNormalized                0.05
#  9:                0.983577               0.9932143 9055805232 fiveGramDice          0.04 cosineTokenNormalizedNormalizedTermFrequency                0.15 tokenDiceNormalized           0.1                          tokenDiceNormalized                0.08
# 10:                0.983577               0.9932143 9056108008 fiveGramDice          0.04 cosineTokenNormalizedNormalizedTermFrequency                0.16 tokenDiceNormalized           0.1 cosineTokenNormalizedNormalizedTermFrequency                0.09

# DECISION: Choose option 1,.
# REMARK: Backup code metric will never be used, because the regular metric can be applied to all inputs.

