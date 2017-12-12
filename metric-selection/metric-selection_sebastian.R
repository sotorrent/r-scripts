setwd("F:/Git/github/r-scripts/metric-selection") # Pfad bitte anpassen
#setwd("/Users/sebastian/git/github/r-scripts/metric-selection")
#setwd("C://Users//Lorik//Documents//GitHub//r-scripts//metric-selection")


# read results of first run with all metrics
library(data.table)
metric_comparison <- fread("MetricComparison_aggregated-all.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
nrow(metric_comparison)
# 1474


### TEXT ###

MatthewsCorrelationText <- metric_comparison$MatthewsCorrelationText

summary(MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.7735  0.9235  0.9630  0.9339  0.9700  0.9791 

boxplot(MatthewsCorrelationText)

MatthewsCorrelationText_99 <- MatthewsCorrelationText[MatthewsCorrelationText >= quantile(MatthewsCorrelationText, 0.99)]

length(MatthewsCorrelationText_99)
# 15

MatthewsCorrelationText_99_metrics <- unique(metric_comparison[metric_comparison$MatthewsCorrelationText %in% MatthewsCorrelationText_99,]$Metric)
length(MatthewsCorrelationText_99_metrics)
# 14

MatthewsCorrelationText_99_metrics
# [1] "cosineThreeGramNormalizedBool"   
# [2] "fourGramDice"                    
# [3] "fiveGramDiceNormalized"          
# [4] "threeGramDiceNormalized"         
# [5] "winnowingThreeGramDiceNormalized"
# [6] "manhattanFiveGramNormalized"     
# [7] "manhattanThreeGramNormalized"    
# [8] "fourGramDiceNormalizedPadding"   
# [9] "fiveGramDice"                    
# [10] "threeGramDice"                   
# [11] "fiveGramDiceNormalizedPadding"   
# [12] "fourGramDiceNormalized"          
# [13] "tokenDiceNormalized"             
# [14] "manhattanFourGramNormalized"

# backup metric
backup_candidates <- metric_comparison[metric_comparison$FailuresText == 0 & metric_comparison$FailuresCode == 0,]
MatthewsCorrelationText_backup <- backup_candidates$MatthewsCorrelationText

MatthewsCorrelationText_99_backup <- MatthewsCorrelationText_backup[MatthewsCorrelationText_backup >= quantile(MatthewsCorrelationText_backup, 0.99)]

length(MatthewsCorrelationText_99_backup)
# 3

MatthewsCorrelationText_99_backup_metrics <- unique(backup_candidates[backup_candidates$MatthewsCorrelationText %in% MatthewsCorrelationText_99_backup,]$Metric)
length(MatthewsCorrelationText_99_backup_metrics)
# 2

MatthewsCorrelationText_99_backup_metrics
# [1] "cosineTokenNormalizedNormalizedTermFrequency"
# [2] "cosineTokenNormalizedBool"  


### CODE ###

MatthewsCorrelationCode <- metric_comparison$MatthewsCorrelationCode

summary(MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.8602  0.9583  0.9743  0.9595  0.9792  0.9843 

boxplot(MatthewsCorrelationCode)

MatthewsCorrelationCode_99 <- MatthewsCorrelationCode[MatthewsCorrelationCode >= quantile(MatthewsCorrelationCode, 0.99)]

length(MatthewsCorrelationCode_99)
# 15

MatthewsCorrelationCode_99_metrics <- unique(metric_comparison[metric_comparison$MatthewsCorrelationCode %in% MatthewsCorrelationCode_99,]$Metric)
length(MatthewsCorrelationCode_99_metrics)
# 10

MatthewsCorrelationCode_99_metrics
# [1] "manhattanFiveGramNormalized"      
# [2] "twoShingleDiceNormalized"         
# [3] "fiveGramDiceNormalized"           
# [4] "winnowingFourGramOptimalAlignment"
# [5] "manhattanTwoShingleNormalized"    
# [6] "fourGramDiceNormalized"           
# [7] "manhattanFourGramNormalized"      
# [8] "fiveGramJaccard"                  
# [9] "manhattanThreeGramNormalized"     
# [10] "fourGramJaccard"  

# backup metric
backup_candidates <- metric_comparison[metric_comparison$FailuresText == 0 & metric_comparison$FailuresCode == 0,]
MatthewsCorrelationCode_backup <- backup_candidates$MatthewsCorrelationCode

MatthewsCorrelationCode_99_backup <- MatthewsCorrelationCode_backup[MatthewsCorrelationCode_backup >= quantile(MatthewsCorrelationCode_backup, 0.99)]

length(MatthewsCorrelationCode_99_backup)
# 3

MatthewsCorrelationCode_99_backup_metrics <- unique(backup_candidates[backup_candidates$MatthewsCorrelationCode %in% MatthewsCorrelationCode_99_backup,]$Metric)
length(MatthewsCorrelationCode_99_backup_metrics)
# 3

MatthewsCorrelationCode_99_backup_metrics
# [1] "cosineTokenNormalizedBool"
# [2] "twoGramJaccard"           
# [3] "manhattanTokenNormalized" 


### BOTH ###

boxplot(MatthewsCorrelationText, MatthewsCorrelationCode)

# best metrics
selected_metrics <- unique(c(
  MatthewsCorrelationText_99_metrics,
  MatthewsCorrelationCode_99_metrics
))

length(selected_metrics)
# 19

selected_metrics
# [1] "cosineThreeGramNormalizedBool"    
# [2] "fourGramDice"                     
# [3] "fiveGramDiceNormalized"           
# [4] "threeGramDiceNormalized"          
# [5] "winnowingThreeGramDiceNormalized" 
# [6] "manhattanFiveGramNormalized"      
# [7] "manhattanThreeGramNormalized"     
# [8] "fourGramDiceNormalizedPadding"    
# [9] "fiveGramDice"                     
# [10] "threeGramDice"                    
# [11] "fiveGramDiceNormalizedPadding"    
# [12] "fourGramDiceNormalized"           
# [13] "tokenDiceNormalized"              
# [14] "manhattanFourGramNormalized"      
# [15] "twoShingleDiceNormalized"         
# [16] "winnowingFourGramOptimalAlignment"
# [17] "manhattanTwoShingleNormalized"    
# [18] "fiveGramJaccard"                  
# [19] "fourGramJaccard"


# backup metric
selected_metrics_backup <- unique(c(
  MatthewsCorrelationText_99_backup_metrics,
  MatthewsCorrelationCode_99_backup_metrics
))

length(selected_metrics_backup)
# 4

selected_metrics_backup
# [1] "cosineTokenNormalizedNormalizedTermFrequency"
# [2] "cosineTokenNormalizedBool"                   
# [3] "twoGramJaccard"                              
# [4] "manhattanTokenNormalized" 


##################
### SECOND RUN ###
##################

# read results of second run with selected metrics and more thresholds
library(data.table)
metric_comparison_selected <- fread("MetricComparison_aggregated-selected.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
nrow(metric_comparison_selected)
# 2020


### TEXT ###

MatthewsCorrelationText <- metric_comparison_selected$MatthewsCorrelationText

summary(MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.7735  0.9240  0.9640  0.9370  0.9739  0.9795

boxplot(MatthewsCorrelationText)

max(MatthewsCorrelationText)
# 0.979533

selected_metric_text <- metric_comparison_selected[metric_comparison_selected$MatthewsCorrelationText == max(MatthewsCorrelationText)]
selected_metrics_text$Metric
# [1] "manhattanFourGramNormalized"
selected_metrics_text$Threshold
# 0.17


### CODE ###

MatthewsCorrelationCode <- metric_comparison_selected$MatthewsCorrelationCode

summary(MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.8602  0.9616  0.9769  0.9627  0.9813  0.9843 

boxplot(MatthewsCorrelationCode)

max(MatthewsCorrelationCode)
# 0.9842914

selected_metric_code <- metric_comparison_selected[metric_comparison_selected$MatthewsCorrelationCode == max(MatthewsCorrelationCode)]
selected_metric_code$Metric
# [1] "fiveGramJaccard"              
# [2] "manhattanTwoShingleNormalized"
# [3] "fiveGramDice"
min_runtime_selected_metric_code <- min(selected_metric_code$Runtime)
selected_metric_code <- metric_comparison_selected[metric_comparison_selected$MatthewsCorrelationCode == max(MatthewsCorrelationCode)
                                                & metric_comparison_selected$Runtime == min_runtime_selected_metric_code]
selected_metric_code$Metric
# [1] "fiveGramDice"
selected_metric_code$Threshold
# 0.33
