setwd("F:/Git/github/r-scripts/metric-selection") # Pfad bitte anpassen
#setwd("/Users/sebastian/git/github/r-scripts/metric-selection")
#setwd("C://Users//Lorik//Documents//GitHub//r-scripts//metric-selection")


# read results of first run with all metrics
library(data.table)
metric_comparison <- fread("2017-12-14_samples_comparison-all.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
nrow(metric_comparison)
# 1474


### TEXT ###

MatthewsCorrelationText <- metric_comparison$MatthewsCorrelationText

summary(MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4342  0.6798  0.7791  0.7284  0.8132  0.8656

boxplot(MatthewsCorrelationText)

MatthewsCorrelationText_99 <- MatthewsCorrelationText[MatthewsCorrelationText >= quantile(MatthewsCorrelationText, 0.99)]

length(MatthewsCorrelationText_99)
# 15

MatthewsCorrelationText_99_metrics <- unique(metric_comparison[metric_comparison$MatthewsCorrelationText %in% MatthewsCorrelationText_99,]$Metric)
length(MatthewsCorrelationText_99_metrics)
# 15

MatthewsCorrelationText_99_metrics
# [1] "tokenDiceNormalized"          
# [2] "manhattanThreeGramNormalized" 
# [3] "fourGramDiceNormalized"       
# [4] "threeGramDiceNormalized"      
# [5] "manhattanFiveGramNormalized"  
# [6] "fiveGramDiceNormalized"       
# [7] "cosineThreeGramNormalizedBool"
# [8] "manhattanFourGramNormalized"  
# [9] "fourGramDiceNormalizedPadding"
# [10] "fiveGramJaccardNormalized"    
# [11] "fiveGramDiceNormalizedPadding"
# [12] "winnowingFiveGramDice"        
# [13] "threeGramDice"                
# [14] "fourGramDice"                 
# [15] "fiveGramDice"

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
# 0.5891  0.8153  0.8653  0.8300  0.8910  0.9157 

boxplot(MatthewsCorrelationCode)

MatthewsCorrelationCode_99 <- MatthewsCorrelationCode[MatthewsCorrelationCode >= quantile(MatthewsCorrelationCode, 0.99)]

length(MatthewsCorrelationCode_99)
# 15

MatthewsCorrelationCode_99_metrics <- unique(metric_comparison[metric_comparison$MatthewsCorrelationCode %in% MatthewsCorrelationCode_99,]$Metric)
length(MatthewsCorrelationCode_99_metrics)
# 10

MatthewsCorrelationCode_99_metrics
# [1] "twoShingleDiceNormalized"         
# [2] "manhattanFiveGramNormalized"      
# [3] "fiveGramJaccard"                  
# [4] "fiveGramDiceNormalized"           
# [5] "manhattanThreeGramNormalized"     
# [6] "manhattanTwoShingleNormalized"    
# [7] "winnowingFourGramOptimalAlignment"
# [8] "manhattanThreeShingleNormalized"  
# [9] "fourGramDiceNormalized"           
# [10] "manhattanFourGramNormalized"

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
# [1] "twoGramJaccard"           
# [2] "manhattanTokenNormalized" 
# [3] "cosineTokenNormalizedBool"


### BOTH ###

boxplot(MatthewsCorrelationText, MatthewsCorrelationCode)

# best metrics
selected_metrics <- unique(c(
  MatthewsCorrelationText_99_metrics,
  MatthewsCorrelationCode_99_metrics
))

length(selected_metrics)
# 20

selected_metrics
# [1] "tokenDiceNormalized"              
# [2] "manhattanThreeGramNormalized"     
# [3] "fourGramDiceNormalized"           
# [4] "threeGramDiceNormalized"          
# [5] "manhattanFiveGramNormalized"      
# [6] "fiveGramDiceNormalized"           
# [7] "cosineThreeGramNormalizedBool"    
# [8] "manhattanFourGramNormalized"      
# [9] "fourGramDiceNormalizedPadding"    
# [10] "fiveGramJaccardNormalized"        
# [11] "fiveGramDiceNormalizedPadding"    
# [12] "winnowingFiveGramDice"            
# [13] "threeGramDice"                    
# [14] "fourGramDice"                     
# [15] "fiveGramDice"                     
# [16] "twoShingleDiceNormalized"         
# [17] "fiveGramJaccard"                  
# [18] "manhattanTwoShingleNormalized"    
# [19] "winnowingFourGramOptimalAlignment"
# [20] "manhattanThreeShingleNormalized"

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
metric_comparison_selected <- fread("2017-12-15_samples_comparison-selected.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
nrow(metric_comparison_selected)
# 2525


### TEXT ###

MatthewsCorrelationText <- metric_comparison_selected$MatthewsCorrelationText

summary(MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4359  0.6915  0.8000  0.7461  0.8324  0.8682 

boxplot(MatthewsCorrelationText)

# best metric
max(MatthewsCorrelationText)
# 0.8682407
selected_metric_text <- metric_comparison_selected[metric_comparison_selected$MatthewsCorrelationText == max(MatthewsCorrelationText)]
selected_metric_text$Metric
# [1] "manhattanFourGramNormalized"
selected_metric_text$Threshold
# 0.17

# backup metric
backup_candidates <- metric_comparison_selected[metric_comparison_selected$FailuresText == 0 & metric_comparison_selected$FailuresCode == 0,]
max(backup_candidates$MatthewsCorrelationText)
# 0.8508292
selected_metric_text_backup <- backup_candidates[backup_candidates$MatthewsCorrelationText == max(backup_candidates$MatthewsCorrelationText)]
selected_metric_text_backup$Metric
# [1] "cosineTokenNormalizedBool"
selected_metric_text_backup$Threshold
# 0.32


### CODE ###

MatthewsCorrelationCode <- metric_comparison_selected$MatthewsCorrelationCode

summary(MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5891  0.8272  0.8817  0.8450  0.9011  0.9159 

boxplot(MatthewsCorrelationCode)

# best metric
max(MatthewsCorrelationCode)
# 0.9158857
selected_metric_code <- metric_comparison_selected[metric_comparison_selected$MatthewsCorrelationCode == max(MatthewsCorrelationCode)]
selected_metric_code$Metric
# [1] "winnowingFourGramOptimalAlignment"
selected_metric_code$Threshold
# 0.25

# backup metric
backup_candidates <- metric_comparison_selected[metric_comparison_selected$FailuresText == 0 & metric_comparison_selected$FailuresCode == 0,]
max(backup_candidates$MatthewsCorrelationCode)
# 0.9117382
selected_metric_code_backup <- backup_candidates[backup_candidates$MatthewsCorrelationCode == max(backup_candidates$MatthewsCorrelationCode)]
selected_metric_code_backup$Metric
# [1] "cosineTokenNormalizedBool"
selected_metric_code_backup$Threshold
# 0.38
