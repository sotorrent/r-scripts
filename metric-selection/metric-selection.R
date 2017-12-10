setwd("F:/Git/github/r-scripts/metric-selection") # Pfad bitte anpassen
#setwd("/Users/sebastian/git/github/r-scripts/metric-selection")

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

unique(metric_comparison[metric_comparison$MatthewsCorrelationText %in% MatthewsCorrelationText_99,]$Metric)
# [1] "cosineThreeGramNormalizedBool"    "fourGramDice"                    
# [3] "fiveGramDiceNormalized"           "threeGramDiceNormalized"         
# [5] "winnowingThreeGramDiceNormalized" "manhattanFiveGramNormalized"     
# [7] "manhattanThreeGramNormalized"     "fourGramDiceNormalizedPadding"   
# [9] "fiveGramDice"                     "threeGramDice"                   
# [11] "fiveGramDiceNormalizedPadding"    "fourGramDiceNormalized"          
# [13] "tokenDiceNormalized"              "manhattanFourGramNormalized" 


### CODE ###

MatthewsCorrelationCode <- metric_comparison$MatthewsCorrelationCode

summary(MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.8602  0.9583  0.9743  0.9595  0.9792  0.9843 

boxplot(MatthewsCorrelationCode)

MatthewsCorrelationCode_99 <- MatthewsCorrelationCode[MatthewsCorrelationCode >= quantile(MatthewsCorrelationCode, 0.99)]

length(MatthewsCorrelationCode_99)
# 15

unique(metric_comparison[metric_comparison$MatthewsCorrelationCode %in% MatthewsCorrelationCode_99,]$Metric)
# [1] "manhattanFiveGramNormalized"       "twoShingleDiceNormalized"         
# [3] "fiveGramDiceNormalized"            "winnowingFourGramOptimalAlignment"
# [5] "manhattanTwoShingleNormalized"     "fourGramDiceNormalized"           
# [7] "manhattanFourGramNormalized"       "fiveGramJaccard"                  
# [9] "manhattanThreeGramNormalized"      "fourGramJaccard"  


### BOTH ###

boxplot(MatthewsCorrelationText, MatthewsCorrelationCode)

selected_metrics <- unique(c(
  metric_comparison[metric_comparison$MatthewsCorrelationText %in% MatthewsCorrelationText_99,]$Metric,
  metric_comparison[metric_comparison$MatthewsCorrelationCode %in% MatthewsCorrelationCode_99,]$Metric
))

length(selected_metrics)
# 19

selected_metrics
# [1] "cosineThreeGramNormalizedBool"     "fourGramDice"                     
# [3] "fiveGramDiceNormalized"            "threeGramDiceNormalized"          
# [5] "winnowingThreeGramDiceNormalized"  "manhattanFiveGramNormalized"      
# [7] "manhattanThreeGramNormalized"      "fourGramDiceNormalizedPadding"    
# [9] "fiveGramDice"                      "threeGramDice"                    
# [11] "fiveGramDiceNormalizedPadding"     "fourGramDiceNormalized"           
# [13] "tokenDiceNormalized"               "manhattanFourGramNormalized"      
# [15] "twoShingleDiceNormalized"          "winnowingFourGramOptimalAlignment"
# [17] "manhattanTwoShingleNormalized"     "fiveGramJaccard"                  
# [19] "fourGramJaccard"   

