#setwd("F:/Git/github/r-scripts/metric-selection") # Pfad bitte anpassen
setwd("/Users/sebastian/git/github/r-scripts/metric-selection")

library(data.table)
metric_comparison <- fread("MetricComparison_aggregated-all.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)

nrow(metric_comparison)
# 1474


### TEXT ###

MatthewsCorrelationText <- metric_comparison$MatthewsCorrelationText

summary(MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.7723  0.9224  0.9633  0.9338  0.9711  0.9793 

boxplot(MatthewsCorrelationText)

MatthewsCorrelationText_99 <- MatthewsCorrelationText[MatthewsCorrelationText >= quantile(MatthewsCorrelationText, 0.99)]

length(MatthewsCorrelationText_99)
# 15

unique(metric_comparison[metric_comparison$MatthewsCorrelationText %in% MatthewsCorrelationText_99,]$Metric)
# [1] "manhattanThreeGramNormalized"                     
# [2] "fourGramDice"                                     
# [3] "cosineTwoShingleNormalizedTermFrequency"          
# [4] "cosineTwoShingleNormalizedNormalizedTermFrequency"
# [5] "cosineFourGramNormalizedBool"                     
# [6] "cosineThreeGramNormalizedNormalizedTermFrequency" 
# [7] "tokenDiceNormalized"                              
# [8] "cosineFiveGramNormalizedBool"                     
# [9] "manhattanFourGramNormalized"                      
# [10] "cosineThreeGramNormalizedBool"                    
# [11] "cosineFiveGramNormalizedNormalizedTermFrequency"  
# [12] "manhattanFiveGramNormalized"                      
# [13] "cosineFourGramNormalizedNormalizedTermFrequency"  


### CODE ###

MatthewsCorrelationCode <- metric_comparison$MatthewsCorrelationCode

summary(MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.8586  0.9568  0.9733  0.9587  0.9795  0.9846 

boxplot(MatthewsCorrelationCode)

MatthewsCorrelationCode_99 <- MatthewsCorrelationCode[MatthewsCorrelationCode >= quantile(MatthewsCorrelationCode, 0.99)]

length(MatthewsCorrelationCode_99)
# 16

unique(metric_comparison[metric_comparison$MatthewsCorrelationCode %in% MatthewsCorrelationCode_99,]$Metric)
# [1] "fourGramJaccardNormalizedPadding" "fiveGramDiceNormalizedPadding"   
# [3] "fourGramDice"                     "cosineFourGramNormalizedBool"    
# [5] "cosineThreeGramNormalizedBool"    "fiveGramJaccard"                 
# [7] "winnowingFiveGramDice"            "manhattanTokenNormalized"        
# [9] "fourGramDiceNormalizedPadding"    "winnowingFourGramDice"           
# [11] "cosineFiveGramNormalizedBool"     "winnowingFiveGramJaccard"        
# [13] "fiveGramDiceNormalized"           "fourGramDiceNormalized"          
# [15] "winnowingFourGramJaccard"    


### BOTH ###

boxplot(MatthewsCorrelationText, MatthewsCorrelationCode)

unique(c(
  metric_comparison[metric_comparison$MatthewsCorrelationText %in% MatthewsCorrelationText_99,]$Metric,
  metric_comparison[metric_comparison$MatthewsCorrelationCode %in% MatthewsCorrelationCode_99,]$Metric
))
# [1] "manhattanThreeGramNormalized"                     
# [2] "fourGramDice"                                     
# [3] "cosineTwoShingleNormalizedTermFrequency"          
# [4] "cosineTwoShingleNormalizedNormalizedTermFrequency"
# [5] "cosineFourGramNormalizedBool"                     
# [6] "cosineThreeGramNormalizedNormalizedTermFrequency" 
# [7] "tokenDiceNormalized"                              
# [8] "cosineFiveGramNormalizedBool"                     
# [9] "manhattanFourGramNormalized"                      
# [10] "cosineThreeGramNormalizedBool"                    
# [11] "cosineFiveGramNormalizedNormalizedTermFrequency"  
# [12] "manhattanFiveGramNormalized"                      
# [13] "cosineFourGramNormalizedNormalizedTermFrequency"  
# [14] "fourGramJaccardNormalizedPadding"                 
# [15] "fiveGramDiceNormalizedPadding"                    
# [16] "fiveGramJaccard"                                  
# [17] "winnowingFiveGramDice"                            
# [18] "manhattanTokenNormalized"                         
# [19] "fourGramDiceNormalizedPadding"                    
# [20] "winnowingFourGramDice"                            
# [21] "winnowingFiveGramJaccard"                         
# [22] "fiveGramDiceNormalized"                           
# [23] "fourGramDiceNormalized"                           
# [24] "winnowingFourGramJaccard" 

