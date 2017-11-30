setwd("F:/Git/github/r-scripts/metric-selection") # Pfad bitte anpassen

library(data.table)
metric_comparison <- fread("MetricComparison_aggregated.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)

nrow(metric_comparison)
# 938


### TEXT ###

MatthewsCorrelationText <- metric_comparison$MatthewsCorrelationText

summary(MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.7718  0.9240  0.9555  0.9380  0.9670  0.9776 

boxplot(MatthewsCorrelationText)

MatthewsCorrelationText_99 <- MatthewsCorrelationText[MatthewsCorrelationText >= quantile(MatthewsCorrelationText, 0.99)]

length(MatthewsCorrelationText_99)
# 10

unique(metric_comparison[metric_comparison$MatthewsCorrelationText %in% MatthewsCorrelationText_99,]$Metric)
# [1] "cosineFourGramNormalizedBool"                    
# [2] "cosineFourGramNormalizedNormalizedTermFrequency" 
# [3] "cosineTokenNormalizedBool"                       
# [4] "cosineThreeGramNormalizedNormalizedTermFrequency"
# [5] "cosineThreeGramNormalizedBool"                   
# [6] "cosineTwoShingleNormalizedBool"                  
# [7] "cosineFiveGramNormalizedBool"                    
# [8] "cosineTokenNormalizedNormalizedTermFrequency"   


### CODE ###

MatthewsCorrelationCode <- metric_comparison$MatthewsCorrelationCode

summary(MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.8582  0.9577  0.9712  0.9625  0.9781  0.9830

boxplot(MatthewsCorrelationCode)

MatthewsCorrelationCode_99 <- MatthewsCorrelationCode[MatthewsCorrelationCode >= quantile(MatthewsCorrelationCode, 0.99)]

length(MatthewsCorrelationCode_99)
# 10

unique(metric_comparison[metric_comparison$MatthewsCorrelationCode %in% MatthewsCorrelationCode_99,]$Metric)
# [1] "cosineFiveGramNormalizedNormalizedTermFrequency"
# [2] "winnowingFourGramDice"                          
# [3] "cosineFourGramNormalizedBool"                   
# [4] "manhattanTokenNormalized"                       
# [5] "fourGramDiceNormalizedPadding"                  
# [6] "winnowingFiveGramDice"                          
# [7] "fiveGramDiceNormalizedPadding"                  
# [8] "cosineTokenNormalizedBool"                      
# [9] "winnowingThreeGramDice"                         
# [10] "cosineTokenNormalizedNormalizedTermFrequency"  


### BOTH ###

boxplot(MatthewsCorrelationText, MatthewsCorrelationCode)
