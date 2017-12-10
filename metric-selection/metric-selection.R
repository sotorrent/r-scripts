#setwd("F:/Git/github/r-scripts/metric-selection") # Pfad bitte anpassen
#setwd("/Users/sebastian/git/github/r-scripts/metric-selection")
setwd("C://Users//Lorik//Documents//GitHub//r-scripts//metric-selection")


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



############################################################################################
# preparing for scatterplot
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  text(horizontal, vertical, format(abs(cor(x,y)), digits=2))
}



# scatterplot for text
pairs(main = "scatterplot matrix (text)",
  pch = 21,
  las=1,
  cex.panels = 3,
  cex.labels=1.5,
  upper.panel=panel.pearson,
  lower.panel=panel.smooth,
  
  ~Threshold

  +InformednessText
  +MarkednessText
  +MatthewsCorrelationText
  +FScoreText
  
  +TruePositivesText
  +TrueNegativesText
  +FalsePositivesText
  +FalseNegativesText
  ,
  data = metric_comparison)


# scatterplot for code
pairs(main = "scatterplot matrix (code)",
      pch = 21,
      las=1,
      cex.panels = 3,
      cex.labels=1.5,
      upper.panel=panel.pearson,
      lower.panel=panel.smooth,
      
      ~Threshold
      
      +InformednessCode
      +MarkednessCode
      +MatthewsCorrelationCode
      +FScoreCode
      
      +TruePositivesCode
      +TrueNegativesCode
      +FalsePositivesCode
      +FalseNegativesCode
      ,
      data = metric_comparison)
############################################################################################


data_filtered <- metric_comparison[metric_comparison$Threshold < 1 & metric_comparison$MetricType != 'EQUAL']