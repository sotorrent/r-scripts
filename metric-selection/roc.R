#setwd("F:/Git/github/r-scripts/metric-selection") # Pfad bitte anpassen
setwd("/Users/sebastian/git/github/r-scripts/metric-selection")

library(data.table)
metric_comparison <- fread("MetricComparison_aggregated-selected.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)

nrow(metric_comparison)
# 2424


### TEXT ###

MatthewsCorrelationText <- metric_comparison$MatthewsCorrelationText

summary(MatthewsCorrelationText)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.7727  0.9331  0.9659  0.9454  0.9749  0.9794

boxplot(MatthewsCorrelationText)

MatthewsCorrelationText_max <- MatthewsCorrelationText[MatthewsCorrelationText == max(MatthewsCorrelationText)]
MatthewsCorrelationText_max
# 0.9794148

metric_text <- metric_comparison[metric_comparison$MatthewsCorrelationText == MatthewsCorrelationText_max,]

metric_text$Metric
# [1] "manhattanFourGramNormalized"
metric_text$Threshold
# 0.17

# best metric
roc_text <- metric_comparison[metric_comparison$Metric == "manhattanFourGramNormalized", c("Threshold", "RecallText", "InverseRecallText")]
roc_text$InverseRecallText <- 1 - roc_text$InverseRecallText
roc_text <- roc_text[with(roc_text, order(roc_text$Threshold)),]
names(roc_text) <- c("Threshold", "TPR", "FPR")
roc_text[roc_text$Threshold==0.17,]
# Threshold      TPR         FPR
# 1:      0.17 0.985957 0.005367202

# baseline metric
roc_text_equal <- metric_comparison[metric_comparison$Metric == "equal", c("Threshold", "RecallText", "InverseRecallText")]
roc_text_equal$InverseRecallText <- 1 - roc_text_equal$InverseRecallText
roc_text_equal <- roc_text_equal[with(roc_text_equal, order(roc_text_equal$Threshold)),]
names(roc_text_equal) <- c("Threshold", "TPR", "FPR")

# plot
plot(roc_text$FPR, roc_text$TPR,
     main="manhattanFourGramNormalized (Text)", xlab="False positive rate", ylab="True positive rate",
     pch=1
)
segments(x0=roc_text[roc_text$Threshold==0.17,]$FPR, y0=0, x1=roc_text[roc_text$Threshold==0.17,]$FPR, y1=roc_text[roc_text$Threshold==0.17,]$TPR, lty=2, lwd=1)
segments(x0=-0.1, y0=roc_text[roc_text$Threshold==0.17,]$TPR, x1=roc_text[roc_text$Threshold==0.17,]$FPR, y1=roc_text[roc_text$Threshold==0.17,]$TPR, lty=2, lwd=1)
points(roc_text[roc_text$Threshold==0.17,]$FPR, roc_text[roc_text$Threshold==0.17,]$TPR, pch=16)
text(0.0074, 0.9, "\u2190 theshold: 0.17 (TPR=0.9860, FPR=0.0054)", font=3)

# TODO: add points roc_text_equal


### CODE ###

MatthewsCorrelationCode <- metric_comparison$MatthewsCorrelationCode

summary(MatthewsCorrelationCode)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.8651  0.9649  0.9779  0.9678  0.9817  0.9850 

boxplot(MatthewsCorrelationCode)

MatthewsCorrelationCode_max <- MatthewsCorrelationCode[MatthewsCorrelationCode == max(MatthewsCorrelationCode)]
MatthewsCorrelationCode_max
# 0.9850383

metric_code <- metric_comparison[metric_comparison$MatthewsCorrelationCode == MatthewsCorrelationCode_max,]

metric_code$Metric
# [1] "fiveGramDiceNormalizedPadding"
metric_code$Threshold
# 0.26

roc_code <- metric_comparison[metric_comparison$Metric == "fiveGramDiceNormalizedPadding", c("Threshold", "RecallText", "InverseRecallText")]
roc_code$InverseRecallText <- 1 - roc_code$InverseRecallText
roc_code <- roc_code[with(roc_code, order(roc_code$Threshold)),]
names(roc_code) <- c("Threshold", "TPR", "FPR")
roc_code[roc_code$Threshold==0.26,]
# Threshold      TPR         FPR
# 1:      0.17 0.985957 0.005367202

plot(roc_code$FPR, roc_code$TPR,
     main="fiveGramDiceNormalizedPadding (Code)", xlab="False positive rate", ylab="True positive rate",
     pch=1, xaxt="n", yaxt="n"
)
axis(1, at=seq(0, 0.01, by=0.001), labels=seq(0, 0.01, by=0.001))
axis(2, at=seq(0.7, 1.0, by=0.05), labels=seq(0.7, 1.0, by=0.05), las=2)
segments(x0=roc_code[roc_code$Threshold==0.26,]$FPR, y0=0, x1=roc_code[roc_code$Threshold==0.26,]$FPR, y1=roc_code[roc_code$Threshold==0.26,]$TPR, lty=2, lwd=1)
segments(x0=-0.1, y0=roc_code[roc_code$Threshold==0.26,]$TPR, x1=roc_code[roc_code$Threshold==0.26,]$FPR, y1=roc_code[roc_code$Threshold==0.26,]$TPR, lty=2, lwd=1)
points(roc_code[roc_code$Threshold==0.26,]$FPR, roc_code[roc_code$Threshold==0.26,]$TPR, pch=16)
text(0.0074, 0.9, "\u2190 theshold: 0.26 (TPR=0.9860, FPR=0.0054)", font=3)

