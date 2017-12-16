setwd("F:/Git/github/r-scripts/metric-selection") # Pfad bitte anpassen
#setwd("/Users/sebastian/git/github/r-scripts/metric-selection")

# use defined colors
source("colors.R")

library(data.table)
metric_comparison <- fread("2017-12-15_samples_comparison-selected.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)

nrow(metric_comparison)
# 2525


### TEXT ###

# best metric

# see metric-selection_sebastian.R
# metric: "manhattanFourGramNormalized"
# threshold: 0.17
roc_text <- metric_comparison[metric_comparison$Metric == "manhattanFourGramNormalized", c("Threshold", "RecallText", "InverseRecallText")]
roc_text$InverseRecallText <- 1 - roc_text$InverseRecallText
roc_text <- roc_text[with(roc_text, order(roc_text$Threshold)),]
names(roc_text) <- c("Threshold", "TPR", "FPR")
roc_text_selected <- roc_text[roc_text$Threshold==0.17,]
roc_text_selected
# Threshold       TPR       FPR
# 1:      0.17 0.9864748 0.1302149

# baseline metric
roc_text_equal <- metric_comparison[metric_comparison$Metric == "equal", c("Threshold", "RecallText", "InverseRecallText")]
roc_text_equal$InverseRecallText <- 1 - roc_text_equal$InverseRecallText
roc_text_equal <- roc_text_equal[with(roc_text_equal, order(roc_text_equal$Threshold)),]
names(roc_text_equal) <- c("Threshold", "TPR", "FPR")
roc_text_equal[roc_text_equal$Threshold==1.0,]
# Threshold       TPR FPR
# 1:         1 0.6604821   0


### CODE ###

# best metric

# see metric-selection_sebastian.R
# metric: "winnowingFourGramOptimalAlignment"
# threshold: 0.25
roc_code <- metric_comparison[metric_comparison$Metric == "fiveGramDice", c("Threshold", "RecallCode", "InverseRecallCode")]
roc_code$InverseRecallCode <- 1 - roc_code$InverseRecallCode
roc_code <- roc_code[with(roc_code, order(roc_code$Threshold)),]
names(roc_code) <- c("Threshold", "TPR", "FPR")
roc_code_selected <- roc_code[roc_code$Threshold==0.25,]
roc_code_selected
# Threshold       TPR        FPR
# 1:      0.25 0.9887159 0.08264463

# baseline metric
roc_code_equal <- metric_comparison[metric_comparison$Metric == "equal", c("Threshold", "RecallCode", "InverseRecallCode")]
roc_code_equal$InverseRecallCode <- 1 - roc_code_equal$InverseRecallCode
roc_code_equal <- roc_code_equal[with(roc_code_equal, order(roc_code_equal$Threshold)),]
names(roc_code_equal) <- c("Threshold", "TPR", "FPR")
roc_code_equal[roc_code_equal$Threshold==1.0,]
# Threshold       TPR FPR
# 1:         1 0.7905665   0


### PLOT ###

library(plotrix)
color_scale <- color.scale(seq(0.0, 1.0, by=0.01), extremes=c("gray20", "gray60"))

# plot ROC curve (complete)

pdf("roc_complete.pdf", width=12, height=12)
par(
  bg="white",
  #mar = c(3, 1.8, 3, 1.5)+0.1, # subplot margins (bottom, left, top, right)
  #omi = c(0.2, 0.4, 0.2, 0.0),  # outer margins in inches (bottom, left, top, right)
  mfrow = c(2, 1),
  #pin = (width, height)
  # mfcol # draw in columns
  # increase font size
  cex=1.3,
  cex.main=1.3,
  cex.sub=1,
  cex.lab=1,
  cex.axis=1
)
#layout(matrix(c(1,2,3,4,5,5), 3, 2, byrow = TRUE))
#layout(4, 1)

# text
plot(roc_text$FPR, roc_text$TPR,
     main="manhattanFourGramNormalized (Text)", xlab="False positive rate", ylab="True positive rate",
     pch=19,
     xlim=c(0.0, 1.0), ylim=c(0.0, 1.0),
     xaxt="n", yaxt="n",
     col=color_scale
)
# axes
axis(1, at=seq(0, 1.0, by=0.1), labels=seq(0, 1.0, by=0.1))
axis(2, at=seq(0, 1.0, by=0.1), labels=seq(0, 1.0, by=0.1), las=2)
# diagonal
abline(0, 1, col=gray_dark)
# legend
legend(0.85,0.3,
       c(expression(paste(theta1, " = 0.0")), expression(paste(theta1, " = 1.0"))),
       c("gray20", "gray60"),
       bty="n")

# code
plot(roc_code$FPR, roc_code$TPR,
     main="winnowingFourGramOptimalAlignment (Code)", xlab="False positive rate", ylab="True positive rate",
     pch=19,
     xlim=c(0.0, 1.0), ylim=c(0.0, 1.0),
     xaxt="n", yaxt="n",
     col=color_scale
)
# axes
axis(1, at=seq(0, 1.0, by=0.1), labels=seq(0, 1.0, by=0.1))
axis(2, at=seq(0, 1.0, by=0.1), labels=seq(0, 1.0, by=0.1), las=2)
# diagonal
abline(0, 1, col=gray_dark)
# legend
legend(0.85,0.3,
       c(expression(paste(theta1, " = 0.0")), expression(paste(theta1, " = 1.0"))),
       c("gray20", "gray60"),
       bty="n")

par(mfrow = c(1, 1))
dev.off() 


# plot ROC curve (excerpt)

pdf("roc_excerpt.pdf", width=12, height=12)
par(
  bg="white",
  #mar = c(3, 1.8, 3, 1.5)+0.1, # subplot margins (bottom, left, top, right)
  #omi = c(0.2, 0.4, 0.2, 0.0),  # outer margins in inches (bottom, left, top, right)
  mfrow = c(2, 1),
  #pin = (width, height)
  # mfcol # draw in columns
  # increase font size
  cex=1.3,
  cex.main=1.3,
  cex.sub=1,
  cex.lab=1,
  cex.axis=1
)
#layout(matrix(c(1,2,3,4,5,5), 3, 2, byrow = TRUE))
#layout(4, 1)


# text
plot(roc_text$FPR, roc_text$TPR,
     main="manhattanFourGramNormalized (Text)", xlab="False positive rate", ylab="True positive rate",
     pch=19,
     xlim=c(0.0, 0.35), ylim=c(0.65, 1.0),
     xaxt="n", yaxt="n",
     col=color_scale
)
# axes
axis(1, at=seq(0, 0.35, by=0.05), labels=seq(0, 0.35, by=0.05))
axis(2, at=seq(0.65, 1.0, by=0.05), labels=seq(0.65, 1.0, by=0.05), las=2)
# baseline metric
segments(x0=min(roc_text_equal$FPR), y0=min(roc_text_equal$TPR), x1=max(roc_text_equal$FPR), y1=max(roc_text_equal$TPR),
         lty=1, lwd=1, col="gray60")
points(roc_text_equal$FPR, roc_text_equal$TPR,
       pch=20, col=color_scale)
text(0.175, 0.79, "equal", font=3, cex=1)
# selected threshold
segments(x0=roc_text_selected$FPR, y0=0, x1=roc_text_selected$FPR, y1=roc_text_selected$TPR,
         lty=2, lwd=1, gray_darker)
segments(x0=-0.1, y0=roc_text_selected$TPR, x1=roc_text_selected$FPR, y1=roc_text_selected$TPR,
         lty=2, lwd=1, gray_darker)
points(roc_text_selected$FPR, roc_text_selected$TPR, pch=16)
# legend
legend(0.3,0.75,
       c(expression(paste(theta1, " = 0.0")), expression(paste(theta1, " = 1.0"))),
       c("gray20", "gray60"),
       bty="n")


# code
plot(roc_code$FPR, roc_code$TPR,
     main="winnowingFourGramOptimalAlignment (Code)", xlab="False positive rate", ylab="True positive rate",
     pch=19,
     xlim=c(0.0, 0.35), ylim=c(0.65, 1.0),
     xaxt="n", yaxt="n",
     col=color_scale
)
# axes
axis(1, at=seq(0, 0.35, by=0.05), labels=seq(0, 0.35, by=0.05))
axis(2, at=seq(0.65, 1.0, by=0.05), labels=seq(0.65, 1.0, by=0.05), las=2)
# baseline metric
segments(x0=min(roc_code_equal$FPR), y0=min(roc_code_equal$TPR), x1=max(roc_code_equal$FPR), y1=max(roc_code_equal$TPR),
         lty=1, lwd=1, col="gray60")
points(roc_code_equal$FPR, roc_code_equal$TPR,
       pch=20, col=color_scale)
text(0.12, 0.86, "equal", font=3, cex=1)
# selected threshold
segments(x0=roc_code_selected$FPR, y0=0, x1=roc_code_selected$FPR, y1=roc_code_selected$TPR,
         lty=2, lwd=1, gray_darker)
segments(x0=-0.1, y0=roc_code_selected$TPR, x1=roc_code_selected$FPR, y1=roc_code_selected$TPR,
         lty=2, lwd=1, gray_darker)
points(roc_code_selected$FPR, roc_code_selected$TPR, pch=16)
# legend
legend(0.3,0.75,
       c(expression(paste(theta1, " = 0.0")), expression(paste(theta1, " = 1.0"))),
       c("gray20", "gray60"),
       bty="n")


par(mfrow = c(1, 1))
dev.off() 
