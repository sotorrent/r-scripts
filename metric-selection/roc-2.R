# set working directory (see https://stackoverflow.com/a/35842119)
dir = tryCatch({
  # script being sourced
  getSrcDirectory()[1]
}, error = function(e) {
  # script being run in RStudio
  dirname(rstudioapi::getActiveDocumentContext()$path)
})
setwd(dir)

# use defined colors
source("../colors.R")

# load functions
source("functions.R")

library(data.table)
library(plotrix)

ITERATION <- 2

### read results from second run with selected metrics ###

read_metrics_evaluation_per_sample(ITERATION, "selected")
merge_and_matthews_correlation()

# calculate recall and inverse recall
sample_candidates <- add_recall(sample_candidates)


### TEXT ###

# best metric

# see metric-comparison-new.R
# metric: "fiveGramDice"
# threshold: 0.04
roc_text <- sample_candidates[sample_candidates$MetricText == "fiveGramDice", c("ThresholdText", "RecallText", "InverseRecallText")]
roc_text$InverseRecallText <- 1 - roc_text$InverseRecallText
roc_text <- roc_text[with(roc_text, order(roc_text$Threshold)),]
names(roc_text) <- c("Threshold", "TPR", "FPR")
roc_text_selected <- roc_text[roc_text$Threshold==0.04,]
roc_text_selected
#    Threshold       TPR      FPR
# 1:      0.04 0.9935691 0.148532

# baseline metric
roc_text_equal <- sample_candidates[sample_candidates$MetricText == "equal", c("ThresholdText", "RecallText", "InverseRecallText")]
roc_text_equal$InverseRecallText <- 1 - roc_text_equal$InverseRecallText
roc_text_equal <- roc_text_equal[with(roc_text_equal, order(roc_text_equal$Threshold)),]
names(roc_text_equal) <- c("Threshold", "TPR", "FPR")
roc_text_equal[roc_text_equal$Threshold==1.0,]
#    Threshold       TPR FPR
# 1:         1 0.6375134   0

# sort for correct painting order
setorderv(roc_text, c("Threshold"), c(1))
setorderv(roc_text_selected, c("Threshold"), c(1))
setorderv(roc_text_equal, c("Threshold"), c(1))


### CODE ###

# best metric

# see metric-comparison-new.R
# metric: "tokenDiceNormalized"
# threshold: 0.1
roc_code <- sample_candidates[sample_candidates$MetricCode == "tokenDiceNormalized", c("ThresholdCode", "RecallCode", "InverseRecallCode")]
roc_code$InverseRecallCode <- 1 - roc_code$InverseRecallCode
roc_code <- roc_code[with(roc_code, order(roc_code$Threshold)),]
names(roc_code) <- c("Threshold", "TPR", "FPR")
roc_code_selected <- roc_code[roc_code$Threshold==0.1,]
roc_code_selected
#    Threshold       TPR       FPR
# 1:       0.1 0.9971123 0.0433145

# baseline metric
roc_code_equal <- sample_candidates[sample_candidates$MetricCode == "equal", c("ThresholdCode", "RecallCode", "InverseRecallCode")]
roc_code_equal$InverseRecallCode <- 1 - roc_code_equal$InverseRecallCode
roc_code_equal <- roc_code_equal[with(roc_code_equal, order(roc_code_equal$Threshold)),]
names(roc_code_equal) <- c("Threshold", "TPR", "FPR")
roc_code_equal[roc_code_equal$Threshold==1.0,]
#    Threshold       TPR FPR
# 1:         1 0.7669651   0

# sort for correct painting order
setorderv(roc_code, c("Threshold"), c(1))
setorderv(roc_code_selected, c("Threshold"), c(1))
setorderv(roc_code_equal, c("Threshold"), c(1))


### PLOT ###

color_scale <- color.scale(seq(0.0, 1.0, by=0.01), extremes=c("gray20", "gray70"))

# plot ROC curve (complete)

pdf("figures/roc_complete_new.pdf", width=14, height=5)
par(
  bg="white",
  #mar = c(3, 1.8, 3, 1.5)+0.1, # subplot margins (bottom, left, top, right)
  #omi = c(0.2, 0.4, 0.2, 0.0),  # outer margins in inches (bottom, left, top, right)
  mfrow = c(1, 2),
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
     main="fiveGramDice (Text)", xlab="False positive rate", ylab="True positive rate",
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
# baseline metric
segments(x0=min(roc_text_equal$FPR), y0=min(roc_text_equal$TPR), x1=max(roc_text_equal$FPR), y1=max(roc_text_equal$TPR),
         lty=1, lwd=1, col="gray70")
points(roc_text_equal$FPR, roc_text_equal$TPR,
       pch=20, col=color_scale)
text(0.21, 0.79, "equal", font=3, cex=1)
# selected threshold
segments(x0=roc_text_selected$FPR, y0=0, x1=roc_text_selected$FPR, y1=roc_text_selected$TPR,
         lty=2, lwd=1, gray_darker)
segments(x0=-0.1, y0=roc_text_selected$TPR, x1=roc_text_selected$FPR, y1=roc_text_selected$TPR,
         lty=2, lwd=1, gray_darker)
points(roc_text_selected$FPR, roc_text_selected$TPR, pch=16)
# legend
legend(0.75,0.3,
       c(expression(paste(theta1, " = 0.0")), expression(paste(theta1, " = 1.0"))),
       c("gray20", "gray70"),
       bty="n", border="white")

# code
plot(roc_code$FPR, roc_code$TPR,
     main="tokenDiceNormalized (Code)", xlab="False positive rate", ylab="True positive rate",
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
# baseline metric
segments(x0=min(roc_code_equal$FPR), y0=min(roc_code_equal$TPR), x1=max(roc_code_equal$FPR), y1=max(roc_code_equal$TPR),
         lty=1, lwd=1, col="gray70")
points(roc_code_equal$FPR, roc_code_equal$TPR,
       pch=20, col=color_scale)
text(0.14, 0.86, "equal", font=3, cex=1)
# selected threshold
segments(x0=roc_code_selected$FPR, y0=0, x1=roc_code_selected$FPR, y1=roc_code_selected$TPR,
         lty=2, lwd=1, gray_darker)
segments(x0=-0.1, y0=roc_code_selected$TPR, x1=roc_code_selected$FPR, y1=roc_code_selected$TPR,
         lty=2, lwd=1, gray_darker)
points(roc_code_selected$FPR, roc_code_selected$TPR, pch=16)
# legend
legend(0.75,0.3,
       c(expression(paste(theta1, " = 0.0")), expression(paste(theta1, " = 1.0"))),
       c("gray20", "gray70"),
       bty="n", border="white")

par(mfrow = c(1, 1))
dev.off() 


# plot ROC curve (excerpt)

pdf("figures/roc_excerpt_new.pdf", width=14, height=5)
par(
  #bg=NA,
  bg="white",
  #mar = c(3, 1.8, 3, 1.5)+0.1, # subplot margins (bottom, left, top, right)
  #omi = c(0.2, 0.4, 0.2, 0.0),  # outer margins in inches (bottom, left, top, right)
  mfrow = c(1, 2),
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
     main="fiveGramDice (Text)", xlab="False positive rate", ylab="True positive rate",
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
         lty=1, lwd=1, col="gray70")
points(roc_text_equal$FPR, roc_text_equal$TPR,
       pch=20, col=color_scale)
text(0.2, 0.795, "equal", font=3, cex=1)
# selected threshold
segments(x0=roc_text_selected$FPR, y0=0, x1=roc_text_selected$FPR, y1=roc_text_selected$TPR,
         lty=2, lwd=1, gray_darker)
segments(x0=-0.1, y0=roc_text_selected$TPR, x1=roc_text_selected$FPR, y1=roc_text_selected$TPR,
         lty=2, lwd=1, gray_darker)
points(roc_text_selected$FPR, roc_text_selected$TPR, pch=16)
# legend
legend(0.26,0.75,
       c(expression(paste(theta1, " = 0.0")), expression(paste(theta1, " = 1.0"))),
       c("gray20", "gray70"),
       bty="n", border="white")


# code
plot(roc_code$FPR, roc_code$TPR,
     main="tokenDiceNormalized (Code)", xlab="False positive rate", ylab="True positive rate",
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
         lty=1, lwd=1, col="gray70")
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
legend(0.26,0.745,
       c(expression(paste(theta1, " = 0.0")), expression(paste(theta1, " = 1.0"))),
       c("gray20", "gray70"),
       bty="n", border="white")


par(mfrow = c(1, 1))
dev.off() 


# both in one plot

color_scale_red <- color.scale(seq(0.0, 1.0, by=0.01), extremes=c("red4", "peachpuff"))
color_scale_blue <- color.scale(seq(0.0, 1.0, by=0.01), extremes=c("royalblue4", "lightskyblue1"))

pdf("figures/roc_excerpt_combined.pdf", width=8, height=8)
par(
  #bg=NA,
  bg="white",
  #mar = c(3, 1.8, 3, 1.5)+0.1, # subplot margins (bottom, left, top, right)
  #omi = c(0.2, 0.4, 0.2, 0.0),  # outer margins in inches (bottom, left, top, right)
  mfrow = c(1, 1),
  #pin = (width, height)
  # mfcol # draw in columns
  # increase font size
  cex=1.3,
  cex.main=1.3,
  cex.sub=1,
  cex.lab=1,
  cex.axis=1
)

# text
plot(roc_text$FPR, roc_text$TPR,
     main="Performance of selected metrics", xlab="False positive rate", ylab="True positive rate",
     pch=19,
     xlim=c(0.0, 0.35), ylim=c(0.65, 1.0),
     xaxt="n", yaxt="n",
     col=color_scale_blue
)

# code
points(roc_code$FPR, roc_code$TPR,
     pch=19,
     xlim=c(0.0, 0.35), ylim=c(0.65, 1.0),
     xaxt="n", yaxt="n",
     col=color_scale_red
)

# axes
axis(1, at=seq(0, 0.35, by=0.05), labels=seq(0, 0.35, by=0.05))
axis(2, at=seq(0.65, 1.0, by=0.05), labels=seq(0.65, 1.0, by=0.05), las=2)

# text
# baseline metric
segments(x0=min(roc_text_equal$FPR), y0=min(roc_text_equal$TPR), x1=max(roc_text_equal$FPR), y1=max(roc_text_equal$TPR),
         lty=1, lwd=2, col=color_scale_blue)
points(roc_text_equal$FPR, roc_text_equal$TPR,
       pch=20, col=color_scale_blue)
text(0.2, 0.795, expression(equal^text), font=4, cex=1, col="royalblue4")
# selected threshold
segments(x0=roc_text_selected$FPR, y0=0, x1=roc_text_selected$FPR, y1=roc_text_selected$TPR,
         lty=2, lwd=1, gray_darker)
segments(x0=-0.1, y0=roc_text_selected$TPR, x1=roc_text_selected$FPR, y1=roc_text_selected$TPR,
         lty=2, lwd=1, gray_darker)
points(roc_text_selected$FPR, roc_text_selected$TPR, pch=16)

# code
# baseline metric
segments(x0=min(roc_code_equal$FPR), y0=min(roc_code_equal$TPR), x1=max(roc_code_equal$FPR), y1=max(roc_code_equal$TPR),
         lty=1, lwd=2, col=color_scale_red)
points(roc_code_equal$FPR, roc_code_equal$TPR,
       pch=20, col=color_scale_red)
text(0.1095, 0.845, expression(equal^code), font=4, cex=1, col="red4")
# selected threshold
segments(x0=roc_code_selected$FPR, y0=0, x1=roc_code_selected$FPR, y1=roc_code_selected$TPR,
         lty=2, lwd=1, gray_darker)
segments(x0=-0.1, y0=roc_code_selected$TPR, x1=roc_code_selected$FPR, y1=roc_code_selected$TPR,
         lty=2, lwd=1, gray_darker)
points(roc_code_selected$FPR, roc_code_selected$TPR, pch=16)

# legend code
legend(0.26,0.75,
       c(expression(paste(theta1^code, " = 0.0")), expression(paste(theta1^code, " = 1.0"))),
       c("red4", "peachpuff"),
       bty="n", border="white")

# legend text
legend(0.26,0.7,
       c(expression(paste(theta1^text, " = 0.0")), expression(paste(theta1^text, " = 1.0"))),
       c("royalblue4", "lightskyblue1"),
       bty="n", border="white")

par(mfrow = c(1, 1))
dev.off() 
