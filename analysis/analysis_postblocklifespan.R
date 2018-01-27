#setwd("F:/Git/github/r-scripts/analysis/") # please update path
setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)
library(plotrix)

# use defined colors
source("../colors.R")

# length of all text block lifespans
textblock_lifespan_length <- fread("data/textblock_lifespan_length.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(textblock_lifespan_length) <- c("PostId", "PostTypeId", "RootPostBlockId", "LifespanLength")

# length of all code block lifespans
codeblock_lifespan_length <- fread("data/codeblock_lifespan_length.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(codeblock_lifespan_length) <- c("PostId", "PostTypeId", "RootPostBlockId", "LifespanLength")


##########
# text
##########
summary(textblock_lifespan_length$LifespanLength)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#1.00     1.00     1.00     2.69     3.00 80660.00 

n <- nrow(textblock_lifespan_length)
n_1 <- length(textblock_lifespan_length$LifespanLength[textblock_lifespan_length$LifespanLength==1])
n_2 <- length(textblock_lifespan_length$LifespanLength[textblock_lifespan_length$LifespanLength==2])
n
# 71,756,580
n_1
# 39,781,460
n_1/n*100
# 55.43946
n_2
# 10,974,663

TextLifespanLength <- ifelse(textblock_lifespan_length$LifespanLength>10, 10, textblock_lifespan_length$LifespanLength)
TextLifespanLengthTable <- table(TextLifespanLength)


##########
# code
##########
summary(codeblock_lifespan_length$LifespanLength)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 1.0      1.0      1.0      2.5      3.0 562500.0 

n <- nrow(codeblock_lifespan_length)
n_1 <- length(codeblock_lifespan_length$LifespanLength[codeblock_lifespan_length$LifespanLength==1])
n_2 <- length(codeblock_lifespan_length$LifespanLength[codeblock_lifespan_length$LifespanLength==2])
n
# 43,728,155
n_1
# 21,925,047
n_1/n*100
# 50.13943
n_2
# 9,532,092

CodeLifespanLength <- ifelse(codeblock_lifespan_length$LifespanLength>10, 10, codeblock_lifespan_length$LifespanLength)
CodeLifespanLengthTable <- table(CodeLifespanLength)


##########
# plot
##########

# plot histogram
#quartz(type="pdf", file="figures/exact_matches_so_filter_histograms.pdf", width=12, height=10) # prevents unicode issues in pdf
pdf("figures/postblocklifespan_length.pdf", width=18, height=6)
par(
  bg="white",
  #mar = c(3, 3, 3, 1)+0.1, # subplot margins (bottom, left, top, right)
  #  omi = c(0.0, 0.0, 0.0, 0.0),  # outer margins in inches (bottom, left, top, right)
  mfrow = c(1, 2),
  #pin = (width, height)
  #mfcol # draw in columns
  # increase font size
  cex=1.3,
  cex.main=1.3,
  cex.sub=1,
  cex.lab=1,
  cex.axis=1
)

# text
hist(TextLifespanLength, 
     main="Length of text block lifespans (n=71,756,580)", 
     freq=TRUE,
     xlab="",
     ylab="",
     border="white",
     col="white",
     #labels=c(rep("", 10), "Selected"),
     xlim=c(0, 10),
     ylim=c(0, 40000000),
     breaks=0:10,
     xaxt="n",
     yaxt="n"
)
for (y in seq(0, 40000000, by=10000000)) {
  segments(x0=-0.5, y0=y, x1=10, y1=y, lty=1, lwd=1, col=gray_lighter)
}
hist(TextLifespanLength,
     add=TRUE,
     main="", 
     freq=TRUE,
     xlab="x",
     ylab="y",
     border=gray_dark,
     col=gray_lighter,
     #labels=c(rep("", 10), "Selected"),
     xlim=c(0, 10),
     ylim=c(0, 40000000),
     breaks=0:10,
     xaxt="n",
     yaxt="n"
)
boxplot(TextLifespanLength-0.5,
        add=TRUE,
        outline=FALSE,
        horizontal=TRUE,
        ylim=c(0, 10),
        log="",
        col="white",
        # https://stackoverflow.com/a/28890111
        lwd=2,
        medlwd=2,
        #staplelty=0,
        whisklty=1,
        #staplelty=0,
        whiskcol=gray_darker,
        medcol=gray_darker,
        boxcol=gray_darker,
        staplecol=gray_darker,
        boxwex=3200000,
        axes=FALSE
        #xaxt="n"
        #yaxt="n"
)
# median
abline(v=0.5, lty=1, lwd=2, col=gray_darker)
# axes
axis(1, at=seq(-0.5, 9.5, by=1), labels=c(seq(0, 9, by=1), "\u2265 10"))
axis(2, at=seq(0, 40000000, by=10000000), labels=c("0", "10m", "20m", "30m", "40m"), las=2)


# code
hist(CodeLifespanLength, 
     main="Length of code block lifespans (n=43,728,155)", 
     freq=TRUE,
     xlab="",
     ylab="",
     border="white",
     col="white",
     #labels=c(rep("", 10), "Selected"),
     xlim=c(0, 10),
     ylim=c(0, 40000000),
     breaks=0:10,
     xaxt="n",
     yaxt="n"
)
for (y in seq(0, 40000000, by=10000000)) {
  segments(x0=-0.5, y0=y, x1=10, y1=y, lty=1, lwd=1, col=gray_lighter)
}
hist(CodeLifespanLength,
     add=TRUE,
     main="", 
     freq=TRUE,
     xlab="x",
     ylab="y",
     border=gray_dark,
     col=gray_lighter,
     #labels=c(rep("", 10), "Selected"),
     xlim=c(0, 10),
     ylim=c(0, 40000000),
     breaks=0:10,
     xaxt="n",
     yaxt="n"
)
boxplot(CodeLifespanLength-0.5,
        add=TRUE,
        outline=FALSE,
        horizontal=TRUE,
        ylim=c(0, 10),
        log="",
        col="white",
        # https://stackoverflow.com/a/28890111
        lwd=2,
        medlwd=2,
        #staplelty=0,
        whisklty=1,
        #staplelty=0,
        whiskcol=gray_darker,
        medcol=gray_darker,
        boxcol=gray_darker,
        staplecol=gray_darker,
        boxwex=3200000,
        axes=FALSE
        #xaxt="n"
        #yaxt="n"
)
# median
abline(v=0.5, lty=1, lwd=2, col=gray_darker) 
# axes
axis(1, at=seq(-0.5, 9.5, by=1), labels=c(seq(0, 9, by=1), "\u2265 10"))
axis(2, at=seq(0, 40000000, by=10000000), labels=c("0", "10m", "20m", "30m", "40m"), las=2)

dev.off()

