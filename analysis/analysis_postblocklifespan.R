#setwd("F:/Git/github/r-scripts/analysis/") # please update path
setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)
library(plotrix)

# use defined colors
source("colors.R")

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

LifespanLength <- ifelse(textblock_lifespan_length$LifespanLength>10, 10, textblock_lifespan_length$LifespanLength)
LifespanLengthTable <- table(LifespanLength)

#options(scipen=5) # prevent scientific notation
gap.barplot(
  LifespanLengthTable,
  xlim=c(1, 10),
  gap=c(12000000, 37500000),
  xtics=seq(1, 10),
  ytics=c(0, 5000000, 10000000, 39000000),
  col=rep(gray_lighter, 10),
  main="Length of Text Block Lifespans (n=71,756,580)",
  xlab="",
  ylab="",
  xaxt="n"
)
# axes
axis(1, at=seq(1, 10), labels=c(seq(1, 9), "\u2265 10"))
title(xlab="Length", font.lab=3)
title(ylab="Count", font.lab=3)


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

LifespanLength <- ifelse(codeblock_lifespan_length$LifespanLength>10, 10, codeblock_lifespan_length$LifespanLength)
LifespanLengthTable <- table(LifespanLength)

#options(scipen=5) # prevent scientific notation
gap.barplot(
  LifespanLengthTable,
  xlim=c(1, 10),
  gap=c(10000000, 20000000),
  xtics=seq(1, 10),
  ytics=c(0, 4000000, 8000000, 21000000),
  col=rep(gray_lighter, 10),
  main="Length of Code Block Lifespans (n=43,728,155)",
  xlab="",
  ylab="",
  xaxt="n"
)
# axes
axis(1, at=seq(1, 10), labels=c(seq(1, 9), "\u2265 10"))
title(xlab="Length", font.lab=3)
title(ylab="Count", font.lab=3)


