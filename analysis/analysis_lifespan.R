setwd("F:/Git/github/r-scripts/analysis/") # please update path
#setwd("/Users/sebastian/git/github/r-scripts/analysis/")

library(data.table)

# use defined colors
source("colors.R")

# length of all text block lifespans
textblock_lifespan_length <- fread("data/textblock_lifespan_length.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(textblock_lifespan_length) <- c("PostId", "PostTypeId", "RootPostBlockId", "LifespanLength")

# length of all code block lifespans
codeblock_lifespan_length <- fread("data/codeblock_lifespan_length.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(codeblock_lifespan_length) <- c("PostId", "PostTypeId", "RootPostBlockId", "LifespanLength")


summary(textblock_lifespan_length$LifespanLength)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#1.00     1.00     1.00     2.69     3.00 80660.00 

# questions
summary(textblock_lifespan_length$LifespanLength[textblock_lifespan_length$PostTypeId==1])
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 1.000     1.000     2.000     2.986     4.000 15880.000

# answers
summary(textblock_lifespan_length$LifespanLength[textblock_lifespan_length$PostTypeId==2])
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 1.00     1.00     1.00     2.43     2.00 80660.00 

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

library(plotrix)
#options(scipen=5) # prevent scientific notation
gap.barplot(
  table(LifespanLength),
  xlim=c(0.6, 10),
  gap=c(11000000, 39000000),
  xtics=seq(1, 6),
  ytics=c(0, 5000000, 10000000, 38000000),
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



# todo: continue here


LifespanLength <- ifelse(codeblock_lifespan_length$LifespanLength>6, 6, codeblock_lifespan_length$LifespanLength)

library(plotrix)
#options(scipen=5) # prevent scientific notation
gap.barplot(
  table(LifespanLength),
  xlim=c(0.6,6),
  gap=c(5000000, 37000000),
  xtics=seq(1, 6),
  ytics=c(0, 2000000, 4000000, 5000000, 38000000),
  col=rep(gray_lighter, 6),
  main="Length of Code Block Lifespans (n=43,728,155)",
  xlab="",
  ylab="",
  xaxt="n"
)
# axes
axis(1, at=seq(1, 6), labels=c(seq(1, 5), "\u2265 6"))
title(xlab="Length", font.lab=3)
title(ylab="Count", font.lab=3)


