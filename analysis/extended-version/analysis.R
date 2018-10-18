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
source("colors.R")

# load libraries
library(data.table)
library(hexbin)
library(plotrix)

# read data
clones <- fread("data/MostRecentPostBlockVersionNormalizedClonesHash.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
n <- nrow(clones)
n
# 113,499,709
n_text <- length(which(clones$PostBlockTypeId==1))
n_text
# 69,556,749
n_code <- length(which(clones$PostBlockTypeId==2))
n_code
# 43,942,960

summary(clones$ThreadCount)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 1.00      1.00      1.00      1.08      1.00 138225.00 

n_1 <- length(which(clones$ThreadCount==1))
n_1
# 111,811,052
n_1/n*100
# 98.51219

n_1_text <- length(which(clones$ThreadCount==1 & clones$PostBlockTypeId==1))
n_1_text/n_text*100
# 98.87957

n_1_code <- length(which(clones$ThreadCount==1 & clones$PostBlockTypeId==2))
n_1_code/n_code*100
# 97.93067

############
# DECISION # Exclude normalized post blocks with only one occurrence 
############

clones <- clones[clones$ThreadCount > 1,]
n <- nrow(clones)
n
# 1,688,657
clones_text <- clones[clones$PostBlockTypeId==1,]
n_text <- nrow(clones_text)
n_text
# 779,334
clones_code <- clones[clones$PostBlockTypeId==2,]
n_code <- nrow(clones_code)
n_code
# 909,323

summary(clones_text$AvgLineCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   1.000   1.248   1.080 221.000 

summary(clones_code$AvgLineCount)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 1.000    1.000    2.667    8.015    7.667 1656.000 

############
# DECISION # Focus on code blocks in the following, exclude code blocks with less than 6 LOC (Bellon et al. 2007)
############

clones_code <- clones_code[clones_code$AvgLineCount > 5,]
n_code <- nrow(clones_code)
n_code
# 294,693

summary(clones_code$AvgLineCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5.00    8.00   12.00   20.74   22.00 1656.00 

summary(clones_code$ThreadCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.000   2.000   2.000   2.522   2.000 694.000 

clones_code <- clones_code[order(-clones_code$ThreadCount, clones_code$AvgLineCount),]
clones_code[1:10]

max_line_count <- max(clones_code$AvgLineCount)
line_count <- ifelse(clones_code$AvgLineCount>50, 50, clones_code$AvgLineCount)

max_thread_count <- max(clones_code$ThreadCount)
thread_count <- ifelse(clones_code$ThreadCount>5, 5, clones_code$ThreadCount)

n_2 <- length(which(thread_count>2))
n_2
# 55,626
n_2/n_code*100
# 18.87591


#plot(thread_count, line_count)
bins = hexbin(clone_count, line_count)
plot(bins)


# extended histogram for line count
quartz(type="pdf", file="figures/code-clones_line-count.pdf", width=18, height=10) # prevents unicode issues in pdf
#pdf("figures/exact_matches_gh_filter_histograms.pdf", width=12, height=20)
par(
  bg="white",
  #mar = c(3, 1.8, 3, 1.5)+0.1, # subplot margins (bottom, left, top, right)
  #omi = c(0.2, 0.4, 0.2, 0.0),  # outer margins in inches (bottom, left, top, right)
  #mfrow = c(4, 1),
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

# histogram (background)
hist(line_count,
     main="Line count of non-trivial code blocks with at least one clone (n=294,693)", 
     freq=TRUE,
     xlab="",
     ylab="",
     border="white",
     col="white",
     #labels=c(rep("", 10), "Selected"),
     xlim=c(5,50),
     ylim=c(0, 35000),
     breaks=seq(5, 55, by=1),
     xaxt="n",
     yaxt="n"
)
# grid
for (y in seq(0, 35000, by=5000)) {
  segments(x0=-5, y0=y, x1=50, y1=y, lty=1, lwd=1, col=gray_lighter)
}
# histogram (foreground)
hist(line_count,
     add=TRUE,
     main="", 
     freq=TRUE,
     xlab="",
     ylab="",
     border=gray_darker,
     #col=c(gray_lighter, rep(gray_selected, 11)),
     col=gray_lighter,
     #labels=c(rep("", 10), "Selected"),
     xlim=c(5,50),
     ylim=c(0, 35000),
     breaks=seq(5, 55, by=1),
     xaxt="n",
     yaxt="n"
)
# median
median(line_count)
# 12
segments(x0=median(line_count), y0=0, x1=median(line_count), y1=35000, lty=2, lwd=2, col=gray_dark)
text(median(line_count)+2.3, 32500, "\u2190 median (12)", font=3)
#boxplot
boxplot(line_count,
        add=TRUE,
        outline=FALSE,
        horizontal=TRUE,
        ylim=c(0, 35000),
        log="",
        col="white",
        # https://stackoverflow.com/a/28890111
        lwd=3,
        medlwd=3,
        #staplelty=0,
        whisklty=1,
        #staplelty=0,
        whiskcol="black",
        medcol="black",
        boxcol="black",
        staplecol="black",
        boxwex=2000,
        axes=FALSE
        #xaxt="n"
        #yaxt="n"
)
# filter
#abline(v=1, lty=2, lwd=2, col=gray_darker) 
# labels
#text(2.5, 82620, "Excluded", font=3)
# axes
axis(1, at=seq(0, 50, by=5), labels=c(seq(0, 45, by=5), "\u2265 50"))
axis(2, at=seq(0, 35000, by=5000), labels=c("0", "5k", "10k", "15k", "20k", "25k", "30k", "35k"), las=2)
title(xlab="Line count", font.lab=3)
title(ylab="Number of code blocks", font.lab=3)

par(mfrow = c(1, 1))
dev.off() 


# histogram for thread count
quartz(type="pdf", file="figures/code-clones_thread-count.pdf", width=12, height=10) # prevents unicode issues in pdf
#pdf("figures/exact_matches_gh_filter_histograms.pdf", width=12, height=20)
par(
  bg="white",
  #mar = c(3, 1.8, 3, 1.5)+0.1, # subplot margins (bottom, left, top, right)
  #omi = c(0.2, 0.4, 0.2, 0.0),  # outer margins in inches (bottom, left, top, right)
  #mfrow = c(4, 1),
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

options(scipen=5) # prevent scientific notation
gap.barplot(
  table(thread_count),
  xlim=c(1,6),
  gap=c(80000, 190000),
  xtics=seq(2, 5),
  ytics=c(0, 25000, 50000, 200000),
  col=c(gray_lighter, rep(gray_selected, 4)),
  main="Number non-trivial code blocks being present in multiple threads (n=294,693)",
  xlab="",
  ylab="",
  xaxt="n"
)
# labels
text(4, 20000, "18.9%", font=2)
# axes
axis(1, at=seq(2, 5, by=1), labels=c(seq(2, 4, by=1), "\u2265 5"))
title(xlab="Number of threads", font.lab=3)
title(ylab="Number of code blocks", font.lab=3)

par(mfrow = c(1, 1))
dev.off() 




