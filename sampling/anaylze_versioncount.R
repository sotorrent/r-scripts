setwd("F:/Git/github/r-scripts/sampling") # Pfad bitte anpassen
#setwd("/Users/sebastian/git/github/r-scripts/sampling")

library(data.table)
data <- fread("data/PostId_VersionCount_SO_17-06.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
names(data) <- c("PostId", "PostTypeId", "VersionCount")

nrow(data)
# 36,062,267

summary(data$VersionCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   1.000   1.564   2.000 754.000

boxplot(data$VersionCount, outline=FALSE)

length(data$VersionCount[data$VersionCount>10])
# 17,931 (0.05%)

data_filtered <- data[data$VersionCount<=10]

boxplot(data_filtered$VersionCount)

# colors
library(grDevices)
gray_transparent <- adjustcolor("gray60", alpha.f=0.5)
gray_lighter <- adjustcolor("gray85", alpha.f=1)
gray_light <- adjustcolor("gray60", alpha.f=1)
gray_dark <- adjustcolor("gray34", alpha.f=1)
gray_darker <- adjustcolor("gray24", alpha.f=1)
blue_light <- adjustcolor("skyblue1", alpha.f=1)
blue_dark <- adjustcolor("steelblue4", alpha.f=1)
red_light <- adjustcolor("salmon", alpha.f=1)
red_dark <- adjustcolor("firebrick3", alpha.f=1)

# plot histogram + boxplot
pdf("figures/PostId_VersionCount_SO_17-06.pdf", width=12, height=8)
par(
  bg="white",
  cex=1.3,
  cex.main=1.8,
  cex.sub=1.3,
  cex.lab=1.3,
  cex.axis=1.3
)

# histogram + boxplot
hist(data_filtered$VersionCount, 
     main="Version count of Stack Overflow Q&A (n=36,062,267)", 
     freq=TRUE,
     xlab="",
     ylab="",
     border="white",
     col="white",
     #labels=c(rep("", 10), "Selected"),
     xlim=c(0,10),
     ylim=c(0, 25000000),
     breaks=c(0,1,2,3,4,5,6,7,8,9,10),
     xaxt="n",
     yaxt="n"
)
for (y in seq(0, 25000000, by=5000000)) {
  segments(x0=-0.5, y0=y, x1=10, y1=y, lty=1, lwd=1, col=gray_lighter)
}
hist(data_filtered$VersionCount,
     add=TRUE,
     main="VersionCount distribution", 
     freq=TRUE,
     xlab="PostId",
     ylab="VersionCount",
     border=gray_dark,
     col=gray_lighter,
     #labels=c(rep("", 10), "Selected"),
     xlim=c(0,10),
     ylim=c(0, 25000000),
     breaks=c(0,1,2,3,4,5,6,7,8,9,10),
     xaxt="n",
     yaxt="n"
)
boxplot(data_filtered$VersionCount-0.5,
        add=TRUE,
        outline=FALSE,
        horizontal=TRUE,
        ylim=c(1,10),
        log="",
        col="white",
        # https://stackoverflow.com/a/28890111
        lwd=3,
        medlwd=3,
        #staplelty=0,
        whisklty=1,
        #staplelty=0,
        whiskcol=gray_darker,
        medcol=gray_darker,
        boxcol=gray_darker,
        staplecol=gray_darker,
        boxwex=2400000,
        axes=FALSE
        #xaxt="n"
        #yaxt="n"
)
axis(1, at=seq(0.5, 9.5, by=1), labels=seq(1, 10, by=1))
axis(2, at=seq(0, 25000000, by=5000000), labels=c("0", "5m", "10m", "15m", "20m", "25m"), las=2)

dev.off()
