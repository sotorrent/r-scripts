#setwd("E:/Git/github/r-scripts/sampling") # Pfad bitte anpassen
setwd("/Users/sebastian/git/github/r-scripts/sampling")

# use defined colors
source("../colors.R")

library(data.table)
#data <- fread("data/PostId_VersionCount_SO_17-06.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
data <- fread("data/PostId_VersionCount_SO_17-12.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
names(data) <- c("PostId", "PostTypeId", "VersionCount")

nrow(data)
## 36,062,267
# 39,554,826

summary(data$VersionCount)
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 1.000   1.000   1.000   1.564   2.000 754.000
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   1.000   1.572   2.000 754.000

boxplot(data$VersionCount, outline=FALSE)

length(data$VersionCount[data$VersionCount==1])/nrow(data)*100
## 64.05568
# 63.74939

length(data$VersionCount[data$VersionCount>1])
## 12,962,337
# 14,338,864

length(data$VersionCount[data$VersionCount>1])/nrow(data)*100
## 35.94432
# 36.25061

VersionCount <- ifelse(data$VersionCount>10, 10, data$VersionCount)

# plot histogram + boxplot
#quartz(type="pdf", file="figures/PostId_VersionCount_SO_17-06.pdf", width=12, height=8) # prevents unicode issues in pdf
quartz(type="pdf", file="figures/PostId_VersionCount_SO_17-12.pdf", width=12, height=8) # prevents unicode issues in pdf
#pdf("figures/PostId_VersionCount_SO_17-06.pdf", width=12, height=8)
par(
  bg="white",
  cex=1.3,
  cex.main=1.8,
  cex.sub=1.3,
  cex.lab=1.3,
  cex.axis=1.3
)

hist(VersionCount, 
     main="Version count of Stack Overflow Q&A (n=39,554,826)", 
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
hist(VersionCount,
     add=TRUE,
     main="VersionCount distribution", 
     freq=TRUE,
     xlab="PostId",
     ylab="VersionCount",
     border=gray_dark,
     col=c(gray_lighter, rep(gray_selected, 9)),
     #labels=c(rep("", 10), "Selected"),
     xlim=c(0,10),
     ylim=c(0, 25000000),
     breaks=c(0,1,2,3,4,5,6,7,8,9,10),
     xaxt="n",
     yaxt="n"
)
boxplot(VersionCount-0.5,
        add=TRUE,
        outline=FALSE,
        horizontal=TRUE,
        ylim=c(1,10),
        log="",
        col=gray_dark,
        # https://stackoverflow.com/a/28890111
        lwd=2,
        medlwd=2,
        #staplelty=0,
        whisklty=1,
        #staplelty=0,
        whiskcol="black",
        medcol="black",
        boxcol="black",
        staplecol="black",
        boxwex=2500000,
        axes=FALSE
        #xaxt="n"
        #yaxt="n"
)
# median
abline(v=0.5, lty=1, lwd=2, col="black")
# labels
text(3.8, 6000000, "Edited Posts (36.3%)", font=3, col="black", cex=1.3)
# axes
axis(1, at=seq(0.5, 9.5, by=1), labels=c(seq(1, 9, by=1), "\u2265 10"))
axis(2, at=seq(0, 25000000, by=5000000), labels=c("0", "5m", "10m", "15m", "20m", "25m"), las=2)

dev.off()
