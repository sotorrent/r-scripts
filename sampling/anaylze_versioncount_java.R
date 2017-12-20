setwd("") # Pfad bitte anpassen

library(data.table)
data_java <- fread("PostId_VersionCount_SO_Java_17-06.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
names(data_java) <- c("PostId", "PostTypeId", "VersionCount")

nrow(data_java)
# 2,081,659

summary(data_java$VersionCount)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   1.000   1.000   1.799   2.000 126.000

boxplot(data_java$VersionCount, outline=FALSE)

length(data_java$VersionCount[data_java$VersionCount==1])/length(data_java$VersionCount)
# 0.5178206

length(data_java$VersionCount[data_java$VersionCount==2])/length(data_java$VersionCount)
# 0.2911351

length(data_java$VersionCount[data_java$VersionCount==2])/length(data_java$VersionCount[data_java$VersionCount>1])
# 0.6037901

length(data_java$VersionCount[data_java$VersionCount>10])
# 1,176 (0.06%)

data_java_filtered <- data_java[data_java$VersionCount<=10]

boxplot(data_java_filtered$VersionCount)

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

plot_histogram <- function(v) {
  # histogram + boxplot
  hist(v, 
       main="", 
       freq=TRUE,
       xlab="",
       ylab="",
       border="white",
       col="white",
       #labels=c(rep("", 10), "Selected"),
       xlim=c(0,10),
       ylim=c(0, 1500000),
       breaks=c(0,1,2,3,4,5,6,7,8,9,10),
       xaxt="n",
       yaxt="n"
  )
  for (y in seq(0, 1500000, by=500000)) {
    segments(x0=-0.5, y0=y, x1=10, y1=y, lty=1, lwd=1, col=gray_lighter)
  }
  hist(v,
       add=TRUE,
       main="VersionCount distribution", 
       freq=TRUE,
       xlab="PostId",
       ylab="VersionCount",
       border=gray_dark,
       col=gray_lighter,
       #labels=c(rep("", 10), "Selected"),
       xlim=c(0,10),
       ylim=c(0, 1500000),
       breaks=c(0,1,2,3,4,5,6,7,8,9,10),
       xaxt="n",
       yaxt="n"
  )
  boxplot(v-0.5,
          add=TRUE,
          outline=FALSE,
          horizontal=TRUE,
          ylim=c(1,10),
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
          boxwex=180000,
          axes=FALSE
          #xaxt="n"
          #yaxt="n"
  )
  axis(1, at=seq(0.5, 9.5, by=1), labels=seq(1, 10, by=1))
  axis(2, at=seq(0, 1500000, by=500000), labels=c("0", "500k", "1m", "1,5m"), las=2)
}

plot_histogram(data_java_filtered$VersionCount)

plot_histogram(data_java_filtered$VersionCount[data_java_filtered$VersionCount>1])
