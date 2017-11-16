############################################################################################
# read data
# TODO set relative path
setwd("C:\\Users\\Lorik\\Documents\\GitHub\\r-scripts\\metric_comparison_r_scripts") # Pfad bitte anpassen

library(data.table)
dataMTS <- fread("PostId_VersionCount_SO_17-06_sample_100_per_metricThresholdSample.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
dataMT <- fread("PostId_VersionCount_SO_17-06_sample_100_per_metricThreshold.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))

############################################################################################




############################################################################################
# data validation
nrow(dataMTS) # 7280 = (130 metrics) * (7 thresholds) * (8 samples)
nrow(dataMT) # 910 = (130 metrics) * (7 thresholds)
############################################################################################




############################################################################################
# preparing for scatterplot
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  text(horizontal, vertical, format(abs(cor(x,y)), digits=2))
}

# scatterplot
pairs(#main = "scatterplot matrix",
  pch = 21,
  las=1,
  upper.panel=panel.pearson,
  lower.panel=panel.smooth,
  dataMT$threshold
  +dataMT$numberOfTextPostsWithThisMetric
  +dataMT$runtimeTextTotal
  +dataMT$truePositivesText
  +dataMT$trueNegativesText
  +dataMT$falsePositivesText
  +dataMT$falseNegativesText
  +dataMT$numberOfCodePostsWithThisMetric
  +dataMT$runtimeCodeTotal
  +dataMT$truePositivesCode
  +dataMT$trueNegativesCode
  +dataMT$falsePositivesCode
  +dataMT$falseNegativesCode
)
############################################################################################




# sample;metric;threshold;

# numberOfTextPostsWithThisMetric;
# runtimeTextTotal;
# truePositivesText;trueNegativesText;falsePositivesText;falseNegativesText;

# numberOfCodePostsWithThisMetric;
# runtimeCodeTotal;
# truePositivesCode;trueNegativesCode;falsePositivesCode;falseNegativesCode

############################################################################################
# parallel coordinates
# Option 1
# https://plot.ly/r/parallel-coordinates-plot/

library(plotly)

parcoords_text <- plot_ly(
  type = 'parcoords',
  line = list(color = 'blue', colorscale = 'Jet',
              showscale = TRUE,
              reversescale = TRUE),
  
  dimensions = list(
    
    list(range = c(min(dataMT$threshold),max(dataMT$threshold)),
      label = 'threshold', values = dataMT$threshold),
    
    list(range = c(min(dataMT$numberOfTextPostsWithThisMetric),max(dataMT$numberOfTextPostsWithThisMetric)),
      label = 'number Of text posts computed with this metric', values = dataMT$numberOfTextPostsWithThisMetric),
    
    list(range = c(min(dataMT$runtimeTextTotal), max(dataMT$runtimeTextTotal)),
      label = 'runtime total per post', values = dataMT$runtimeTextTotal),
    
    list(range = c(min(dataMT$truePositivesText), max(dataMT$truePositivesText)),
      label = 'true positives text per post', values = dataMT$truePositivesText),
    
    list(range = c(min(dataMT$trueNegativesText), max(dataMT$trueNegativesText)),
      label = 'true negatives text per post', values = dataMT$trueNegativesText),
    
    list(range = c(min(dataMT$falsePositivesText), max(dataMT$falsePositivesText)),
      label = 'false positives text per post', values = dataMT$falsePositivesText),
    
    list(range = c(min(dataMT$falseNegativesText), max(dataMT$falseNegativesText),
      label = 'false negatives text per post', values = dataMT$falseNegativesText))
  )
)

parcoords_code <- plot_ly(
  type = 'parcoords',
  line = list(color = 'red', colorscale = 'Jet',
              showscale = TRUE,
              reversescale = TRUE),
  
  dimensions = list(
    
    list(range = c(min(dataMT$threshold),max(dataMT$threshold)),
         label = 'threshold', values = dataMT$threshold),
    
    list(range = c(min(dataMT$numberOfCodePostsWithThisMetric),max(dataMT$numberOfCodePostsWithThisMetric)),
         label = 'number Of code posts computed with this metric', values = dataMT$numberOfCodePostsWithThisMetric),
    
    list(range = c(min(dataMT$runtimeCodeTotal), max(dataMT$runtimeCodeTotal)),
         label = 'runtime total per post', values = dataMT$runtimeCodeTotal),
    
    list(range = c(min(dataMT$truePositivesCode), max(dataMT$truePositivesCode)),
         label = 'true positives code per post', values = dataMT$truePositivesCode),
    
    list(range = c(min(dataMT$trueNegativesCode), max(dataMT$trueNegativesCode)),
         label = 'true negatives code per post', values = dataMT$trueNegativesCode),
    
    list(range = c(min(dataMT$falsePositivesCode), max(dataMT$falsePositivesCode)),
         label = 'false positives code per post', values = dataMT$falsePositivesCode),
    
    list(range = c(min(dataMT$falseNegativesCode), max(dataMT$falseNegativesCode),
                   label = 'false negatives code per post', values = dataMT$falseNegativesCode))
  )
)

parcoords_text
parcoords_code
############################################################################################


# Parallele Koordinaten in R
# Option 2

# K-Means Cluster Analysis for 5 clusters
fit <- kmeans(dataMT$threshold, 5)
# append cluster assignment
cluster_dataMT <- data.frame(dataMT, fit$cluster)
colnames(cluster_dataMT)[12] <- "cluster"

# use parcoord from package MASS
library(MASS)
# draw parallel coordinates and color by cluster
parcoord(cluster_dataMT[,2:8], col = 1 + (0:149)%/%50, main="Parallel coordinates of data (5 colored clusters)")

#require(grDevices)
#stars(data)