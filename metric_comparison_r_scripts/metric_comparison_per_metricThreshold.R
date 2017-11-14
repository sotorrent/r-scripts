# TODO set relative path
setwd("C:\\Users\\Lorik\\Documents\\GitHub\\r-scripts\\metric_comparison_r_scripts") # Pfad bitte anpassen

library(data.table)
data <- fread("PostId_VersionCount_SO_17-06_sample_100_per_metricThreshold.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))

names(data) <- c("metric", "threshold",
                 "numberOfTextPostsWithThisMetric",
                 "runtimeTextTotal", "runtimeTextUser",
                 "truePositivesText", "trueNegativesText", "falsePositivesText","falseNegativesText",
                 "numberOfCodePostsWithThisMetric",
                 "runtimeCodeTotal", "runtimeCodeUser",
                 "truePositivesCode", "trueNegativesCode", "falsePositivesCode", "falseNegativesCode")

nrow(data) # 911


# Korrelationsmatrix der allgemeinen Variablen
selfassessments <- data.frame(data$threshold,
                              data$numberOfTextPostsWithThisMetric,
                              data$runtimeTextTotal, data$runtimeTextUser,
                              data$truePositivesText, data$trueNegativesText, data$falsePositivesText, data$falseNegativesText,
                              data$numberOfCodePostsWithThisMetric,
                              data$runtimeCodeTotal, data$runtimeCodeUser,
                              data$truePositivesCode, data$trueNegativesCode, data$falsePositivesCode, data$falseNegativesCode,
                              stringsAsFactors=FALSE) # zu untersuchende Variablen in Dataframe speichern

# Vorbereitende Maßnahmen für die Scatterplot-Matrizen
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



pairs(#main = "Scatterplot-Matrix für allgemeine Variablen",
  pch = 21,
  las=1,
  upper.panel=panel.pearson,
  lower.panel=panel.smooth,
  ~data$threshold
  +data$numberOfTextPostsWithThisMetric
  +data$runtimeTextTotal
  +data$runtimeTextUser
  +data$truePositivesText
  +data$trueNegativesText
  +data$falsePositivesText
  +data$falseNegativesText
#  +data$numberOfCodePostsWithThisMetric
#  +data$runtimeCodeTotal
#  +data$runtimeCodeUser
#  +data$truePositivesCode
#  +data$trueNegativesCode
#  +data$falsePositivesCode
#  +data$falseNegativesCode
)

