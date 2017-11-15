# TODO set relative path
setwd("C:\\Users\\Lorik\\Documents\\GitHub\\r-scripts\\metric_comparison_r_scripts") # Pfad bitte anpassen

library(data.table)
data <- fread("differences_between_runtimes_metric_comparisons.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))


names(data) <- c("sample", "metric", "threshold",
                 "runtimeTotalTextMinimum",
                 "runtimeTextTotalMaxDifference",
                 "runtimeTextTotalDeviationWithConstantBaseAndAverage",
                 "runtimeTextTotalDeviationWithMinAndMax",
                 "runtimeTotalCodeMinimum",
                 "runtimeCodeTotalMaxDifference",
                 "runtimeCodeTotalDeviationWithConstantBaseAndAverage",
                 "runtimeCodeTotalDeviationWithMinAndMax")




# divide lower values by the bigger
# runtimeTextTotalDeviationWithMinAndMax
min(data$runtimeTextTotalDeviationWithMinAndMax) # 0.0007774804
max(data$runtimeTextTotalDeviationWithMinAndMax) # 0.9999995
mean(data$runtimeTextTotalDeviationWithMinAndMax) # 0.1997921
median(data$runtimeTextTotalDeviationWithMinAndMax) # 0.09136631

boxplot(data$runtimeTextTotalDeviationWithMinAndMax)


# runtimeCodeTotalDeviationWithMinAndMax
min(data$runtimeCodeTotalDeviationWithMinAndMax) # 9.165189e-05
max(data$runtimeCodeTotalDeviationWithMinAndMax) # 1
mean(data$runtimeCodeTotalDeviationWithMinAndMax) # 0.2407295
median(data$runtimeCodeTotalDeviationWithMinAndMax) # 0.1170249

boxplot(data$runtimeCodeTotalDeviationWithMinAndMax)




hist(data$runtimeTextTotalDeviationWithMinAndMax, 
     main="divide lower values by the bigger text", 
     xlab="time correspondation", 
     ylab="number of metrics",
     col="azure3", 
     xlim=c(0,1),
     ylim=c(0,350000),
     las=1, 
     breaks=10, 
     prob = FALSE
)

hist(data$runtimeCodeTotalDeviationWithMinAndMax, 
     main="divide lower values by the bigger code", 
     xlab="time correspondation", 
     ylab="number of metrics",
     col="azure3", 
     xlim=c(0,1),
     ylim=c(0,350000),
     las=1, 
     breaks=10, 
     prob = FALSE
)
