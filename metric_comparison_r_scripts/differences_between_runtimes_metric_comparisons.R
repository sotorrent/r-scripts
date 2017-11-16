# TODO set relative path
setwd("C:\\Users\\Lorik\\Documents\\GitHub\\r-scripts\\metric_comparison_r_scripts") # Pfad bitte anpassen

library(data.table)
data <- fread("differences_between_runtimes_metric_comparisons.csv", header=TRUE, sep=";", quote="", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))




############################################################################################
# divide lower values by the bigger

# runtimeTextTotalDeviationWithMinAndMax
min(data$runtimeTextTotalDeviationWithMinAndMax) # 0.0007774804
max(data$runtimeTextTotalDeviationWithMinAndMax) # 0.9999995
mean(data$runtimeTextTotalDeviationWithMinAndMax) # 0.1997921
median(data$runtimeTextTotalDeviationWithMinAndMax) # 0.09136631


# runtimeCodeTotalDeviationWithMinAndMax
min(data$runtimeCodeTotalDeviationWithMinAndMax) # 9.165189e-05
max(data$runtimeCodeTotalDeviationWithMinAndMax) # 1
mean(data$runtimeCodeTotalDeviationWithMinAndMax) # 0.2407295
median(data$runtimeCodeTotalDeviationWithMinAndMax) # 0.1170249



# divide values of one sample by the values of the other sample
# runtimeTextTotalDeviationWithMinAndMax
min(data$runtimeTextTotalDeviationWithConstantBaseAndAverage) # 0.0003846759
max(data$runtimeTextTotalDeviationWithConstantBaseAndAverage) # 1694.944
mean(data$runtimeTextTotalDeviationWithConstantBaseAndAverage) # 1.338625
median(data$runtimeTextTotalDeviationWithConstantBaseAndAverage) # 1.220091


# runtimeCodeTotalDeviationWithMinAndMax
min(data$runtimeCodeTotalDeviationWithConstantBaseAndAverage) # 0.0002131644
max(data$runtimeCodeTotalDeviationWithConstantBaseAndAverage) # 5725.731
mean(data$runtimeCodeTotalDeviationWithConstantBaseAndAverage) # 0.8065627
median(data$runtimeCodeTotalDeviationWithConstantBaseAndAverage) # 0.8305956
############################################################################################





############################################################################################
# boxplots

boxplot(data$runtimeCodeTotalDeviationWithMinAndMax)
boxplot(data$runtimeTextTotalDeviationWithMinAndMax)

boxplot(data$runtimeTextTotalDeviationWithConstantBaseAndAverage)
boxplot(data$runtimeCodeTotalDeviationWithConstantBaseAndAverage)
############################################################################################




############################################################################################
# histograms

# histograms for runtimeTextTotalDeviationWithMinAndMax
hist(data$runtimeTextTotalDeviationWithMinAndMax, 
     main="divide lower values by the bigger text", 
     xlab="time correspondation", 
     ylab="number of tupels (sample, metric, threshold)",
     col="azure3", 
     ylim=c(0,250000),
     las=1, 
     prob = FALSE
)

hist(data$runtimeCodeTotalDeviationWithMinAndMax, 
     main="divide lower values by the bigger code", 
     xlab="time correspondation", 
     ylab="number of tupels (sample, metric, threshold)",
     col="azure3", 
     ylim=c(0,250000),
     las=1, 
     prob = FALSE
)


# histograms for runtimeTextTotalDeviationWithConstantBaseAndAverage
# splitting of results due to some outliers
runtimeTextTotalDeviationWithConstantBaseAndAverage_limitFactorLessEqual10 <- data[data$runtimeTextTotalDeviationWithConstantBaseAndAverage <= 10]
runtimeTextTotalDeviationWithConstantBaseAndAverage_limitFactorMoreThan10 <- data[data$runtimeTextTotalDeviationWithConstantBaseAndAverage > 10]

nrow(runtimeTextTotalDeviationWithConstantBaseAndAverage_limitFactorLessEqual10) # 636835
nrow(runtimeTextTotalDeviationWithConstantBaseAndAverage_limitFactorMoreThan10) #  165

hist(runtimeTextTotalDeviationWithConstantBaseAndAverage_limitFactorLessEqual10$runtimeTextTotalDeviationWithConstantBaseAndAverage, 
     main="divide values of one sample by the ones of the other sample text",
     xlab="time correspondation", 
     ylab="number of metrics",
     col="azure3",
     las = 1,
     xlim=c(0,5),
     ylim=c(0,600000)
)

hist(runtimeTextTotalDeviationWithConstantBaseAndAverage_limitFactorMoreThan10$runtimeTextTotalDeviationWithConstantBaseAndAverage, 
     main="divide values of one sample by the ones of the other sample text", 
     xlab="time correspondation", 
     ylab="number of metrics",
     col="azure3",
     las = 1,
     xlim=c(0,700),
     ylim=c(0,500)
)


runtimeCodeTotalDeviationWithConstantBaseAndAverage_limitFactorLessEqual10 <- data[data$runtimeCodeTotalDeviationWithConstantBaseAndAverage <= 10]
runtimeCodeTotalDeviationWithConstantBaseAndAverage_limitFactorMoreThan10 <- data[data$runtimeCodeTotalDeviationWithConstantBaseAndAverage > 10]

nrow(runtimeCodeTotalDeviationWithConstantBaseAndAverage_limitFactorLessEqual10) # 636835
nrow(runtimeCodeTotalDeviationWithConstantBaseAndAverage_limitFactorMoreThan10) #  165

hist(runtimeCodeTotalDeviationWithConstantBaseAndAverage_limitFactorLessEqual10$runtimeCodeTotalDeviationWithConstantBaseAndAverage, 
     main="divide values of one sample by the ones of the other sample code",
     xlab="time correspondation", 
     ylab="number of metrics",
     col="azure3",
     las = 1,
     xlim=c(0,5),
     ylim=c(0,600000)
)

hist(runtimeCodeTotalDeviationWithConstantBaseAndAverage_limitFactorMoreThan10$runtimeCodeTotalDeviationWithConstantBaseAndAverage, 
     main="divide values of one sample by the ones of the other sample code", 
     xlab="time correspondation", 
     ylab="number of metrics",
     col="azure3",
     las = 1,
     xlim=c(0,700),
     ylim=c(0,500)
)