#setwd("F:/Git/github/r-scripts/metric-selection") # Pfad bitte anpassen
#setwd("/Users/sebastian/git/github/r-scripts/metric-selection")
setwd("C://Users//Lorik//Documents//GitHub//r-scripts//metric-selection")


library(data.table)
metric_comparison <- fread("MetricComparison_aggregated-all.csv", header=TRUE, sep=";", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)

nrow(metric_comparison)
# 1474


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



# scatterplot for text
pairs(main = "scatterplot matrix (text)",
  pch = 21,
  las=1,
  cex.panels = 3,
  cex.labels=1.5,
  upper.panel=panel.pearson,
  lower.panel=panel.smooth,
  
  ~Threshold

  +InformednessText
  +MarkednessText
  +MatthewsCorrelationText
  +FScoreText
  
  +TruePositivesText
  +TrueNegativesText
  +FalsePositivesText
  +FalseNegativesText
  ,
  data = metric_comparison)


# scatterplot for code
pairs(main = "scatterplot matrix (code)",
      pch = 21,
      las=1,
      cex.panels = 3,
      cex.labels=1.5,
      upper.panel=panel.pearson,
      lower.panel=panel.smooth,
      
      ~Threshold
      
      +InformednessCode
      +MarkednessCode
      +MatthewsCorrelationCode
      +FScoreCode
      
      +TruePositivesCode
      +TrueNegativesCode
      +FalsePositivesCode
      +FalseNegativesCode
      ,
      data = metric_comparison)
############################################################################################



############################################################################################
# choosing the best metrics per metric type
# text

# informedness
best_informedness_text_edit <- metric_comparison[metric_comparison$MetricType == 'EDIT' & metric_comparison$InformednessText == max(metric_comparison[metric_comparison$MetricType == 'EDIT']$InformednessText)]
best_informedness_text_set <- metric_comparison[metric_comparison$MetricType == 'SET' & metric_comparison$InformednessText == max(metric_comparison[metric_comparison$MetricType == 'SET']$InformednessText)]
best_informedness_text_profile <- metric_comparison[metric_comparison$MetricType == 'PROFILE' & metric_comparison$InformednessText == max(metric_comparison[metric_comparison$MetricType == 'PROFILE']$InformednessText)]
best_informedness_text_fingerprint <- metric_comparison[metric_comparison$MetricType == 'FINGERPRINT' & metric_comparison$InformednessText == max(metric_comparison[metric_comparison$MetricType == 'FINGERPRINT']$InformednessText)]
best_informedness_text_equal <- metric_comparison[metric_comparison$MetricType == 'EQUAL' & metric_comparison$InformednessText == max(metric_comparison[metric_comparison$MetricType == 'EQUAL']$InformednessText)]

best_informedness_text <- rbindlist(list(best_informedness_text_edit, best_informedness_text_set), use.names=TRUE, fill=TRUE)
best_informedness_text <- rbindlist(list(best_informedness_text, best_informedness_text_profile), use.names=TRUE, fill=TRUE)
best_informedness_text <- rbindlist(list(best_informedness_text, best_informedness_text_fingerprint), use.names=TRUE, fill=TRUE)
best_informedness_text <- rbindlist(list(best_informedness_text, best_informedness_text_equal), use.names=TRUE, fill=TRUE)

rm(best_informedness_text_edit, best_informedness_text_set, best_informedness_text_profile, best_informedness_text_fingerprint, best_informedness_text_equal)


# markedness
best_markedness_text_edit <- metric_comparison[metric_comparison$MetricType == 'EDIT' & metric_comparison$MarkednessText == max(metric_comparison[metric_comparison$MetricType == 'EDIT']$MarkednessText)]
best_markedness_text_set <- metric_comparison[metric_comparison$MetricType == 'SET' & metric_comparison$MarkednessText == max(metric_comparison[metric_comparison$MetricType == 'SET']$MarkednessText)]
best_markedness_text_profile <- metric_comparison[metric_comparison$MetricType == 'PROFILE' & metric_comparison$MarkednessText == max(metric_comparison[metric_comparison$MetricType == 'PROFILE']$MarkednessText)]
best_markedness_text_fingerprint <- metric_comparison[metric_comparison$MetricType == 'FINGERPRINT' & metric_comparison$MarkednessText == max(metric_comparison[metric_comparison$MetricType == 'FINGERPRINT']$MarkednessText)]
best_markedness_text_equal <- metric_comparison[metric_comparison$MetricType == 'EQUAL' & metric_comparison$MarkednessText == max(metric_comparison[metric_comparison$MetricType == 'EQUAL']$MarkednessText)]

best_markedness_text <- rbindlist(list(best_markedness_text_edit, best_markedness_text_set), use.names=TRUE, fill=TRUE)
best_markedness_text <- rbindlist(list(best_markedness_text, best_markedness_text_profile), use.names=TRUE, fill=TRUE)
best_markedness_text <- rbindlist(list(best_markedness_text, best_markedness_text_fingerprint), use.names=TRUE, fill=TRUE)
best_markedness_text <- rbindlist(list(best_markedness_text, best_markedness_text_equal), use.names=TRUE, fill=TRUE)

rm(best_markedness_text_edit, best_markedness_text_set, best_markedness_text_profile, best_markedness_text_fingerprint, best_markedness_text_equal)


# matthews correlation
best_matthewsCorrelation_text_edit <- metric_comparison[metric_comparison$MetricType == 'EDIT' & metric_comparison$MatthewsCorrelationText == max(metric_comparison[metric_comparison$MetricType == 'EDIT']$MatthewsCorrelationText)]
best_matthewsCorrelation_text_set <- metric_comparison[metric_comparison$MetricType == 'SET' & metric_comparison$MatthewsCorrelationText == max(metric_comparison[metric_comparison$MetricType == 'SET']$MatthewsCorrelationText)]
best_matthewsCorrelation_text_profile <- metric_comparison[metric_comparison$MetricType == 'PROFILE' & metric_comparison$MatthewsCorrelationText == max(metric_comparison[metric_comparison$MetricType == 'PROFILE']$MatthewsCorrelationText)]
best_matthewsCorrelation_text_fingerprint <- metric_comparison[metric_comparison$MetricType == 'FINGERPRINT' & metric_comparison$MatthewsCorrelationText == max(metric_comparison[metric_comparison$MetricType == 'FINGERPRINT']$MatthewsCorrelationText)]
best_matthewsCorrelation_text_equal <- metric_comparison[metric_comparison$MetricType == 'EQUAL' & metric_comparison$MatthewsCorrelationText == max(metric_comparison[metric_comparison$MetricType == 'EQUAL']$MatthewsCorrelationText)]

best_matthewsCorrelation_text <- rbindlist(list(best_matthewsCorrelation_text_edit, best_matthewsCorrelation_text_set), use.names=TRUE, fill=TRUE)
best_matthewsCorrelation_text <- rbindlist(list(best_matthewsCorrelation_text, best_matthewsCorrelation_text_profile), use.names=TRUE, fill=TRUE)
best_matthewsCorrelation_text <- rbindlist(list(best_matthewsCorrelation_text, best_matthewsCorrelation_text_fingerprint), use.names=TRUE, fill=TRUE)
best_matthewsCorrelation_text <- rbindlist(list(best_matthewsCorrelation_text, best_matthewsCorrelation_text_equal), use.names=TRUE, fill=TRUE)

rm(best_matthewsCorrelation_text_edit, best_matthewsCorrelation_text_set, best_matthewsCorrelation_text_profile, best_matthewsCorrelation_text_fingerprint, best_matthewsCorrelation_text_equal)


# fscore
best_fScore_text_edit <- metric_comparison[metric_comparison$MetricType == 'EDIT' & metric_comparison$FScoreText == max(metric_comparison[metric_comparison$MetricType == 'EDIT']$FScoreText)]
best_fScore_text_set <- metric_comparison[metric_comparison$MetricType == 'SET' & metric_comparison$FScoreText == max(metric_comparison[metric_comparison$MetricType == 'SET']$FScoreText)]
best_fScore_text_profile <- metric_comparison[metric_comparison$MetricType == 'PROFILE' & metric_comparison$FScoreText == max(metric_comparison[metric_comparison$MetricType == 'PROFILE']$FScoreText)]
best_fScore_text_fingerprint <- metric_comparison[metric_comparison$MetricType == 'FINGERPRINT' & metric_comparison$FScoreText == max(metric_comparison[metric_comparison$MetricType == 'FINGERPRINT']$FScoreText)]
best_fScore_text_equal <- metric_comparison[metric_comparison$MetricType == 'EQUAL' & metric_comparison$FScoreText == max(metric_comparison[metric_comparison$MetricType == 'EQUAL']$FScoreText)]

best_fScore_text <- rbindlist(list(best_fScore_text_edit, best_fScore_text_set), use.names=TRUE, fill=TRUE)
best_fScore_text <- rbindlist(list(best_fScore_text, best_fScore_text_profile), use.names=TRUE, fill=TRUE)
best_fScore_text <- rbindlist(list(best_fScore_text, best_fScore_text_fingerprint), use.names=TRUE, fill=TRUE)
best_fScore_text <- rbindlist(list(best_fScore_text, best_fScore_text_equal), use.names=TRUE, fill=TRUE)

rm(best_fScore_text_edit, best_fScore_text_set, best_fScore_text_profile, best_fScore_text_fingerprint, best_fScore_text_equal)


# false positives
best_falsePositives_text_edit <- metric_comparison[metric_comparison$MetricType == 'EDIT' & metric_comparison$Threshold < 1 & metric_comparison$FalsePositivesText == min(metric_comparison[metric_comparison$MetricType == 'EDIT' & metric_comparison$Threshold < 1]$FalsePositivesText)]
best_falsePositives_text_set <- metric_comparison[metric_comparison$MetricType == 'SET' & metric_comparison$Threshold < 1 & metric_comparison$FalsePositivesText == min(metric_comparison[metric_comparison$MetricType == 'SET' & metric_comparison$Threshold < 1]$FalsePositivesText)]
best_falsePositives_text_profile <- metric_comparison[metric_comparison$MetricType == 'PROFILE' & metric_comparison$Threshold < 1 & metric_comparison$FalsePositivesText == min(metric_comparison[metric_comparison$MetricType == 'PROFILE' & metric_comparison$Threshold < 1]$FalsePositivesText)]
best_falsePositives_text_fingerprint <- metric_comparison[metric_comparison$MetricType == 'FINGERPRINT' & metric_comparison$Threshold < 1 & metric_comparison$FalsePositivesText == min(metric_comparison[metric_comparison$MetricType == 'FINGERPRINT' & metric_comparison$Threshold < 1]$FalsePositivesText)]

best_falsePositives_text <- rbindlist(list(best_falsePositives_text_edit, best_falsePositives_text_set), use.names=TRUE, fill=TRUE)
best_falsePositives_text <- rbindlist(list(best_falsePositives_text, best_falsePositives_text_profile), use.names=TRUE, fill=TRUE)
best_falsePositives_text <- rbindlist(list(best_falsePositives_text, best_falsePositives_text_fingerprint), use.names=TRUE, fill=TRUE)


rm(best_falsePositives_text_edit, best_falsePositives_text_set, best_falsePositives_text_profile, best_falsePositives_text_fingerprint)


# best text
best_text <- rbindlist(list(best_informedness_text, best_markedness_text), use.names=TRUE, fill=TRUE)
best_text <- rbindlist(list(best_text, best_matthewsCorrelation_text), use.names=TRUE, fill=TRUE)
best_text <- rbindlist(list(best_text, best_fScore_text), use.names=TRUE, fill=TRUE)
best_text <- rbindlist(list(best_text, best_falsePositives_text), use.names=TRUE, fill=TRUE)


write.table(best_informedness_text, "best_informedness_text.csv", sep=";", row.names = F)
write.table(best_markedness_text, "best_markedness_text.csv", sep=";", row.names = F)
write.table(best_matthewsCorrelation_text, "best_matthewsCorrelation_text.csv", sep=";", row.names = F)
write.table(best_falsePositives_text, "best_falsePositives_text.csv", sep=";", row.names = F)




# code

# informedness
best_informedness_code_edit <- metric_comparison[metric_comparison$MetricType == 'EDIT' & metric_comparison$InformednessCode == max(metric_comparison[metric_comparison$MetricType == 'EDIT']$InformednessCode)]
best_informedness_code_set <- metric_comparison[metric_comparison$MetricType == 'SET' & metric_comparison$InformednessCode == max(metric_comparison[metric_comparison$MetricType == 'SET']$InformednessCode)]
best_informedness_code_profile <- metric_comparison[metric_comparison$MetricType == 'PROFILE' & metric_comparison$InformednessCode == max(metric_comparison[metric_comparison$MetricType == 'PROFILE']$InformednessCode)]
best_informedness_code_fingerprint <- metric_comparison[metric_comparison$MetricType == 'FINGERPRINT' & metric_comparison$InformednessCode == max(metric_comparison[metric_comparison$MetricType == 'FINGERPRINT']$InformednessCode)]
best_informedness_code_equal <- metric_comparison[metric_comparison$MetricType == 'EQUAL' & metric_comparison$InformednessCode == max(metric_comparison[metric_comparison$MetricType == 'EQUAL']$InformednessCode)]

best_informedness_code <- rbindlist(list(best_informedness_code_edit, best_informedness_code_set), use.names=TRUE, fill=TRUE)
best_informedness_code <- rbindlist(list(best_informedness_code, best_informedness_code_profile), use.names=TRUE, fill=TRUE)
best_informedness_code <- rbindlist(list(best_informedness_code, best_informedness_code_fingerprint), use.names=TRUE, fill=TRUE)
best_informedness_code <- rbindlist(list(best_informedness_code, best_informedness_code_equal), use.names=TRUE, fill=TRUE)

rm(best_informedness_code_edit, best_informedness_code_set, best_informedness_code_profile, best_informedness_code_fingerprint, best_informedness_code_equal)


# markedness
best_markedness_code_edit <- metric_comparison[metric_comparison$MetricType == 'EDIT' & metric_comparison$MarkednessCode == max(metric_comparison[metric_comparison$MetricType == 'EDIT']$MarkednessCode)]
best_markedness_code_set <- metric_comparison[metric_comparison$MetricType == 'SET' & metric_comparison$MarkednessCode == max(metric_comparison[metric_comparison$MetricType == 'SET']$MarkednessCode)]
best_markedness_code_profile <- metric_comparison[metric_comparison$MetricType == 'PROFILE' & metric_comparison$MarkednessCode == max(metric_comparison[metric_comparison$MetricType == 'PROFILE']$MarkednessCode)]
best_markedness_code_fingerprint <- metric_comparison[metric_comparison$MetricType == 'FINGERPRINT' & metric_comparison$MarkednessCode == max(metric_comparison[metric_comparison$MetricType == 'FINGERPRINT']$MarkednessCode)]
best_markedness_code_equal <- metric_comparison[metric_comparison$MetricType == 'EQUAL' & metric_comparison$MarkednessCode == max(metric_comparison[metric_comparison$MetricType == 'EQUAL']$MarkednessCode)]

best_markedness_code <- rbindlist(list(best_markedness_code_edit, best_markedness_code_set), use.names=TRUE, fill=TRUE)
best_markedness_code <- rbindlist(list(best_markedness_code, best_markedness_code_profile), use.names=TRUE, fill=TRUE)
best_markedness_code <- rbindlist(list(best_markedness_code, best_markedness_code_fingerprint), use.names=TRUE, fill=TRUE)
best_markedness_code <- rbindlist(list(best_markedness_code, best_markedness_code_equal), use.names=TRUE, fill=TRUE)

rm(best_markedness_code_edit, best_markedness_code_set, best_markedness_code_profile, best_markedness_code_fingerprint, best_markedness_code_equal)


# matthews correlation
best_matthewsCorrelation_code_edit <- metric_comparison[metric_comparison$MetricType == 'EDIT' & metric_comparison$MatthewsCorrelationCode == max(metric_comparison[metric_comparison$MetricType == 'EDIT']$MatthewsCorrelationCode)]
best_matthewsCorrelation_code_set <- metric_comparison[metric_comparison$MetricType == 'SET' & metric_comparison$MatthewsCorrelationCode == max(metric_comparison[metric_comparison$MetricType == 'SET']$MatthewsCorrelationCode)]
best_matthewsCorrelation_code_profile <- metric_comparison[metric_comparison$MetricType == 'PROFILE' & metric_comparison$MatthewsCorrelationCode == max(metric_comparison[metric_comparison$MetricType == 'PROFILE']$MatthewsCorrelationCode)]
best_matthewsCorrelation_code_fingerprint <- metric_comparison[metric_comparison$MetricType == 'FINGERPRINT' & metric_comparison$MatthewsCorrelationCode == max(metric_comparison[metric_comparison$MetricType == 'FINGERPRINT']$MatthewsCorrelationCode)]
best_matthewsCorrelation_code_equal <- metric_comparison[metric_comparison$MetricType == 'EQUAL' & metric_comparison$MatthewsCorrelationCode == max(metric_comparison[metric_comparison$MetricType == 'EQUAL']$MatthewsCorrelationCode)]

best_matthewsCorrelation_code <- rbindlist(list(best_matthewsCorrelation_code_edit, best_matthewsCorrelation_code_set), use.names=TRUE, fill=TRUE)
best_matthewsCorrelation_code <- rbindlist(list(best_matthewsCorrelation_code, best_matthewsCorrelation_code_profile), use.names=TRUE, fill=TRUE)
best_matthewsCorrelation_code <- rbindlist(list(best_matthewsCorrelation_code, best_matthewsCorrelation_code_fingerprint), use.names=TRUE, fill=TRUE)
best_matthewsCorrelation_code <- rbindlist(list(best_matthewsCorrelation_code, best_matthewsCorrelation_code_equal), use.names=TRUE, fill=TRUE)

rm(best_matthewsCorrelation_code_edit, best_matthewsCorrelation_code_set, best_matthewsCorrelation_code_profile, best_matthewsCorrelation_code_fingerprint, best_matthewsCorrelation_code_equal)


# fscore
best_fScore_code_edit <- metric_comparison[metric_comparison$MetricType == 'EDIT' & metric_comparison$FScoreCode == max(metric_comparison[metric_comparison$MetricType == 'EDIT']$FScoreCode)]
best_fScore_code_set <- metric_comparison[metric_comparison$MetricType == 'SET' & metric_comparison$FScoreCode == max(metric_comparison[metric_comparison$MetricType == 'SET']$FScoreCode)]
best_fScore_code_profile <- metric_comparison[metric_comparison$MetricType == 'PROFILE' & metric_comparison$FScoreCode == max(metric_comparison[metric_comparison$MetricType == 'PROFILE']$FScoreCode)]
best_fScore_code_fingerprint <- metric_comparison[metric_comparison$MetricType == 'FINGERPRINT' & metric_comparison$FScoreCode == max(metric_comparison[metric_comparison$MetricType == 'FINGERPRINT']$FScoreCode)]
best_fScore_code_equal <- metric_comparison[metric_comparison$MetricType == 'EQUAL' & metric_comparison$FScoreCode == max(metric_comparison[metric_comparison$MetricType == 'EQUAL']$FScoreCode)]

best_fScore_code <- rbindlist(list(best_fScore_code_edit, best_fScore_code_set), use.names=TRUE, fill=TRUE)
best_fScore_code <- rbindlist(list(best_fScore_code, best_fScore_code_profile), use.names=TRUE, fill=TRUE)
best_fScore_code <- rbindlist(list(best_fScore_code, best_fScore_code_fingerprint), use.names=TRUE, fill=TRUE)
best_fScore_code <- rbindlist(list(best_fScore_code, best_fScore_code_equal), use.names=TRUE, fill=TRUE)

rm(best_fScore_code_edit, best_fScore_code_set, best_fScore_code_profile, best_fScore_code_fingerprint, best_fScore_code_equal)


# false positives
best_falsePositives_code_edit <- metric_comparison[metric_comparison$MetricType == 'EDIT' & metric_comparison$Threshold < 1 & metric_comparison$FalsePositivesCode == min(metric_comparison[metric_comparison$MetricType == 'EDIT' & metric_comparison$Threshold < 1]$FalsePositivesCode)]
best_falsePositives_code_set <- metric_comparison[metric_comparison$MetricType == 'SET' & metric_comparison$Threshold < 1 & metric_comparison$FalsePositivesCode == min(metric_comparison[metric_comparison$MetricType == 'SET' & metric_comparison$Threshold < 1]$FalsePositivesCode)]
best_falsePositives_code_profile <- metric_comparison[metric_comparison$MetricType == 'PROFILE' & metric_comparison$Threshold < 1 & metric_comparison$FalsePositivesCode == min(metric_comparison[metric_comparison$MetricType == 'PROFILE' & metric_comparison$Threshold < 1]$FalsePositivesCode)]
best_falsePositives_code_fingerprint <- metric_comparison[metric_comparison$MetricType == 'FINGERPRINT' & metric_comparison$Threshold < 1 & metric_comparison$FalsePositivesCode == min(metric_comparison[metric_comparison$MetricType == 'FINGERPRINT' & metric_comparison$Threshold < 1]$FalsePositivesCode)]

best_falsePositives_code <- rbindlist(list(best_falsePositives_code_edit, best_falsePositives_code_set), use.names=TRUE, fill=TRUE)
best_falsePositives_code <- rbindlist(list(best_falsePositives_code, best_falsePositives_code_profile), use.names=TRUE, fill=TRUE)
best_falsePositives_code <- rbindlist(list(best_falsePositives_code, best_falsePositives_code_fingerprint), use.names=TRUE, fill=TRUE)


rm(best_falsePositives_code_edit, best_falsePositives_code_set, best_falsePositives_code_profile, best_falsePositives_code_fingerprint)


# runtime
best_runtime_edit <- metric_comparison[metric_comparison$MetricType == 'EDIT' & metric_comparison$Runtime == min(metric_comparison[metric_comparison$MetricType == 'EDIT']$Runtime)]
best_runtime_set <- metric_comparison[metric_comparison$MetricType == 'SET' & metric_comparison$Runtime == min(metric_comparison[metric_comparison$MetricType == 'SET']$Runtime)]
best_runtime_profile <- metric_comparison[metric_comparison$MetricType == 'PROFILE' & metric_comparison$Runtime == min(metric_comparison[metric_comparison$MetricType == 'PROFILE']$Runtime)]
best_runtime_fingerprint <- metric_comparison[metric_comparison$MetricType == 'FINGERPRINT' & metric_comparison$Runtime == min(metric_comparison[metric_comparison$MetricType == 'FINGERPRINT']$Runtime)]
best_runtime_equal <- metric_comparison[metric_comparison$MetricType == 'EQUAL' & metric_comparison$Runtime == min(metric_comparison[metric_comparison$MetricType == 'EQUAL']$Runtime)]

best_runtime <- rbindlist(list(best_runtime_edit, best_runtime_set), use.names=TRUE, fill=TRUE)
best_runtime <- rbindlist(list(best_runtime, best_runtime_profile), use.names=TRUE, fill=TRUE)
best_runtime <- rbindlist(list(best_runtime, best_runtime_fingerprint), use.names=TRUE, fill=TRUE)
best_runtime <- rbindlist(list(best_runtime, best_runtime_equal), use.names=TRUE, fill=TRUE)

rm(best_runtime_edit, best_runtime_set, best_runtime_profile, best_runtime_fingerprint, best_runtime_equal)


# best code 
best_code <- rbindlist(list(best_informedness_code, best_markedness_code), use.names=TRUE, fill=TRUE)
best_code <- rbindlist(list(best_code, best_matthewsCorrelation_code), use.names=TRUE, fill=TRUE)
best_code <- rbindlist(list(best_code, best_fScore_code), use.names=TRUE, fill=TRUE)
best_code <- rbindlist(list(best_code, best_falsePositives_code), use.names=TRUE, fill=TRUE)

best <- rbindlist(list(best_text, best_code), use.names=TRUE, fill=TRUE)
best <- rbindlist(list(best, best_runtime), use.names=TRUE, fill=TRUE)


# write best metrics in own csv files
write.table(best_informedness_code, "best_informedness_code.csv", sep=";", row.names = F)
write.table(best_markedness_code, "best_markedness_code.csv", sep=";", row.names = F)
write.table(best_matthewsCorrelation_code, "best_matthewsCorrelation_code.csv", sep=";", row.names = F)
write.table(best_falsePositives_code, "best_falsePositives_code.csv", sep=";", row.names = F)
write.table(best_runtime, "best_runtime.csv", sep=";", row.names = F)


