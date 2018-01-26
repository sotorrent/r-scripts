setwd("F:/Git/github/r-scripts/analysis/") # please update path
#setwd("/Users/sebastian/git/github/r-scripts/analysis/")

# post score
library(data.table)
posts_score <- fread("data/posts_score.csv", header=FALSE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"), stringsAsFactors=FALSE)
names(posts_score) <- c("PostId", "PostTypeId", "Score")

summary(posts_score$Score)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -147.000     0.000     1.000     2.356     2.000 26580.000

quantile(posts_score$Score, seq(0.9, 1.0, by=0.01))
# 90%   91%   92%   93%   94%   95%   96%   97%   98%   99%  100% 
# 4     5     5     6     6     7     9    11    16    28 26580

length(posts_score$Score[posts_score$Score>=1])/nrow(posts_score)*100
# 54.40137

length(posts_score$Score[posts_score$Score>=5])/nrow(posts_score)*100
# 9.080947
