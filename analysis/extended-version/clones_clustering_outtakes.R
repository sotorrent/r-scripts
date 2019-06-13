clones_clustering <- clones[,c("QuestionRatio", "PostsPerOwner", "MedianTimeBetweenPosts")]
ncol(clones_clustering)
# 3

# z-score standardization
# https://vitalflux.com/data-science-scale-normalize-numeric-data-using-r/
clones_clustering$QuestionRatio <- scale(clones_clustering$QuestionRatio)
clones_clustering$PostsPerOwner <- scale(clones_clustering$PostsPerOwner)
clones_clustering$MedianTimeBetweenPosts <- scale(clones_clustering$MedianTimeBetweenPosts)

clones_clustering_dist <- dist(clones_clustering, method="manhattan")

library(cluster)
# https://www.datanovia.com/en/lessons/k-medoids-in-r-algorithm-and-practical-examples/
# https://uc-r.github.io/kmeans_clustering#optimal
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  res <- pam(clones_clustering, k, metric="manhattan")
  sil <- silhouette(res$cluster, clones_clustering_dist)
  mean(sil[,3])
}

# Compute and plot average silhouette for k = 2 to k = 15
k_values <- 2:15
# extract avg silhouette for 2-15 clusters
library(purrr)
avg_sil_values <- map_dbl(k_values, avg_sil)

plot(k_values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters k",
     ylab = "Average silhouettes")

clones_clustering <- clones[,c("AnswerRatio", "PostsPerOwner", "MedianTimeBetweenPosts")]
ncol(clones_clustering)
# 3

# z-score standardization
# https://vitalflux.com/data-science-scale-normalize-numeric-data-using-r/
clones_clustering$AnswerRatio <- scale(clones_clustering$AnswerRatio)
clones_clustering$PostsPerOwner <- scale(clones_clustering$PostsPerOwner)
clones_clustering$MedianTimeBetweenPosts <- scale(clones_clustering$MedianTimeBetweenPosts)

library(scatterplot3d)
scatterplot3d(clones_clustering$AnswerRatio,
              clones_clustering$MedianTimeBetweenPosts,
              clones_clustering$PostsPerOwner,
              angle=80
              #xlab="Median Score",
              #ylab="Post Count",
              #zlab="Median Time Between Posts",
              #pch=shapes,
              #color=colors
)

library(dbscan)
# http://www.sthda.com/english/wiki/print.php?id=246
# https://stackoverflow.com/a/54115075
# minPts=2*dim => 2*3=6

kNNdistplot(clones_clustering, k=6)
abline(h=0.2, lty=2)

dbscan_clustering <- dbscan(clones_clustering, eps=0.2, minPts=6)
table(dbscan_clustering$cluster)
#   0     1     2     3     4     5     6     7     8     9    10    11 
# 198 12544 10379  4157   621   283    15    67   719    12    36   433 
#
#  12    13    14    15    16    17    18    19    20    21    22    23 
# 110    42    13     6    58    89    12     7    40    19    14    13 
#
# 24    25    26    27    28    29    30    31    32    33    34 
# 14     8     7     7    10     6     8     8     6    11     6 
