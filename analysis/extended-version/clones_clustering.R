# set working directory (see https://stackoverflow.com/a/35842119)
dir = tryCatch({
  # script being sourced
  getSrcDirectory()[1]
}, error = function(e) {
  # script being run in RStudio
  dirname(rstudioapi::getActiveDocumentContext()$path)
})
setwd(dir)

# load libraries
library(data.table)
#library(random)
library(sqldf)

# read data
clones <- fread("data/CodeBlocksComparisonFiltered.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
# prevent problems with SQLDF and integer64
clones$ContentNormalizedHash <- as.character(clones$ContentNormalizedHash)
n <- nrow(clones)
n
# 29,978

summary(clones$PostCount)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.000   2.000   2.000   2.256   2.000  78.000 

table(clones$PostCount)[1:10]
#     2     3     4     5     6     7     8     9    10    11 
# 25779  2786   772   283   132    63    41    39    20    15 

clones$UniqueOwners <- as.character(rep(NA, n))
clones$UniqueOwnerCount <- as.integer(rep(NA, n))
clones$ClonesWithinThreads <- as.integer(rep(NA, n))
clones$MedianScore <- as.numeric(rep(NA, n))
clones$MedianTimeBetweenPosts <- as.numeric(rep(NA, n))
clones$QuestionCount <- as.integer(rep(NA, n))
clones$AnswerCount <- as.integer(rep(NA, n))

# option for timestamps
options(tz="UTC")

for (i in 1:n) {
  if (i == 1 || i %% 100 == 0) {
    print(paste0("Processing clone ", i, " of ", n, "..."))     
  }
  
  # split columns with owner information
  owner_user_ids <- unlist(strsplit(as.character(clones[i,"OwnerUserIds"]), ";", fixed=TRUE))
  owner_user_display_names <- unlist(strsplit(as.character(clones[i,"OwnerDisplayNames"]), ";", fixed=TRUE))
  
  # replace N/A users with display names
  na_pos <- which(owner_user_ids == "N/A")
  owner_user_ids[na_pos] <- owner_user_display_names[na_pos]
  
  # get unique users
  unique_owners <- unique(owner_user_ids)
  
  # add number of unique owners to dataframe
  clones[i,"UniqueOwners"] <- paste(unique_owners, collapse=";")
  clones[i,"UniqueOwnerCount"] <- length(unique_owners)
  
  # determine whether there are clones within a thread
  clones[i,"ClonesWithinThreads"] <- as.integer(clones[i,"PostCount"]) - as.integer(clones[i,"ThreadCount"])
  
  # determine median score
  scores <- as.integer(unlist(strsplit(as.character(clones[i,"Scores"]), ";", fixed=TRUE)))
  clones[i,"MedianScore"] <- median(scores)
  
  # split creation date timestamps
  creation_dates <- unlist(strsplit(as.character(clones[i,"CreationDates"]), ";", fixed=TRUE))
  # parse timestamps
  creation_dates <- as.POSIXct(creation_dates, tz="UTC")
  creation_dates <- sort(creation_dates, decreasing=TRUE)
  creation_dates_diff <- as.integer(rep(NA, length(creation_dates)-1))
  for (j in 1:length(creation_dates)-1) {
    creation_dates_diff[j] <- as.numeric(difftime(creation_dates[j], creation_dates[j+1], units="hours"))  
  }
  clones[i,"MedianTimeBetweenPosts"] <- median(creation_dates_diff)
  
  post_ids <- as.integer(unlist(strsplit(as.character(clones[i,"PostIds"]), ";", fixed=TRUE)))
  parent_ids <- as.integer(unlist(strsplit(as.character(clones[i,"ParentIds"]), ";", fixed=TRUE)))
  clones[i,"QuestionCount"] <- length(which(post_ids == parent_ids))
  clones[i,"AnswerCount"] <- length(which(post_ids != parent_ids))

  # TODO: Links
}

clones$PostsPerOwner <- as.numeric(clones$PostCount/clones$UniqueOwnerCount)
clones$QuestionRatio <- as.numeric(clones$QuestionCount/clones$PostCount)
clones$AnswerRatio <- as.numeric(clones$AnswerCount/clones$PostCount)

clones_clustering <- clones[,c("ContentNormalizedHash", "AnswerRatio", "MedianScore", "MedianTimeBetweenPosts")]
ncol(clones_clustering)
# 3

library(Hmisc)
rcorr(as.matrix(clones_clustering[,2:4]), type="spearman")
#                        AnswerRatio MedianScore MedianTimeBetweenPosts
# AnswerRatio                   1.00        0.33                   0.18
# MedianScore                   0.33        1.00                   0.25
# MedianTimeBetweenPosts        0.18        0.25                   1.00

# z-score standardization
# https://vitalflux.com/data-science-scale-normalize-numeric-data-using-r/
clones_clustering$AnswerRatioScaled <- scale(clones_clustering$AnswerRatio)
clones_clustering$MedianScoreScaled <- scale(clones_clustering$MedianScore)
clones_clustering$MedianTimeBetweenPostsScaled <- scale(clones_clustering$MedianTimeBetweenPosts)

library(cluster)
clones_clustering$cluster <- pam(clones_clustering[,5:7], 3, metric="manhattan")$clustering
table(clones_clustering$cluster)
#     1     2     3 
# 18548  3322  8108 

#shapes <- as.integer(rep(1, length(clones_clustering$cluster)))
#shapes[clones_clustering$cluster==1] <- 16
#shapes[clones_clustering$cluster==2] <- 17
#shapes[clones_clustering$cluster==3] <- 18
#length(shapes)
# 29978

colors <- as.character(rep("gray95", length(clones_clustering$cluster)))
colors[clones_clustering$cluster==1] <- "cornflowerblue"
colors[clones_clustering$cluster==2] <- "darkseagreen1"
colors[clones_clustering$cluster==3] <- "coral"
length(colors)
# 29978

quartz(type="pdf", file="figures/scatterplot-3d.pdf", width=12, height=10) # prevents unicode issues in pdf
par(
  bg="white",
  # mar = c(3, 1.8, 3, 1.5)+0.1, # subplot margins (bottom, left, top, right)
  # omi = c(0.0, 0.0, 0.0, 0.0),  # outer margins in inches (bottom, left, top, right)
  # mfrow = c(2, 1),
  # pin = (width, height)
  # mfcol # draw in columns
  # increase font size
  cex=1.3,
  cex.main=1.3,
  cex.sub=1,
  cex.lab=1,
  cex.axis=1
)

library(scatterplot3d)
scatterplot3d(clones_clustering$AnswerRatioScaled,
              clones_clustering$MedianScoreScaled,
              clones_clustering$MedianTimeBetweenPostsScaled,
              angle=60,
              xlab="AnswerRatioScaled",
              ylab="MedianScoreScaled",
              zlab="MedianTimeBetweenPostsScaled",
              #pch=shapes,
              color=colors
)

dev.off()

quartz(type="pdf", file="figures/scatterplot-matrix.pdf", width=12, height=10) # prevents unicode issues in pdf
par(
  bg="white",
  # mar = c(3, 1.8, 3, 1.5)+0.1, # subplot margins (bottom, left, top, right)
  # omi = c(0.0, 0.0, 0.0, 0.0),  # outer margins in inches (bottom, left, top, right)
  # mfrow = c(2, 1),
  # pin = (width, height)
  # mfcol # draw in columns
  # increase font size
  cex=1.3,
  cex.main=1.3,
  cex.sub=1,
  cex.lab=1,
  cex.axis=1
)

pairs(clones_clustering[,5:7], col=colors)

dev.off()


summary(clones_clustering[clones_clustering$cluster == 1,2:4])
#  AnswerRatio       MedianScore       MedianTimeBetweenPosts
# Min.   :0.00000   Min.   :-11.0000   Min.   :    0.000     
# 1st Qu.:0.00000   1st Qu.:  0.0000   1st Qu.:    4.582     
# Median :0.00000   Median :  0.0000   Median :   23.605     
# Mean   :0.04439   Mean   :  0.6053   Mean   :  829.689     
# 3rd Qu.:0.00000   3rd Qu.:  1.0000   3rd Qu.:  137.633     
# Max.   :0.50000   Max.   : 75.5000   Max.   :16962.002

summary(clones_clustering[clones_clustering$cluster == 2,2:4])
#  AnswerRatio     MedianScore      MedianTimeBetweenPosts
# Min.   :0.000   Min.   : -3.500   Min.   : 5377         
# 1st Qu.:0.500   1st Qu.:  0.500   1st Qu.:16009         
# Median :0.500   Median :  2.000   Median :21958         
# Mean   :0.585   Mean   :  9.045   Mean   :24817         
# 3rd Qu.:1.000   3rd Qu.:  6.000   3rd Qu.:31792         
# Max.   :1.000   Max.   :881.000   Max.   :85610

summary(clones_clustering[clones_clustering$cluster == 3,2:4])
#  AnswerRatio      MedianScore      MedianTimeBetweenPosts
# Min.   :0.5000   Min.   : -5.000   Min.   :    0.005     
# 1st Qu.:1.0000   1st Qu.:  0.500   1st Qu.:    0.454     
# Median :1.0000   Median :  1.000   Median :  143.001     
# Mean   :0.8988   Mean   :  3.094   Mean   : 2154.915     
# 3rd Qu.:1.0000   3rd Qu.:  2.500   3rd Qu.: 2808.218     
# Max.   :1.5000   Max.   :422.000   Max.   :16683.234

medians <- data.frame(
  rbind(apply(clones_clustering[clones_clustering$cluster == 1,2:4], 2, median),
        apply(clones_clustering[clones_clustering$cluster == 2,2:4], 2, median),
        apply(clones_clustering[clones_clustering$cluster == 3,2:4], 2, median)
  )
)
medians
#   AnswerRatio MedianScore MedianTimeBetweenPosts
# 1         0.0           0               23.60542
# 2         0.5           2            21957.78313
# 3         1.0           1              143.00069

# draw sample for qualitative analysis
n <- 5

sample_cluster_1 <- clones[clones$ContentNormalizedHash %in% sample(clones_clustering[clones_clustering$cluster == 1,]$ContentNormalizedHash, n),]
sample_cluster_1$Cluster <- as.integer(rep(1, n))
sample_cluster_2 <- clones[clones$ContentNormalizedHash %in% sample(clones_clustering[clones_clustering$cluster == 2,]$ContentNormalizedHash, n),]
sample_cluster_2$Cluster <- as.integer(rep(2, n))
sample_cluster_3 <- clones[clones$ContentNormalizedHash %in% sample(clones_clustering[clones_clustering$cluster == 3,]$ContentNormalizedHash, n),]
sample_cluster_3$Cluster <- as.integer(rep(3, n))

sample_export <- data.frame(
  rbind(sample_cluster_1, sample_cluster_2, sample_cluster_3)
)
names(sample_export) <- names(sample_cluster_1)

write.table(sample_export, file="sample4.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")

# write posts containing clone
for (i in 1:nrow(sample_export)) {
  post_ids <- strsplit(sample_export[i,]$PostIds, ";", fixed = TRUE)
  parent_ids <- strsplit(sample_export[i,]$ParentIds, ";", fixed = TRUE)
  owner_user_ids <- strsplit(sample_export[i,]$OwnerUserIds, ";", fixed = TRUE)
  creation_dates <- strsplit(sample_export[i,]$CreationDates, ";", fixed = TRUE)
  scores <- strsplit(sample_export[i,]$Scores, ";", fixed = TRUE)
  
  clone_data <- data.frame(
    post_ids,
    parent_ids,
    owner_user_ids,
    creation_dates,
    scores,
    stringsAsFactors=FALSE
  )
  names(clone_data) <- c("PostId", "ParentId", "OwnerUserId", "CreationDate", "Score")
  
  # sort posts chronologically
  clone_data <- clone_data[order(clone_data$CreationDate),]
  
  # write metadata
  write.table(clone_data, file=paste0("export_sample4/", sample_export[i,]$ContentNormalizedHash, ".csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
  
  # write content
  content <- data.frame(Content=as.character(sample_export[i,]$Content))
  write.table(content, file=paste0("export_sample4/", sample_export[i,]$ContentNormalizedHash, "_content.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
}

# write index file
index <- sample_export[,c("ContentNormalizedHash", "Cluster", "PostCount", "ThreadCount", "LineCount")]
names(index) <- c("HashValue", "Sample", "PostCount", "ThreadCount", "LineCount")

write.table(index, file="export_sample4/index.csv", sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")


# read SO links
clones_so_links <- fread("data/CodeBlocksComparisonFilteredLinksSOExport.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
# prevent problems with SQLDF and integer64
clones_so_links$ContentNormalizedHash <- as.character(clones_so_links$ContentNormalizedHash)
nrow(clones_so_links)
# 4,962

# write SO links for each cloned code block
for (hash_value in sample_export$ContentNormalizedHash) {
  so_links <- sqldf(paste0("SELECT LinkedPostId, LinkedPostTypeId, PostCount FROM clones_so_links WHERE ContentNormalizedHash=", hash_value, " ORDER BY PostCount DESC"))
  write.table(so_links, file=paste0("export_sample4/", hash_value, "_so-links.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
}


# read other links
clones_links <- fread("data/CodeBlocksComparisonFilteredLinksNonSOExport.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null"))
# prevent problems with SQLDF and integer64
clones_links$ContentNormalizedHash <- as.character(clones_links$ContentNormalizedHash)
nrow(clones_links)
# 28,733

# write other links for each cloned code block
for (hash_value in sample_export$ContentNormalizedHash) {
  links <- sqldf(paste0("SELECT Url, PostCount FROM clones_links WHERE ContentNormalizedHash=", hash_value, " ORDER BY PostCount DESC"))
  write.table(links, file=paste0("export_sample4/", hash_value, "_links.csv"), sep=",", col.names=TRUE, row.names=FALSE, na="", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
}
