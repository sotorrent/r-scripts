# set working directory (see https://stackoverflow.com/a/35842119)
dir = tryCatch({
  # script being sourced
  getSrcDirectory()[1]
}, error = function(e) {
  # script being run in RStudio
  dirname(rstudioapi::getActiveDocumentContext()$path)
})
setwd(dir)

# load functions
source("functions.R")

ITERATION <- 3

# read results of metric evaluation run
read_metrics_evaluation_per_post(ITERATION, "default")
retrieve_fp_fn(ITERATION)

length(fp_text_posts)
# 4

length(fn_text_posts)
# 6

length(fp_code_posts)
# 1

length(fn_code_posts)
# 4

length(f_text)
# 7

length(f_code)
# 4
