# Preparing threshold columns
prepare_threshold_columns <- function(possible_seed_arcs_filter, threshold) {
  
  print("Starting 'prepare_threshold_columns' inputs:")
  
  source("temp.white_thresh.cols.R")
  temp.white_thresh.cols <- temp.white_thresh.cols(possible_seed_arcs_filter, threshold)
  return(temp.white_thresh.cols)
}