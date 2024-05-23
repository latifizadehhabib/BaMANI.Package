finding_threshold_values <- function(possible_seed_arcs_filter, n) {
  
  cat("------------------------------", "\n")
  print("Starting 'finding_threshold_values' inputs:")
  cat("------------------------------", "\n")
  
  strength_cols_thresh<- possible_seed_arcs_filter
  
  # select columns that contain "_strength" but not "Min_strength" or "Max_strength"
  cols_name <- grep("_strength", names(strength_cols_thresh), value = TRUE, ignore.case = TRUE)
  selected_cols_name <- cols_name[!(cols_name %in% c("Min_strength", "Max_strength"))]
  
  # extract selected columns from data
  selected_data_thresh <- strength_cols_thresh[, selected_cols_name]
  selected_data_thresh[mapply(is.na, selected_data_thresh)] <- ""
  selected_data_thresh <- lapply(selected_data_thresh, as.numeric)
  
  # calculate quantiles for data
  quantile_vals <- quantile(unlist(selected_data_thresh), probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
  threshold_vals <- unname(quantile_vals)
  threshold_vals <- as.numeric(lapply(threshold_vals, function(x) signif(x, digits = 2))) 
  
  
  # threshold_vals <- signif(threshold_vals, digits = 5)
  # threshold_vals <- as.numeric(sprintf("%.3f", threshold_vals))
  
  # threshold_vals <- as.data.frame(threshold.vals = threshold_vals)
  # colnames(threshold_vals) <- "threshold.vals"
  # threshold_vals <- c(0, threshold_vals)
  return(threshold_vals)
}



# -------------------------------------------
# finding_threshold_values <- function(possible_seed_arcs_filter) {
#   
# strength_cols_thresh<- possible_seed_arcs_filter
# 
# # select columns that contain "_strength" but not "Min_strength" or "Max_strength"
# cols_name <- grep("_strength", names(strength_cols_thresh), value = TRUE, ignore.case = TRUE)
# # view(selected_cols)
# selected_cols_name <- cols_name[!(cols_name %in% c("Min_strength", "Max_strength"))]
# # view(selected_cols)
# strength_cols_thresh <- strength_cols_thresh[, selected_cols_name]
# # strength_cols_thresh <- strength_cols_thresh[, !colnames(strength_cols_thresh) %in% selected_cols_name]
# 
# 
# # extract selected columns from data
# selected_data_thresh <- strength_cols_thresh[selected_cols_name]
# # view(selected_data)
# 
# selected_data_thresh[mapply(is.na, selected_data_thresh)] <- ""
# selected_data_thresh <- lapply(selected_data_thresh, as.numeric)
# mix_strength <- as.data.frame(selected_data_thresh)
# view(mix_strength)
# 
# # Calculate (n-1) quantiles for data in interval. 
# n <- 8  # just as an example 
# (quantile <- quantile(mix_strength, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE) )
# (threshold <-unname(quantile))      # unname(quantile)[4]              # Get only quantile values#
# # print(threshold)
# 
# return(threshold)
# }




