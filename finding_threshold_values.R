finding_threshold_values <- function(possible_seed_arcs_filter, n) {

  cat("------------------------------", "\n")
  print("Starting 'finding_threshold_values' inputs:")
  cat("------------------------------", "\n")

  cat("****************************************", "\n")
  print("possible_seed_arcs_filter:")
  cat("****************************************", "\n")
  print(possible_seed_arcs_filter)

  strength_cols_thresh<- possible_seed_arcs_filter


  # --------------------------- 
  selected_cols_name <- c("Min.strength", "Max.strength")

  selected_data_thresh <- strength_cols_thresh[, selected_cols_name]
  selected_data_thresh[mapply(is.na, selected_data_thresh)] <- ""
  selected_data_thresh <- lapply(selected_data_thresh, as.numeric)


  cat("**********************-----------******************", "\n")
  print("selected_data_thresh:")
  cat("**********************-----------******************", "\n")
  print(selected_data_thresh)


  # quantiles for data
  quantile_vals <- quantile(unlist(selected_data_thresh), probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
  threshold_vals <- unname(quantile_vals)
  threshold_vals <- as.numeric(lapply(threshold_vals, function(x) signif(x, digits = 2)))

  return(threshold_vals)
}