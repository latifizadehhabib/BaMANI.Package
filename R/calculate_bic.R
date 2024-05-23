calculate_bic <- function(discretized_data, npar_merged, L1_merged, threshold) {
  
  
  
  cat("------------------------------", "\n")
  print("Starting 'calculate_bic' inputs:")
  cat("------------------------------", "\n")
  
  
  temp_threshold <- c("empty_graph", 0, threshold, "all")
  num_sample <- nrow(discretized_data)
  BIC_merged <- data.frame(node = rownames(npar_merged))  # npar_merged$node  replaced with "rownames(npar_merged)"
  
  for (i in 1:(length(temp_threshold))) {
      BIC_thresh <- L1_merged[[i]] * num_sample + npar_merged[[i]] * log(num_sample)
      
      col.BIC.threshold.name <- paste0("BIC_thresh_", temp_threshold[i], sep = "")
      BIC_thresh <- data.frame(BIC_thresh)
      colnames(BIC_thresh)<- col.BIC.threshold.name
      BIC_merged <- cbind(BIC_merged, BIC_thresh)
      }
  # --------------
  rownames(BIC_merged) <- BIC_merged$node  # Set row names to first column
  BIC_merged <- BIC_merged[, -1]  # Remove first column
  # --------------
  
  return(BIC_merged)
}



# #----------------------------------------------------
# 
# calculate_bic <- function(discretized_data, npar_merged, L1_merged, threshold) {
#   num_sample <- dim(discretized_data)[1]
#   BIC_merged <- data.frame(node = npar_merged$node)
#   
#   threshold_name <- c(names(BIC_merged), "BIC_empty_graph", "BIC_thresh_0")
#   
#   for (i in 1:(length(threshold) + 2)) {
#     if (i > 2){
#       BIC_thresh <- L1_merged[[i+1]] * num_sample + npar_merged[[i+1]] * log(num_sample)
#       BIC_thresh <- data.frame(BIC_thresh)
#       BIC_merged <- cbind(BIC_merged, BIC_thresh)
#       threshold_name <- c(threshold_name, paste0("BIC_thresh_", threshold[i-2], sep = ""))
#     } else {
#       BIC_thresh <- L1_merged[[i+1]] * num_sample + npar_merged[[i+1]] * log(num_sample)
#       BIC_thresh <- data.frame(BIC_thresh)
#       BIC_merged <- cbind(BIC_merged, BIC_thresh)
#     }
#   }
#   
#   colnames(BIC_merged) <- threshold_name
#   return(BIC_merged)
# }
