calculate_bic <- function(discretized_data, npar_table, Loss_table, threshold) {
  
  
  set.seed(2023) 
  cat("------------------------------", "\n")
  print("Starting 'calculate_bic' inputs:")
  cat("------------------------------", "\n")
  
  # ------------- 
  temp_threshold <- c("empty_graph", threshold, "all")
  # temp_threshold <- c(threshold, "all")

  num_sample <- nrow(discretized_data)
  
  # browser()

  BIC_merged <- data.frame(node = rownames(npar_table))  # npar_table$node  replaced with "rownames(npar_table)"
  
  for (i in 1:(length(temp_threshold))) {
      BIC_thresh <- Loss_table[[i]] * num_sample + npar_table[[i]] * log(num_sample)
      
      BIC.threshold.colname <- paste0("Threshold (", temp_threshold[i], ")", sep = "")
     
      BIC_thresh <- data.frame(BIC_thresh)
      colnames(BIC_thresh)<- BIC.threshold.colname
      # browser()
      BIC_merged <- cbind(BIC_merged, BIC_thresh)
      # print(BIC_merged)
      
      }
  # --------------
  rownames(BIC_merged) <- BIC_merged$node  
  BIC_merged <- BIC_merged[, -1]  
  # --------------
  
  return(BIC_merged)
}