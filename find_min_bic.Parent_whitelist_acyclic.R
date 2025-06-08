
find_min_bic.Parent_whitelist_acyclic <- function(BIC_merged_table, Loss_table, npar_table, parents.list_all_threshold) {
  
  set.seed(2023)
  
  print("Starting 'find_min_bic.Parent_whitelist_acyclic' inputs:")
  
  # -------------------- 
  print(paste("Dimensions of BIC_merged_table:", dim(BIC_merged_table)))
  print(paste("Dimensions of npar_table:", dim(npar_table)))
  
  print("npar_table:")
  print(npar_table)
  
  print(paste("Length of parents.list_all_threshold:", length(parents.list_all_threshold)))
  
  print("parents.list_all_threshold:")
  print(names(parents.list_all_threshold))
  
  # print("parents.list_all_threshold:")
  # print(parents.list_all_threshold)
  
  # --------------------
  
  bic_table_temp <- BIC_merged_table
  
  #  minimum BIC values for each row
  bic_min <- apply(bic_table_temp, 1, min)
  bic_min_index <- vector("numeric", nrow(bic_table_temp))
  bic_min_thresh <- character(length(nrow(bic_table_temp)))
  
  for (i in 1:nrow(bic_table_temp)) {
    min_indices <- which(bic_table_temp[i, ] == min(bic_table_temp[i, ]))
    
    if (length(min_indices) > 1) {
      npar_values <- sapply(min_indices, function(index) npar_table[i, index])
      min_npar_val <- min(npar_values)
      min_npar_indices <- which(npar_values == min_npar_val)
      # -------------------------------
      if (length(min_npar_indices) > 1) {
        min_index <- min_indices[min_npar_indices[1]]
      } else {
        min_index <- min_indices[min_npar_indices]
      }
    } else {
      min_index <- min_indices
    }
    
    bic_min_index[i] <- min_index
    bic_min_thresh[i] <- colnames(BIC_merged_table)[min_index]
  }
  
  bic_min_table <- data.frame(Min.BIC = bic_min, Min.BIC_Column.Index = bic_min_index, Min.BIC_Threshold.Column = bic_min_thresh)
  
  # # -----------------------------------------------
  nodes <- rownames(bic_min_table)
  parent_nodes <- lapply(nodes, function(node) {
    min_index <- bic_min_table[node, "Min.BIC_Column.Index"]
    
    # -------------------- 
    if (min_index > length(parents.list_all_threshold) || is.na(min_index)) {
      stop(paste("min_index out of bounds for node:", node, "with min_index:", min_index))
    }
    
    if (!node %in% names(parents.list_all_threshold[[min_index]])) {
      stop(paste("Node:", node, "not found in parents.list_all_threshold at min_index:", min_index))
    }
    # --------------------
    
    parents <- if(is.null(parents.list_all_threshold[[min_index]][[node]])) character(0) else parents.list_all_threshold[[min_index]][[node]]
    
    data.frame(from = parents, to = rep(node, length(parents)))
  })
  
  possible.white.list <- do.call(rbind, parent_nodes)
  
  
  return(list(
    bic_min_table = bic_min_table,
    possible.white.list = possible.white.list
  ))
}
