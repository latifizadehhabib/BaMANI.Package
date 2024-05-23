find_min_bic.Parent_whitelist_acyclic <- function(BIC_merged_table, npar_merged, parents_list_merged) {
  
  
  print("Starting 'find_min_bic.Parent_whitelist_acyclic' inputs:")
  
  # library(igraph)
  bic_table_temp<- BIC_merged_table
  
  bic_min <- apply(bic_table_temp, 1, min)
  
  bic_min_index <- vector("numeric", nrow(bic_table_temp))
  
  for (i in 1:nrow(bic_table_temp)) {
    min_indices <- which(bic_table_temp[i,] == min(bic_table_temp[i,]))
    if (length(min_indices) > 1) {
      parent_cols <- colnames(npar_merged)
      parent_counts <- numeric(length(parent_cols))
      
      for (j in seq_along(parent_cols)) {
        parent_counts[j] <- npar_merged[i, parent_cols[j]]
        # parent_counts[j] <- min(npar_merged[i, parent_cols[j]], na.rm = TRUE)
      }
      #-------------------
      min_parent_count <- min(parent_counts[min_indices], na.rm = TRUE)
      
      min_parent_col <- parent_cols[parent_counts == min_parent_count][1]
      # min_parent_col <- parent_cols[parent_counts == min_parent_count][1]
      
      min_index <- match(min_parent_col,  colnames(npar_merged))
      # min_index <- match(min_parent_col, colnames(BIC_merged_table))
    } else {
      min_index <- which(bic_table_temp[i,] == min(bic_table_temp[i,]))
    }
    bic_min_index[i] <- min_index
  }
  
  bic_min_table <- data.frame(bic_min = bic_min, bic_min_index = bic_min_index)
  

  # Find parent nodes for each variable (node)
  nodes <- rownames(bic_min_table)
  parent_nodes <- lapply(nodes, function(node) {
    # index of row with minimum BIC score for this node
    min_index <- bic_min_table[node, "bic_min_index"]
    # Get parents for this variable based on index
    # parents <- parents_list_merged[[min_index]][[node]]
    parents <- if(is.null(parents_list_merged[[min_index]][[node]])) 
      character() 
    else 
      parents_list_merged[[min_index]][[node]]
    # Return a data frame with node name its parent nodes
    # data.frame(from = rep(node, length(parents)), to = parents)
    data.frame(from = parents, to = rep(node, length(parents)))
  })
  
  # Combine parent nodes for all variables into a single data frame
  possible.white.list <- do.call(rbind, parent_nodes)
  
 
  return(list(
    bic_min_table = bic_min_table, 
    possible.white.list = possible.white.list
  ))
}
