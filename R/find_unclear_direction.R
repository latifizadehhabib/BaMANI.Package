# ---------------------------------------------------------
#   # return unique unclear_direction arcs )
find_unclear_direction <- function(augmented_edge_list) {
  
  cat("------------------------------", "\n")
  print("Starting 'find_unclear_direction' inputs:")
  cat("------------------------------", "\n")
  # print(augmented_edge_list)
  # cat("------------------------------", "\n")
  
  
  
    # ------------------------------------------ Not_this_direction
  
  strength_cols <- grep("_strength", colnames(augmented_edge_list), value = TRUE)
  
  # tolerance <- 1e-250
  # Identify rows with all zero or empty values in strength columns  
  
  strength_cols <- grep("_strength", colnames(augmented_edge_list), value = TRUE)
  augmented_edge_list$Not_this_direction <- ifelse(rowSums(augmented_edge_list[strength_cols] == 0 | 
                                                             augmented_edge_list[strength_cols] == "" |
                                                             is.na(augmented_edge_list[strength_cols])) == length(strength_cols),
                                                   "1", "")
  

  # Find pairs of rows that have same Edge_No value
  augmented_edge_list_pairs <- split(augmented_edge_list, f = augmented_edge_list$Edge_No)
  
  # function to apply "unclear_direction" rule
  unclear_direction <- function(augmented_edge_list_pair) {
    if (nrow(augmented_edge_list_pair) == 2) {
      # from1 <- augmented_edge_list_pair$from[1]      # to1 <- augmented_edge_list_pair$to[1]
      # from2 <- augmented_edge_list_pair$from[2]      # to2 <- augmented_edge_list_pair$to[2]
      
      strength_cols <- grep("_strength", names(augmented_edge_list_pair), value = TRUE)
      if ((any(augmented_edge_list_pair[1, strength_cols] > 0) & any(augmented_edge_list_pair[2, strength_cols] > 0))) {
      # if ((any(augmented_edge_list_pair[1, strength_cols] >= 0) & any(augmented_edge_list_pair[2, strength_cols] >= 0))) {
          
        augmented_edge_list_pair$unclear_direction <- 1
      } else {
        augmented_edge_list_pair$unclear_direction <- ""
      }
    }
    return(augmented_edge_list_pair)
  }
  
  # Apply "unclear_direction" rule to each pair of rows
  augmented_edge_list_pairs <- lapply(augmented_edge_list_pairs, unclear_direction)
  
  # Combine pairs back into single dataframe
  augmented_edge_list_result <- do.call(rbind, augmented_edge_list_pairs)
  
  rownames(augmented_edge_list_result) <- NULL
  # Remove original Edge_No column
  augmented_edge_list.unclear.arcs <- augmented_edge_list_result
 
  # Return result and unclear_direction_rows
  return(augmented_edge_list.unclear.arcs)

}