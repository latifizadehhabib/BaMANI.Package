not_this_direction_arcs <- function(edge_info.table) {
  
  print("Starting 'not_this_direction_arcs' inputs:")
  
  # Identify columns with "_strength" in column name
  strength_cols <- grep("_strength", colnames(edge_info.table), value = TRUE)
  
  # tolerance <- 1e-250
  # Identify rows with all zero or empty values in strength columns  
  
  strength_cols <- grep("_strength", colnames(edge_info.table), value = TRUE)
  edge_info.table$Not_this_direction <- ifelse(rowSums(edge_info.table[strength_cols] == 0 | 
                                                         edge_info.table[strength_cols] == "" |
                                                         is.na(edge_info.table[strength_cols])) == length(strength_cols),
                                               "1", "")
  
  
  return(edge_info.table)
}

# -------------------------------------------------------------------------------------------------------- 
# 
# not_this_direction_arcs <- function(edge_info.table) {
#   # Identify columns with "_strength" in column name
#   strength_cols <- grep("_strength", colnames(edge_info.table), value = TRUE)
#   
#   # tolerance <- 1e-250
#   # Identify rows with all zero or empty values in strength columns  
#   edge_info.table$all_zero_or_empty <- (
#     rowSums(edge_info.table[strength_cols] == 0, na.rm = TRUE)== length(strength_cols) |
#       rowSums(edge_info.table[strength_cols] == "", na.rm = TRUE) == length(strength_cols) |
#       rowSums(is.na(edge_info.table[strength_cols])) == length(strength_cols)
#   )
#   
#   
#   
#   # Assign "Not this direction" column with value 1 to those rows
#   edge_info.table$Not_this_direction <- ifelse(edge_info.table$all_zero_or_empty == TRUE, 1, "")
#   
#   # Remove temporary "all_zero_or_empty" column
#   edge_info.table <- edge_info.table[, !grepl("all_zero_or_empty", colnames(edge_info.table))]
#   # rownames(Not_this_direction_rows) <- NULL
#   # edge_info.table[is.na(edge_info.table)] <- ""
#   
#   # Add Not_this_direction column to augmented_edge_list
#   # augmented_edge_list$Not_this_direction <- edge_info.table$Not_this_direction
#   
#   # replace NA with "" in augmented_edge_list
#   # edge_info.table[is.na(edge_info.table)] <- ""
#   # Not_this_direction_column <- edge_info.table$Not_this_direction
#   # 
#   # Not_this_direction_rows <- edge_info.table[Not_this_direction_column == 1, ]
#   # Not_this_direction_rows <- Not_this_direction_rows[!duplicated(Not_this_direction_rows$Edge_No), ]
#   # Not_this_direction_rows <- Not_this_direction_rows[complete.cases(Not_this_direction_rows), ]
#   # rownames(Not_this_direction_rows) <- NULL
#   
#   # return(list(Not_this_direction_column = Not_this_direction_column, Not_this_direction_rows = Not_this_direction_rows))
#   return(edge_info.table)
# }
