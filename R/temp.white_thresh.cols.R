temp.white_thresh.cols <- function(temp_possible_seed_arcs, threshold) {
  
  
  cat("------------------------------", "\n")
  print("Starting 'temp.white_thresh.cols' inputs:")
  cat("------------------------------", "\n")
  
  

threshold <- c(as.integer(0), threshold)
# Create columns for each threshold value
column_names <- sapply(threshold, function(x) paste0("Include_[", x, "]"))
temp_possible_seed_arcs[column_names] <- ""

# Loop through each threshold value and each edge
temp_list <- vector("list", length(threshold))
names(temp_list) <- paste0("temp_white_[", threshold, "]")
for (i in seq_along(threshold)) {
  if (i==1){
    # -----------------new
    col_name <- column_names[i]
    temp_possible_seed_arcs[ , col_name] <- ifelse(as.numeric(temp_possible_seed_arcs[ , "Min.strength"]) <= threshold[i], 1L, "")
    # temp_possible_seed_arcs[inx, col_name] <- ifelse(temp_possible_seed_arcs[inx, "Min_strength"] <= threshold[i], 1L, "")
    
    #------------------
    temp_list[[i]] <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE) 
  } else{
    for (j in unique(temp_possible_seed_arcs$Edge_No)) {
      inx <- which(temp_possible_seed_arcs$Edge_No == j)
      col_name <- column_names[i]
      temp_possible_seed_arcs[inx, col_name] <- ifelse(as.numeric(temp_possible_seed_arcs[inx, "Min.strength"]) <= threshold[i], 1L, "")
      # temp_possible_seed_arcs[inx, col_name] <- ifelse(temp_possible_seed_arcs[inx, "Min_strength"] <= threshold[i], 1L, "")
      
      if (any(temp_possible_seed_arcs[inx, col_name] == 1L)) {
      # if (nrow(temp_possible_seed_arcs[inx, c("from", "to")]) > 0) {
        temp_list[[i]] <- rbind(temp_list[[i]], temp_possible_seed_arcs[inx, c("from", "to")])
      }
    }
  }
  
  
  if (is.data.frame(temp_list[[i]])) {
    colnames(temp_list[[i]]) <- c("from", "to")
  }
  # colnames(temp_list[[i]]) <- c("from", "to")
}

# ************************************************************************

# Define the new empty list
empty_list <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE) 
all_list <- data.frame(from = temp_possible_seed_arcs$from, to = temp_possible_seed_arcs$to)  

# add empty list for zero threshold to the left and add all_list for no threshold to the right
temp_list_merge <- c(list(empty_list), temp_list, list(all_list)) # concatenate new_list to the beginning of temp_list
names(temp_list_merge) <- c("empty_graph", names(temp_list), "all_possible_arc" )

  # temp_list[[1]] <- data.frame(from = character(), to = character())
 # temp_list <- append(temp_list, list(list(from = character(), to = character())), after = 0)


# -----------------new
# names(temp_possible_seed_arcs) <- c(column_names, "Include_[all]")
# temp_possible_seed_arcs[ , "Include_[all]"] <- 0L
# temp_possible_seed_arcs[, "Include_[all]"] <- ifelse(as.numeric(temp_possible_seed_arcs[, "Min_strength"]) <= 1, 1L, "")

#------------------


# temp_white_thresh_list <- vector("list", length(temp_list_merge))  # for storing nrows and thresh value for plot...

# Print out data frames for each threshold value
for (i in seq_along(temp_list_merge)) {
  cat("************************************************************************", "\n")
  
  cat(paste("Threshold category:", names(temp_list_merge)[i], "\n"))
  cat(paste("Number of Arcs:", nrow(temp_list_merge[[i]]), "\n"))
  print(temp_list_merge[[i]])
  
  
  # temp_white_thresh_list[[i]] <- rbind(temp_white_thresh_list[[i]], temp_possible_seed_arcs[inx, c("from", "to")])
  
  
  temp_arcs_thresh <- temp_list_merge[[i]]
  # save(temp_arcs_thresh, file = paste("temp_white[", names(temp_list_merge), ".rda", sep = ''))
  # save(temp_arcs_thresh, file = paste("temp_white[", names(temp_list_merge)[i], "].rda", sep = ''))
  # 
  # file_name <- paste("temp_white[", names(temp_list_merge)[i], ".csv", sep = '')
  # write.csv(temp_arcs_thresh, file_name, row.names=FALSE)
  
  cat("************************************************************************", "\n")
  
}

# ---------------------------
# Identify columns ending with .strength or _strength
# cols_to_round <- grep("\\.strength$|_strength$", names(temp_possible_seed_arcs), value = TRUE)
# 
# # For each column we want to round, check if it's numeric. If it is, round it.
# for (col in cols_to_round) {
#   if (is.numeric(temp_possible_seed_arcs[[col]])) {
#     temp_possible_seed_arcs[[col]] <- as.numeric(sprintf("%.2f", temp_possible_seed_arcs[[col]]))
#   }
# }
# ---------------------------
# cols_to_round <- grep("\\.strength|_strength", names(temp_possible_seed_arcs))
# 
# for(col in cols_to_round){
#   temp_possible_seed_arcs[[col]] <- signif(as.numeric(temp_possible_seed_arcs[[col]]), 3)
# }
#---------------------------

cols_to_round <- grep("\\.strength|_strength", names(temp_possible_seed_arcs))

for(col in cols_to_round){
  temp_possible_seed_arcs[[col]] <- sprintf("%.2g", signif(as.numeric(temp_possible_seed_arcs[[col]]), 2))
  # temp_possible_seed_arcs[[col]] <- as.numeric(sprintf("%.2g", signif(as.numeric(temp_possible_seed_arcs[[col]]), 3)))
}
#---------------------------

# cols_to_round <- grep("\\.strength|_strength", names(temp_possible_seed_arcs))
# 
# for(col in cols_to_round){
#   # Check if the column is all numeric
#   if (all(!is.na(as.numeric(temp_possible_seed_arcs[[col]])))) {
#     temp_possible_seed_arcs[[col]] <- signif(as.numeric(temp_possible_seed_arcs[[col]]), 3)
#   } else {
#     # Print a message if the column has non-numeric values
#     cat("Column", col, "contains non-numeric values and will not be rounded.\n")
#   }
# }
#---------------------------


augmneted.thresh.cols <- temp_possible_seed_arcs

# Get number of elements for each list
num.white_thresh <- sapply(temp_list_merge, function(x) dim(x)[1])  # number of white list arcs in each threshold
num.white_thresh <- as.data.frame(num.white_thresh)


cat(paste("For each Threshold: list of arcs have been saved in ''temp_list_merge' and the augmented matrix returned", "\n"))
return(list(temp_list_merge = temp_list_merge, augmneted.thresh.cols = augmneted.thresh.cols, num.white_thresh = num.white_thresh))

}




# WORKED WELL -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# 
# temp_white_thresh <- function(temp_possible_seed_arcs, threshold) {
#   
#   # Create columns for each threshold value
#   column_names <- sapply(threshold, function(x) paste0("Include_[", x, "]"))
#   temp_possible_seed_arcs[column_names] <- ""
#   
#   # Loop through each threshold value and each edge
#   temp_list <- vector("list", length(threshold))
#   names(temp_list) <- paste0("temp_white_[", threshold, "]")
#   for (i in seq_along(threshold)) {
#     for (j in unique(temp_possible_seed_arcs$Edge_No)) {
#       inx <- which(temp_possible_seed_arcs$Edge_No == j)
#       col_name <- column_names[i]
#       temp_possible_seed_arcs[inx, col_name] <- ifelse(temp_possible_seed_arcs[inx, "Min_strength"] <= threshold[i], 1L, "")
#       if (any(temp_possible_seed_arcs[inx, col_name] == 1L)) {
#         temp_list[[i]] <- rbind(temp_list[[i]], temp_possible_seed_arcs[inx, c("from", "to")])
#       }
#     }
#     
#     if (is.data.frame(temp_list[[i]])) {
#       colnames(temp_list[[i]]) <- c("from", "to")
#     }
#     # colnames(temp_list[[i]]) <- c("from", "to")
#   }
#   
#   # Print out data frames for each threshold value
#   for (i in seq_along(temp_list)) {
#     cat("************************************************************************", "\n")
#     
#     cat(paste("Threshold:", format(threshold[i], scientific = TRUE), "\n"))
#     cat(paste("Number of Arcs:", nrow(temp_list[[i]]), "\n"))
#     print(temp_list[[i]])
#     
#     temp_arcs_thresh <- temp_list[[i]]
#     save(temp_arcs_thresh, file = paste("arcs_thresh_2[", format(threshold[i], scientific = TRUE), ".rda", sep = ''))
#     file_name <- paste("arcs_thresh_2[", format(threshold[i], scientific = TRUE), ".csv", sep = '')
#     write.csv(temp_arcs_thresh, file_name, row.names=FALSE)
#     
#     cat("************************************************************************", "\n")
#     
#   }
#   
#   # Define the new empty list
#   empty_list <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE) 
#   all_list <- data.frame(from = temp_possible_seed_arcs$from, to = temp_possible_seed_arcs$to)  
#   
#   # add empty list for zero threshold to the left and add all_list for no threshold to the right
#   temp_list_merge <- c(list(empty_list), temp_list, list(all_list)) # concatenate new_list to the beginning of temp_list
#   names(temp_list_merge) <- c("empty_list", names(temp_list), "all_list" )
#   
#   # temp_list[[1]] <- data.frame(from = character(), to = character())
#   # temp_list <- append(temp_list, list(list(from = character(), to = character())), after = 0)
#   cat("************************************************************************", "\n")
#   
#   cat(paste("Threshold: zero","\n"))
#   cat(paste("Number of Arcs:", (nrow(temp_list_merge[[1]])), "\n"))
#   print(temp_list_merge[[1]])
#   
#   cat("************************************************************************", "\n")
#   
#   cat(paste("For each Threshold: list of arcs have been saved in ''temp_list_merge' and the augmented matrix returned", "\n"))
#   return(temp_list_merge)
# }
