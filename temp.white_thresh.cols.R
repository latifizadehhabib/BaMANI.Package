temp.white_thresh.cols <- function(filtered.seed_arcs.table, threshold) {
  
  
  set.seed(2023) 
  
  cat("------------------------------", "\n")
  print("Starting 'temp.white_thresh.cols' inputs:")
  cat("------------------------------", "\n")
  
column_names <- sapply(threshold, function(x) paste0("Include_[", x, "]"))
filtered.seed_arcs.table[column_names] <- ""

temp_list <- vector("list", length(threshold))
# names(temp_list) <- paste0("temp_white_[", threshold, "]")
names(temp_list) <- paste0("Whitelist [Threshold: ", threshold, "]")

for (i in seq_along(threshold)) {
  col_name <- column_names[i]
  # filtered.seed_arcs.table[, col_name] <- ifelse(as.numeric(filtered.seed_arcs.table[, "Min.strength"]) <= threshold[i], 1L, "")
  temp_list[[i]] <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
  
  cat("------------------------------", "\n")
  print(paste("temp_list for threshold [", i, "]:"))
  cat("------------------------------", "\n")

  for (j in unique(filtered.seed_arcs.table$Edge_No)) {
    inx <- which(filtered.seed_arcs.table$Edge_No == j)
    filtered.seed_arcs.table[inx, col_name] <- ifelse(as.numeric(filtered.seed_arcs.table[inx, "Min.strength"]) <= threshold[i], 1L, "")
    
    
    if (any(filtered.seed_arcs.table[inx, col_name] == 1L)) {
      temp_list[[i]] <- rbind(temp_list[[i]], filtered.seed_arcs.table[inx, c("from", "to")])
      
      cat("------------------------------", "\n")
      #print("temp_list[[i]]:")
      #print(paste("temp_list[",i,"]:" ))
      print(paste("  Edge_No [", j, "]:"))
      cat("------------------------------", "\n")
      print(temp_list[[i]])
      
    }
  }
  
  if (is.data.frame(temp_list[[i]])) {
    colnames(temp_list[[i]]) <- c("from", "to")
  }
}
# ************************************************************************
names(temp_list)[length(temp_list)] <- "all_possible_arc"

temp_white_list_merge  <- temp_list

for (i in seq_along(temp_white_list_merge)) {
  cat("************************************************************************", "\n")
  
  cat(paste("Threshold category:", names(temp_white_list_merge)[i], "\n"))
  cat(paste("Number of Arcs:", nrow(temp_white_list_merge[[i]]), "\n"))
  print(temp_white_list_merge[[i]])
  cat("************************************************************************", "\n")
  
}

# browser()

cols_to_round <- grep("\\.strength|_strength", names(filtered.seed_arcs.table))

# ---------  
for (col in cols_to_round) {
  filtered.seed_arcs.table[[col]] <- as.numeric(lapply(filtered.seed_arcs.table[[col]], function(x) {
    if (is.numeric(x)) {
      signif(x, digits = 4)
    } else {
      signif(as.numeric(x), digits = 4)
    }
  }))
}
#---------------------------
augmented.arcs.table.thresh.cols <- filtered.seed_arcs.table


cat(paste("For each Threshold: list of arcs have been saved in ''temp_white_list_merge' and the augmented matrix returned", "\n"))
return(list(temp_whitelist_threshold_merge_before_cycle.check = temp_white_list_merge, augmented.arcs.table.thresh.cols = augmented.arcs.table.thresh.cols
))

}


