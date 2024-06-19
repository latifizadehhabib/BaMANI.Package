augmented_edge_table <- function(Black_List,
                                 all_edge_list, 
                                 algorithm_directed, 
                                 Arcs.Cor_streng_table.alg,
                                 algorithm_undirected,
                                 Arcs.Cor_table.alg) {
  
  
  cat("------------------------------", "\n")
  print("Starting augmented_edge_table with inputs:")
  cat("------------------------------", "\n")
  
  # Load dplyr package
  # install.packages("rlang")
  # install.packages("dplyr")
  library("dplyr")
  
  tmp <- all_edge_list


for(algorithm in algorithm_undirected){
  
  Arcs.Cor <- as.data.frame(Arcs.Cor_table.alg[[algorithm]])
  names(Arcs.Cor) <- c("from", "to", paste(algorithm, "_CorSign", sep = ' '))
  tmp <- merge(tmp, Arcs.Cor, by = c("from", "to"), all = TRUE)
  
}

tmp <- tmp %>% distinct(from, to, .keep_all = TRUE) 


for(algorithm in algorithm_directed){
  
  Arcs.Cor_streng <- as.data.frame(Arcs.Cor_streng_table.alg[[algorithm]])
  names(Arcs.Cor_streng) <- c("from", "to", paste(algorithm, "_strength", sep = ' '), paste(algorithm, "_CorSign", sep = ' '))
  tmp <- merge(tmp, Arcs.Cor_streng, by = c("from", "to"), all = TRUE)
  
}

# Remove duplicate rows based on "from" and "to" columns
tmp <- tmp %>% distinct(from, to, .keep_all = TRUE) # distinct() function from dplyr package in tidyverse library to remove duplicate rows. .keep_all argument is set to TRUE so that all other columns in original dataset are kept.

tmp[mapply(is.na, tmp)] <- ""  #tmp[is.na(tmp)] <- 0   # remove any "NA"
# tmp$Edge_No <- as.numeric(tmp$Edge_No)

tmp$Edge_No <- as.numeric(tmp$Edge_No)  #class(tmp$Edge_No)
tmp <- tmp[order(tmp$Edge_No, decreasing = F), ]

augmented_edge_list <- tmp

#auxiliary data
# tmp1 <- tmp
# tmp2 <- tmp


#---------------------------------------------------------------------------------------
# adding column "Hit.count" 
arcs_CorSign <- tmp %>% select(contains("_CorSign"))

# apply() function to count number of "+" and "-" signs in each row # add hit_count as a new column in original data frame
augmented_edge_list$Hit.Count <- apply(arcs_CorSign, 1, function(x) sum(x == "+" | x == "-"))

# ***************important for next section*****************
# augmented_edge_list <- subset(augmented_edge_list, Hit.Count != "0")
rownames(augmented_edge_list) <- NULL

#---------------------------------------------------------------------------------------
# Add a Blacklist (Red) column to Check if row (arc) is in Blacklist # ifelse() function checks if each row in from and to columns of tmp is in set of rows in from and to columns of Black_List, using apply() and paste() to concatenate values in each row into a single string. If row is in Black_List, function returns "1" for that row, otherwise it returns an empty string. resulting Black_Sign variable contains "1" for rows in tmp that are also in Black_List, and an empty string for all other rows.

# Check if Black_List is not NULL and has rows
if (!is.null(Black_List) && nrow(Black_List) > 0) {
  # If Black_List is not empty, proceed with operation
  Black_Sign <- ifelse(apply(tmp %>% select(from, to), 1, function(x) paste(x, collapse = "|")) %in% 
                         apply(Black_List %>% select(from, to), 1, function(x) paste(x, collapse = "|")), "1", "")
  
  # Assign results to augmented_edge_list
  augmented_edge_list$Black_list <- Black_Sign
  
} else {
  # If Black_List is empty or NULL, do nothing (or handle case as needed)
  print("Black_List is empty or NULL; no operations performed.")
}

# -------------------------------------Not_this_direction (A-->B not been learned by any algorithm, zero or empty value for this row )
# "Unclear_direction": When Row A->B and Row B->A have some arc strength columns with non-zero (even for zero) value at same time (which means some algorithm learnt A->B and others B->A, so it's not safe to choose one of them by chance (unclear))

#source("find_unclear_direction.R")  # if add two columns "Not_this_direction" and "unclear_direction"
# debug(find_unclear_direction)
augmented_edge_list <- find_unclear_direction (augmented_edge_list)


#---------------------------------------------------------------------------------------
# Select columns with "_strength" in column name
# Load matrixStats package

strength_cols_names <- grep("_strength", names(tmp), value = TRUE)
strength_cols <- tmp[, strength_cols_names]

# Convert data frame to a numeric data type
#strength_cols_numeric <- apply(strength_cols, 2, as.numeric)
strength_cols_numeric <- data.frame(lapply(strength_cols, function(x) as.numeric(as.character(x))))


min_vals <- rowMins(as.matrix(strength_cols_numeric), na.rm = TRUE)
max_vals <- rowMaxs(as.matrix(strength_cols_numeric), na.rm = TRUE)

# Replace Inf and -Inf with empty
min_vals[min_vals == Inf | min_vals == -Inf] <- ""
max_vals[max_vals == Inf | max_vals == -Inf] <- ""

# Add max values as a new column in data frame
augmented_edge_list$Min.strength <- min_vals
augmented_edge_list$Max.strength <- max_vals

return(augmented_edge_list)

}

