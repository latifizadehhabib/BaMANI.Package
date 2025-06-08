augmented_edge_table <- function(Black_List,
                                 ensemble_arc_list, 
                                 algorithm_directed, 
                                 Arcs.Cor_streng_table.alg,
                                 algorithm_undirected,
                                 Arcs.Cor_table.alg) {
  
  set.seed(2023) 
  cat("------------------------------", "\n")
  print("Starting augmented_edge_table with inputs:")
  cat("------------------------------", "\n")
  
  # Load dplyr package
  # install.packages("rlang")
  # install.packages("dplyr")
  # library("dplyr")
  
  tmp <- ensemble_arc_list


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

tmp <- tmp %>% distinct(from, to, .keep_all = TRUE) 

tmp[mapply(is.na, tmp)] <- ""  
# tmp$Edge_No <- as.numeric(tmp$Edge_No)

tmp$Edge_No <- as.numeric(tmp$Edge_No)  #class(tmp$Edge_No)
tmp <- tmp[order(tmp$Edge_No, decreasing = F), ]

augmented_edge_list <- tmp

#----------------------------
# adding column "Hit.count" 
arcs_CorSign <- tmp %>% select(contains("_CorSign"))

augmented_edge_list$Hit.Count <- apply(arcs_CorSign, 1, function(x) sum(x == "+" | x == "-"))

rownames(augmented_edge_list) <- NULL

#---------------------------------------------------------------------------------------
# Check if Black_List is not NULL and has rows
if (!is.null(Black_List) && nrow(Black_List) > 0) {
  # If Black_List is not empty, proceed with operation
  Black_Sign <- ifelse(apply(tmp %>% select(from, to), 1, function(x) paste(x, collapse = "|")) %in% 
                         apply(Black_List %>% select(from, to), 1, function(x) paste(x, collapse = "|")), "1", "")
  
  augmented_edge_list$Black_list <- Black_Sign
  
} else {
  print("Black_List is empty or NULL; no operations performed.")
}

# -------------------------------------Not_this_direction (A-->B not been learned by any algorithm, zero or empty value for this row )
# source("find_unclear_direction.R")  # if add two columns "Not_this_direction" and "unclear_direction"
# debug(find_unclear_direction)
augmented_edge_list <- find_unclear_direction(augmented_edge_list)

#---------------------------------------------------------------------------------------
# Select columns with "_strength" in column name
# Load matrixStats package

strength_cols_names <- grep("_strength", names(tmp), value = TRUE)
strength_cols <- tmp[, strength_cols_names]

#strength_cols_numeric <- apply(strength_cols, 2, as.numeric)
strength_cols_numeric <- data.frame(lapply(strength_cols, function(x) as.numeric(as.character(x))))


min_vals <- rowMins(as.matrix(strength_cols_numeric), na.rm = TRUE)
max_vals <- rowMaxs(as.matrix(strength_cols_numeric), na.rm = TRUE)

min_vals[min_vals == Inf | min_vals == -Inf] <- ""
max_vals[max_vals == Inf | max_vals == -Inf] <- ""

augmented_edge_list$Min.strength <- min_vals
augmented_edge_list$Max.strength <- max_vals

return(augmented_edge_list)

}

