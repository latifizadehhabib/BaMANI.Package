uninformative_arcs_removal <- function(augmented_edge_list, algorithm_undirected) {
  
  print("Starting 'uninformative_arcs_removal' inputs:")
  
    
  # -------------------------new
  # possible_seed_arcs <- subset(augmented_edge_list, augmented_edge_list$Black_list != 1 |
  #                                augmented_edge_list$unclear_direction != 1 | augmented_edge_list$Not_this_direction != 1)
  # 
  
  arcs.not.Black_list <- subset(augmented_edge_list, augmented_edge_list$Black_list != 1)
  arcs.not.unclear_direction <- subset(arcs.not.Black_list, arcs.not.Black_list$unclear_direction != 1)
  possible_seed_arcs <- subset(arcs.not.unclear_direction, arcs.not.unclear_direction$Not_this_direction != 1)
  
  
  
  # -------------------------old
  # possible_seed_arcs <- subset(augmented_edge_list, augmented_edge_list$Black_list != 1 &
  #                                augmented_edge_list$unclear_direction != 1 & augmented_edge_list$Not_this_direction != 1)
  # # -------------------------
  
  rownames(possible_seed_arcs) <- NULL
  # removed all the rows that had a value "1" in the three columns, now its safe to remove those columns too. 
  possible_seed_arcs <- subset(possible_seed_arcs, select = -c(Black_list, unclear_direction, Not_this_direction))
  
  
  # -----
  
  # reorder the column names
  possible_seed_arcs$Edge_No <- as.numeric(possible_seed_arcs$Edge_No)  #class(tmp$Edge_No)
  possible_seed_arcs <- possible_seed_arcs[order(possible_seed_arcs$Edge_No, decreasing = F), ]
  
# 
  return(possible_seed_arcs)
  
}






# ------------------------------------------------------------------------------------------------------------old

# uninformative_arcs_removal <- function(augmented_edge_list, algorithm_undirected) {
#   
#   # -------------------------new
#   # possible_seed_arcs <- subset(augmented_edge_list, augmented_edge_list$Black_list != 1 |
#   #                                augmented_edge_list$unclear_direction != 1 | augmented_edge_list$Not_this_direction != 1)
#   # 
#   
#   arcs.not.Black_list <- subset(augmented_edge_list, augmented_edge_list$Black_list != 1)
#   arcs.not.unclear_direction <- subset(arcs.not.Black_list, arcs.not.Black_list$unclear_direction != 1)
#   possible_seed_arcs <- subset(arcs.not.unclear_direction, arcs.not.unclear_direction$Not_this_direction != 1)
#   
#   
#   
#   # -------------------------old
#   # possible_seed_arcs <- subset(augmented_edge_list, augmented_edge_list$Black_list != 1 &
#   #                                augmented_edge_list$unclear_direction != 1 & augmented_edge_list$Not_this_direction != 1)
#   # # -------------------------
#   
#   rownames(possible_seed_arcs) <- NULL
#   # removed all the rows that had a value "1" in the three columns, now its safe to remove those columns too. 
#   possible_seed_arcs <- subset(possible_seed_arcs, select = -c(Black_list, unclear_direction, Not_this_direction))
#   
#   
#   # -------------------------------------
#   # I should remove any row with only two "+" or two "-", which related to the undirected algorithms
#   # should remove the columns related to the undirected algorithms, then remove any rows that has new counted Hit.Count" column with "0" using "subset"
#   # then assign that filtered table to the "possible_seed_arcs" and go to the Max and Min step (by this way, will not get any +-inf)
#   # -----
#   
#   auxiliary <- possible_seed_arcs
#   # auxiliary <- auxiliary[, -which(colnames(auxiliary) == "Hit.Count")]
#   # removed_cols_names <- lapply(algorithm_undirected, function(algorithm) grep(paste0(algorithm, "_CorSign", sep = " "), colnames(auxiliary)))
#   # removed_cols_names <- unlist(removed_cols_names)
#   
#   removed_cols_names <- c("mmpc _CorSign", "si.hiton.pc _CorSign")
#   removed_cols <- auxiliary[, removed_cols_names]
#   auxiliary <- auxiliary[, !colnames(auxiliary) %in% removed_cols_names]
#   
#   # for (algorithm in algorithm_undirected) {
#   #   removed_cols_names <- removed_cols_names | grepl(paste(algorithm, "_CorSign", sep = " "), colnames(auxiliary))
#   # }
#   
#   # adding auxiliary column "Hit.count" (this is different from previous because we omitted two columns of undirected Algorithms)
#   auxiliary_arcs_CorSign <- auxiliary %>% select(contains("_CorSign"))
#   
#   # apply() function to count the number of "+" and "-" signs in each row # add hit_count as a new column in the original data frame
#   auxiliary$Hit.Count_auxiliary <- apply(auxiliary_arcs_CorSign, 1, function(x) sum(x == "+" | x == "-"))
#   auxiliary <- subset(auxiliary, Hit.Count_auxiliary != "0")
#   
#   # now all the rows has values for min and max, without any NA
#   auxiliary <- auxiliary[, -which(colnames(auxiliary) == "Hit.Count_auxiliary")]
#   
#   # rownames(auxiliary) <- NULL
#   
#   # merge the removed columns back to the data frame
#   possible_seed_arcs <- merge(removed_cols, auxiliary, by = 0, all.y = TRUE)[-1]
#   
#   # reorder the column names
#   possible_seed_arcs <- possible_seed_arcs %>% select("from", "to", "Edge_No", everything())
#   
#   possible_seed_arcs$Edge_No <- as.numeric(possible_seed_arcs$Edge_No)  #class(tmp$Edge_No)
#   possible_seed_arcs <- possible_seed_arcs[order(possible_seed_arcs$Edge_No, decreasing = F), ]
#   
#   # 
#   return(possible_seed_arcs)
#   
# }



