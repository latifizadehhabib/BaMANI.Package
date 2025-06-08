uninformative_arcs_removal <- function(augmented_edge_list) {
  
  set.seed(2023) 
  
  print("Starting 'uninformative_arcs_removal' inputs:")
  
  arcs.not.Black_list <- subset(augmented_edge_list, augmented_edge_list$Black_list != 1)
  arcs.not.unclear_direction <- subset(arcs.not.Black_list, arcs.not.Black_list$unclear_direction != 1)
  possible_seed_arcs <- subset(arcs.not.unclear_direction, arcs.not.unclear_direction$Not_this_direction != 1)
  
  
  rownames(possible_seed_arcs) <- NULL
  possible_seed_arcs <- subset(possible_seed_arcs, select = -c(Black_list, unclear_direction, Not_this_direction))
  
  
  # -----
  
  possible_seed_arcs$Edge_No <- as.numeric(possible_seed_arcs$Edge_No)  
  possible_seed_arcs <- possible_seed_arcs[order(possible_seed_arcs$Edge_No, decreasing = F), ]
  
# 
  return(possible_seed_arcs)
  
}