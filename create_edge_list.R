# function to create Blank_edge_list
create_edge_list <- function(Blank_edge_list, Edge_count, ars) {
  
  set.seed(2023) 
  
  for (i in seq_len(nrow(ars))) {
    # arc is already in Blank_edge_list?
    if (!any(Blank_edge_list$from == ars[i, 1] & Blank_edge_list$to == ars[i, 2])) {
      # If not, add arc and its reverse to Blank_edge_list
      Blank_edge_list[nrow(Blank_edge_list) + 1,] <- c(ars[i,1], ars[i,2], Edge_count)
      Blank_edge_list[nrow(Blank_edge_list) + 1,] <- c(ars[i,2], ars[i,1], Edge_count)
      Edge_count <- Edge_count + 1
    } else {
      
      next
    }
  }
  
  return(list(edge_list = Blank_edge_list, edge_count = Edge_count))
}