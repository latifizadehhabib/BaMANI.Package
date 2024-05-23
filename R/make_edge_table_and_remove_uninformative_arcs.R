# Making edge table & removing uninformative arcs
make_edge_table_and_remove_uninformative_arcs <- function(algorithm_results, algorithm_directed, algorithm_undirected, threshold.level) {
  
  
  
  print("Starting 'make_edge_table_and_remove_uninformative_arcs' inputs:")
  
  
  source("augmented_edge_table.R")
  source("uninformative_arcs_removal.R")
  
  augmented_edge_list <- augmented_edge_table(algorithm_results$all_arc_list$all_edge_list, 
                                              algorithm_directed, 
                                              algorithm_results$result_directed$Arcs.Cor_streng_table.alg,
                                              algorithm_undirected,
                                              algorithm_results$all_arc_list$Arcs.Cor_table.alg) 
  possible_seed_arcs_filter <- uninformative_arcs_removal(augmented_edge_list, algorithm_undirected)
  
  source("finding_threshold_values.R")
  threshold <- finding_threshold_values(possible_seed_arcs_filter, threshold.level)
  
  return(list(augmented_edge_list = augmented_edge_list, possible_seed_arcs_filter = possible_seed_arcs_filter, threshold = threshold))
}