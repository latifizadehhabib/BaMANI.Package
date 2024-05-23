run_single_algorithm_directed <- function(algorithm_directed, 
                                          discretized_data, 
                                          nboot, cl, 
                                          Black_List
                                          ) {
  
  
  cat("------------------------------", "\n")
  print("Starting 'run_single_algorithm_directed' inputs:")
  cat("------------------------------", "\n")
  
  set.seed(123)  # Setting seed for reproducibility
  
  # *---------------------------------------
  
  black_list_present <- !is.null(Black_List) && nrow(Black_List) > 0
  
  if (black_list_present) {
    arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm_directed, cluster = cl,
                           algorithm.args = list(blacklist = Black_List))
  } else {
    arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm_directed, cluster = cl)
  }
  
  
  suppressWarnings( # Use suppressWarnings to skip warning
    ave.dag <- averaged.network(arstr) 
  )
  #------------------------------original
  arcs(ave.dag) <- directed.arcs(ave.dag) # ignore undirected arcs
  
  #------------------------------
  ars <- arcs(ave.dag)
  ars <- as.data.frame(ars)
  
  return(list(ars = ars, ave.dag = ave.dag))
}
