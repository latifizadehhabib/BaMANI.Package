run_single_algorithm_directed <- function(algorithm_directed, 
                                          discretized_data, 
                                          nboot, cl, 
                                          corrcoef,
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
  
  #------------------------------ recently added
  
  #fit_dag_data <- bn.fit(ave.dag, data = data)
  fit_dag_data <- bn.fit(ave.dag, data = discretized_data)
  
  BRCA_str = arc.strength(ave.dag, data = discretized_data)
  weight.strength <- BRCA_str$strength
  
  # ----------------------------------
  arc_slopes <- data.frame(from = character(), to = character(), slope = numeric())
  

  # iterate over all nodes in network
  for(node in nodes(fit_dag_data)) {
    # get names of parent nodes
    parents <- parents(fit_dag_data, node)
    
    # iterate over all parent nodes & get slope coefficients
    for(parent in parents) {
      # get slope coefficient
      slope <- coef(fit_dag_data)[[node]][[parent]]
      
      # add arc & slope coefficient to data frame
      arc_slopes <- rbind(arc_slopes, data.frame(from = parent, to = node, slope = slope))
    }
  }
  arcs_strength <- data.frame(from = as.character(ars[, 1]), 
                              to = as.character(ars[, 2]), 
                              P_strength = weight.strength)
  
  # arc_slopes.strength <- merge(arcs_strength, arc_slopes)
  arc_slopes.strength <- merge(arcs_strength, arc_slopes, by = c("from", "to"), all = TRUE)
  
  
  #------------------------------------------------  visNetwork
  # Apply transformation
  transformed_values <- sapply(arcs_strength$P_strength, function(p) -log10(p))
  # transformed_values <- sapply(P_strength, function(p) -log10(p))
  
  
  # Handle infinite values after transformation
  max_value <- max(transformed_values[!is.infinite(transformed_values)], na.rm = TRUE)
  transformed_values[is.infinite(transformed_values)] <- max_value
  
  # Normalize transformed values
  normalized_weights <- rescale(transformed_values, to = c(0, 1))
  #------------------------------------------------ 
   CorSign <- calculate_cor_sign(ars, corrcoef)
  
  HLarcs <- ars[CorSign == "-",]
  
  # ----------------------------------- Replace arc_slopes.strength$slope < 0  with  ars$CorSign == "-"
  
  # HLarcs' is subset datafrme containing only negative correlations
  # Create a key column in arc_slopes.strength & temporarily in HLarcs for comparison
  arc_slopes.strength$key <- with(arc_slopes.strength, paste(from, to, sep = "_"))
  HLarcs_with_key <- transform(HLarcs, key = paste(from, to, sep = "_"))
  
  # Create a logical vector for checking presence in HLarcs using temporary 'key' column
  in_HLarcs <- arc_slopes.strength$key %in% HLarcs_with_key$key
  
  # Remove key column from arc_slopes.strength after its use
  arc_slopes.strength$key <- NULL
  

  # Ensure final DAG detail also uses this color logic
  DAG_detail <- data.frame(
    from = arc_slopes.strength$from, 
    to = arc_slopes.strength$to, 
    color = ifelse(in_HLarcs, "red", "black"), 
    Effect_Size = paste0("  ", as.character(signif(arc_slopes.strength$slope, digits = 2)), "  "),  # Added spaces
    Arc_strength = paste0("  ", as.character(signif(normalized_weights, digits = 2)), "  ")   # Use normalized arc strength as value
  )
  
  # ----------------------------------
  
  
  return(list(ars = ars, ave.dag = ave.dag, DAG_detail = DAG_detail))
}
