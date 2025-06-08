run_algorithm_directed <- function(algorithm_directed, Blank_edge_list, Edge_count, 
                                   discretized_data, nboot, cl, Black_List, white_List, corrcoef) {
  
  set.seed(2023) 
  
 
  cat("------------------------------", "\n")
  print("Starting 'run_algorithm_directed' inputs:")
  cat("------------------------------", "\n")

  # arcs.strength.table.alg = list()
  # -------------------
  Blank_edge_list_temp <- Blank_edge_list
  Edge_count_temp <- Edge_count
  # ------------------- 
  Arcs.Cor_streng_table.alg = list()
  Single_Algorithm_DAG_data_list <- list()  
  
  
  # set.seed(2023)  
  
    for (algorithm in algorithm_directed) {
      
      set.seed(2023)  
      
      white_list_present <- !is.null(white_List) && nrow(white_List) > 0
      black_list_present <- !is.null(Black_List) && nrow(Black_List) > 0
      
      if (white_list_present && black_list_present) {
        arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl,
                               algorithm.args = list(whitelist = white_List, blacklist = Black_List))
      } else if (white_list_present) {
        arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl,
                               algorithm.args = list(whitelist = white_List))
      } else if(black_list_present) {
        arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl,
                               algorithm.args = list(blacklist = Black_List))
      }
      else if (!white_list_present && !black_list_present) {
        arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl)
        # arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl, algorithm.args = list())
      }
      
 
    #Average model:
    
    suppressWarnings( # 
      ave.dag <- averaged.network(arstr) 
    )
    #------------------------------
    arcs(ave.dag) <- directed.arcs(ave.dag) 

    ars <- arcs(ave.dag)
    ars <- as.data.frame(ars)
    
    #------------------------------ 
    #fit_dag_data <- bn.fit(ave.dag, data = data)
    fit_dag_data <- bn.fit(ave.dag, data = discretized_data)
    
    BRCA_str = arc.strength(ave.dag, data = discretized_data)
    weight.strength <- BRCA_str$strength
    
    # ----------------------------------
    arc_slopes <- data.frame(from = character(), to = character(), slope = numeric())
    
    
    for(node in nodes(fit_dag_data)) {
      parents <- parents(fit_dag_data, node)
      
      for(parent in parents) {
        slope <- coef(fit_dag_data)[[node]][[parent]]
        
        arc_slopes <- rbind(arc_slopes, data.frame(from = parent, to = node, slope = slope))
      }
    }
    arcs_strength <- data.frame(from = as.character(ars[, 1]), 
                                to = as.character(ars[, 2]), 
                                P_strength = weight.strength)
    
    # arc_slopes.strength <- merge(arcs_strength, arc_slopes)
    arc_slopes.strength <- merge(arcs_strength, arc_slopes, by = c("from", "to"), all = TRUE)
    
    
    #------------------------------------------------  visNetwork
    transformed_values <- sapply(arcs_strength$P_strength, function(p) -log10(p))
    # transformed_values <- sapply(P_strength, function(p) -log10(p))
    
    
    max_value <- max(transformed_values[!is.infinite(transformed_values)], na.rm = TRUE)
    transformed_values[is.infinite(transformed_values)] <- max_value
    
    normalized_weights <- rescale(transformed_values, to = c(0, 1))
    #------------------------------------------------ 
    CorSign <- calculate_cor_sign(ars, corrcoef)
    
    HLarcs <- ars[CorSign == "-",]
    
    arc_slopes.strength$key <- with(arc_slopes.strength, paste(from, to, sep = "_"))
    HLarcs_with_key <- transform(HLarcs, key = paste(from, to, sep = "_"))
    
    in_HLarcs <- arc_slopes.strength$key %in% HLarcs_with_key$key
    
    arc_slopes.strength$key <- NULL
    
    
    DAG_detail <- data.frame(
      from = arc_slopes.strength$from, 
      to = arc_slopes.strength$to, 
      # Arc_Color = ifelse(in_HLarcs, "red", "black"), 
      Arc_Color = ifelse(arc_slopes.strength$slope > 0, "black", "red"),

      Effect_Size = paste0("  ", as.character(signif(arc_slopes.strength$slope, digits = 2)), "  "),  
      Arc_Strength = paste0("  ", as.character(signif(normalized_weights, digits = 2)), "  ")  
    )
    #------------------------------------------------   
    message(sprintf("Arcs for algorithm <%s>:", algorithm))
    message(capture.output(print(ars)))
    
    # cat(sprintf("Arcs for algorithm <%s>:\n", algorithm))
    # print(ars)
    
    # -------------------------------------------------
    list_numb_before <- nrow(Blank_edge_list_temp)
    Edge_count_before <- Edge_count_temp
    
    # create_edge_list function to create Blank_edge_list_temp
    source("create_edge_list.R")
    create_edge <- create_edge_list(Blank_edge_list_temp, Edge_count_temp, ars)
    Blank_edge_list_temp <- create_edge$edge_list
    Edge_count_temp <- create_edge$edge_count
    
    # -------------------------------------------
    source("calculate_cor_sign.R")
    CorSign <- calculate_cor_sign(ars, corrcoef)
    
    Arcs.Cor_streng_table <- data.frame(arc.strength(ave.dag, discretized_data), CorSign)
    
    # ---------------------------------
    Arcs.Cor_streng_table.alg[[algorithm]] <- Arcs.Cor_streng_table
    
    cat("************************************************************************", "\n")
    
    cat(sprintf("If the list Empty before directed algorithm <%s> ?  %s\n", algorithm, ifelse(list_numb_before == 0, "YES", "NO")))
    cat(sprintf("Number of arcs before: %d  || after: %d runing algorithm \n", list_numb_before, nrow(Blank_edge_list_temp)))
    # cat(sprintf("Number of Edges:  ==>  before running algorithm: %d   || after algorithm: %d\n", Edge_count_before-1, Edge_count-1))
    
    if (Edge_count_before == Edge_count_temp) {
      cat("-------This algorithm learned some existed Arcs but could not learn more!-------.", "\n")
    } else {
      
    }
    
    Single_Algorithm_DAG_data_list[[algorithm]] <- list(ars = ars, ave.dag = ave.dag, DAG_detail = DAG_detail)  
    
    
    }
  return(list(Arcs.Cor_streng_table.alg = Arcs.Cor_streng_table.alg, 
              # arcs.strength.table.alg = arcs.strength.table.alg, 
              edge_list = Blank_edge_list_temp, 
              # edge_count = Edge_count, ave.dag = ave.dag))
              edge_count = Edge_count_temp, 
              Single_Algorithm_DAG_data_list = Single_Algorithm_DAG_data_list))
  
  
}
