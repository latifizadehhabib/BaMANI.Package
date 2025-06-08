run_algorithm_Undirected <- function(algorithm_undirected, 
                                     Blank_edge_list, 
                                     Edge_count, 
                                     discretized_data, 
                                     nboot, cl, corrcoef) {
  
  
  set.seed(2023) 
  
  cat("------------------------------", "\n")
  print("Starting 'run_algorithm_Undirected' inputs:")
  cat("------------------------------", "\n")
  
  # install.packages("rlang")
  # Load dplyr package
  # library(dplyr)
  
  # ------------------- 
  Blank_edge_list_temp <- Blank_edge_list
  Edge_count_temp <- Edge_count
  # ------------------- 
  
  Arcs.Cor_table.alg = list()
  
  for (algorithm in algorithm_undirected) {
    set.seed(2023) 
  arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl) # undirected
  
  suppressWarnings( 
    ave.dag <- averaged.network(arstr) 
  )
  
  (ars <- arcs(ave.dag))
  
  
  ars <- as.data.frame(ars)
  
  # print arcs for the specific algorithm
  message(sprintf("Arcs for algorithm <%s>:", algorithm))
  message(capture.output(print(ars)))
  
  # cat(sprintf("Arcs for algorithm <%s>:\n", algorithm))
  # print(ars)
  
  # edge_list------------------------------------------
  list_numb_before <- nrow(Blank_edge_list_temp)
  Edge_count_before <- Edge_count_temp
  
  source("create_edge_list.R")
  create_edge <- create_edge_list(Blank_edge_list_temp, Edge_count_temp, ars)
  Blank_edge_list_temp <- create_edge$edge_list
  Edge_count_temp <- create_edge$edge_count
  
  
  # access updated Blank_edge_list_temp and Edge_count from function output
  # ---------------- 
  # ensemble_arc_list <- Blank_edge_list_temp
  
  # ---------------------------:   
  ars_both <- data.frame(from = character(), to = character())
  for (i in 1:nrow(ars)) {
    ars_both <- rbind(ars_both, ars[i, ])
    ars_both <- rbind(ars_both, data.frame(from = ars[i, "to"], to = ars[i, "from"]))
  }
  rownames(ars_both) <- NULL
  
  # --------------------------- 
  source("calculate_cor_sign.R")
  
  # ------------- 
  CorSign <- calculate_cor_sign(ars, corrcoef)
  # -------------
  # CorSign <- calculate_cor_sign(ars_both, corrcoef)
  
  Arcs.Cor_table <- data.frame(ars_both, CorSign)
  
  Arcs.Cor_table.alg[[algorithm]] <- Arcs.Cor_table
  

  cat("************************************************************************", "\n")
  
  cat(sprintf("If the list Empty before undirected algorithm <%s> ?  %s\n", algorithm, ifelse(list_numb_before == 0, "YES", "NO")))
  cat(sprintf("Number of arcs before: %d  || after: %d runing algorithm \n", list_numb_before, nrow(Blank_edge_list_temp)))

  
  if (Edge_count_before == Edge_count_temp) {
    cat("-------This algorithm learned some existed Arcs but could not learn more!-------.", "\n")
  } else {
      
  } 
  
  }

  return(list (ensemble_arc_list = Blank_edge_list_temp, Arcs.Cor_table.alg =Arcs.Cor_table.alg))


  }
