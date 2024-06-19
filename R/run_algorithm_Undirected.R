run_algorithm_Undirected <- function(algorithm_undirected, 
                                     # Arcs.Cor_streng_table.alg,
                                     Blank_edge_list, 
                                     Edge_count, 
                                     discretized_data, 
                                     nboot, cl, corrcoef) {
  
  cat("------------------------------", "\n")
  print("Starting 'run_algorithm_Undirected' inputs:")
  cat("------------------------------", "\n")
  
  # install.packages("rlang")
  # Load dplyr package
  # library(dplyr)
  set.seed(123)  # Setting seed for reproducibility
  
  Arcs.Cor_table.alg = list()
  
  for (algorithm in algorithm_undirected) {
  arstr <- boot.strength(discretized_data, R = nboot, algorithm = algorithm, cluster = cl) # undirected
  
  suppressWarnings( # Use suppressWarnings to skip warning
    ave.dag <- averaged.network(arstr) 
  )
  
  (ars <- arcs(ave.dag))
  if (nrow(ars) == 0) {    # handle case where there are no arcs
  } else {    # proceed with loop
    }
  
  ars <- as.data.frame(ars)
  
  # ------------------------------------------------- newly added
  # plot DAG-----------------------------------------
  # source("calculate_cor_sign.R")
  # CorSign <- calculate_cor_sign(ars, corrcoef)
  # 
  # HLarcs <- ars[CorSign == "-",]
  # arc_str = arc.strength(ave.dag, data = discretized_data)
  # 
  # plot_title <- paste("DAG network: Algorithm", algorithm)
  # strength.plot(ave.dag, arc_str, shape = "ellipse", highlight = list(arcs = HLarcs), main = plot_title)
  # # strength.plot(ave.dag, arc_str, shape = "ellipse", highlight = list(arcs = HLarcs))
  # # graphviz.plot(ave.dag, shape = "ellipse", highlight = list(arcs = HLarcs))
  
  # -------------------------------------------------
  # -------------------------------------------------
  
  
  
  # edge_list------------------------------------------
  list_numb_before <- nrow(Blank_edge_list)
  Edge_count_before <- Edge_count
  
  # Call create_edge_list function to create Blank_edge_list
  #source("create_edge_list.R")
  create_edge <- create_edge_list(Blank_edge_list, Edge_count, ars)
  Blank_edge_list <- create_edge$edge_list
  Edge_count <- create_edge$edge_count
  
  
  # access updated Blank_edge_list and Edge_count from function output
  all_edge_list <- Blank_edge_list
  
 
  # ---------------------------:   # Add reverse of each row after current row
  # Create an empty data frame with same column names
  ars_both <- data.frame(from = character(), to = character())
  # Loop through each row of original data frame and add reverse as a new row after current row
  for (i in 1:nrow(ars)) {
    ars_both <- rbind(ars_both, ars[i, ])
    ars_both <- rbind(ars_both, data.frame(from = ars[i, "to"], to = ars[i, "from"]))
  }
  # Reset row names of new data frame
  rownames(ars_both) <- NULL
  
  # --------------------------- 
  #source("calculate_cor_sign.R")
  CorSign <- calculate_cor_sign(ars_both, corrcoef)
  
  # Create a data frame from columns
  Arcs.Cor_table <- data.frame(ars_both, CorSign)
  
  #  can replace "save" and "write.csv" function
  
  Arcs.Cor_table.alg[[algorithm]] <- Arcs.Cor_table
  

  cat("************************************************************************", "\n")
  
  cat(sprintf("If the list Empty before undirected algorithm <%s> ?  %s\n", algorithm, ifelse(list_numb_before == 0, "YES", "NO")))
  # cat(sprintf("Number of arcs:  ==>  before algorithm: %d  || after algorithm: %d\n", list_numb_before, nrow(Blank_edge_list)))
  cat(sprintf("Number of arcs before: %d  || after: %d runing algorithm \n", list_numb_before, nrow(Blank_edge_list)))
  # cat(sprintf("Number of Edges:  ==>  before running algorithm: %d  || after algorithm: %d\n", Edge_count_before, Edge_count))
  
  
  if (Edge_count_before == Edge_count) {
    cat("-------This algorithm learned some existed Arcs but could not learn more!-------.", "\n")
  } else {
      
    # Insert code to execute if Edge_count_before and Edge_count are not equal
  } 
  
  }
  # cat("************************************************************************", "\n")
  # cat(sprintf("Final raw nuber of Arcs  %d\n", (Edge_count-1)/2))
  
  return(list (all_edge_list = all_edge_list, Arcs.Cor_table.alg =Arcs.Cor_table.alg, ave.dag = ave.dag))
  }
