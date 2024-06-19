run_algorithm_directed <- function(algorithm_directed, Blank_edge_list, Edge_count, 
                                   # discretized_data, nboot, cl, Black_List, white_List= NULL, corrcoef) {
                                   discretized_data, nboot, cl, Black_List, white_List, corrcoef) {
  
 
  cat("------------------------------", "\n")
  print("Starting 'run_algorithm_directed' inputs:")
  cat("------------------------------", "\n")
  
  # For DAGMA algorithm
  # averaging adjacency matrices: (if each network is represented by an adjacency matrix)
  average_networks <- function(network_list) {
    if (length(network_list) == 0) {
      stop("Network list is empty")
    }
    
    n <- nrow(network_list[[1]])
    for (network in network_list) {
      if (nrow(network) != n || ncol(network) != n) {
        stop("All networks must have same size")
      }
    }
    
    avg_matrix <- Reduce('+', network_list) / length(network_list)
    return(avg_matrix)
  }
  # -----------------
  # Function for running bootstrap process with given algorithm
  run_bootstrap_dagma <- function(nboot, Black_List, white_List, discretized_data) {
    bootstrap_results <- list()
    
    n_iterations <- min(nboot, 1)
    
    for(i in 1:n_iterations) {
      bootstrap_sample <- discretized_data[sample(nrow(discretized_data), replace = TRUE), ]
      
      library(reticulate)
      
      # REPLACE PATH TO PYTHON TO WHERE IT IS LOCATED ON YOUR COMPUTER
      # use_python("C:/Users/GON/AppData/Local/Programs/Python/Python310/python.exe", required = TRUE)

      # Convert R data frames to Python objects and assign them to Python variables
      py$bootstrap_sample_py <- r_to_py(bootstrap_sample)
      py$Black_List_py <- r_to_py(Black_List)
      py$white_List_py <- r_to_py(white_List)
      
      # Execute Python code
      py_run_string("
import pandas as pd
from dagma.linear import DagmaLinear

# Convert R data frames to pandas DataFrames
data = pd.DataFrame(bootstrap_sample_py)
blacklist = pd.DataFrame(Black_List_py)
whitelist = pd.DataFrame(white_List_py)

# Create mapping from column names to indices
names_idx = {col: idx for idx, col in enumerate(data.columns)}

# Initialize lists for excluded and included edges
edges_ex = []
edges_in = []

# Populate list of excluded edges from blacklist
n_blacklist, _ = blacklist.shape
for i in range(n_blacklist):
  row = blacklist.values[i].tolist()
  edges_ex.append((names_idx[row[0]], names_idx[row[1]]))

# Populate list of included edges from whitelist
n_whitelist, _ = whitelist.shape
for i in range(n_whitelist):
  row = whitelist.values[i].tolist()
  edges_in.append((names_idx[row[0]], names_idx[row[1]]))

# Fit model with both excluded and included edges
model = DagmaLinear(loss_type='l2')
W_est = model.fit(data.values, lambda1=0.1, exclude_edges=edges_ex, include_edges=edges_in)
")
      
      # Retrieve adjusted matrix and store it
      # Here we directly access Python object without converting, based on your previous preference
      adj_matrix <- py$W_est
      bootstrap_results[[i]] <- adj_matrix
    }
    
    # Compute and return averaged network from bootstrap results
    return(average_networks(bootstrap_results))
  }
  # -----------------
  # --------------------------------------------
  
  # arcs.strength.table.alg = list()
  Arcs.Cor_streng_table.alg = list()
  
  set.seed(123)  # Setting seed for reproducibility
  
  # *---------------------------------------new 2
    
    for (algorithm in algorithm_directed) {
      if (algorithm != "DAGMA") {
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
      } else {
        # Process DAGMA algorithm
        # Note: DAGMA-specific functions and processing are called here
        average_network_dagma <- run_bootstrap_dagma(nboot, Black_List, white_List, discretized_data)
        average_network_dagma_edges <- as.data.frame(which(average_network_dagma != 0, arr.ind = TRUE))
        names(average_network_dagma_edges) <- c("from", "to")
        
        # Map numeric indices to column names
        col_names <- colnames(discretized_data)
        average_network_dagma_edges$from <- col_names[average_network_dagma_edges$from]
        average_network_dagma_edges$to <- col_names[average_network_dagma_edges$to]
      }
        # white_list_present <- !is.na(white_List) && nrow(white_List) > 0
        # white_list_present <- !is.null(white_List) && !is.na(white_List) && length(white_List) > 0
        
        
      # }
  
 
    #Average model:
    
    suppressWarnings( # Use suppressWarnings to skip warning
      ave.dag <- averaged.network(arstr) 
    )
    #------------------------------original
    arcs(ave.dag) <- directed.arcs(ave.dag) # ignore undirected arcs

    #------------------------------
    ars <- arcs(ave.dag)
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
    # # strength.plot(ave.dag, arc_str, shape = "ellipse", highlight = list(arcs = HLarcs))
    # strength.plot(ave.dag, arc_str, shape = "ellipse", highlight = list(arcs = HLarcs), main = plot_title)
    # # graphviz.plot(ave.dag, shape = "ellipse", highlight = list(arcs = HLarcs))
    
    # -------------------------------------------------
    # -------------------------------------------------
    
    
    
    list_numb_before <- nrow(Blank_edge_list)
    Edge_count_before <- Edge_count
    
    # Call create_edge_list function to create Blank_edge_list
    source("create_edge_list.R")
    create_edge <- create_edge_list(Blank_edge_list, Edge_count, ars)
    Blank_edge_list <- create_edge$edge_list
    Edge_count <- create_edge$edge_count
    
    # -------------------------------------------
    source("calculate_cor_sign.R")
    CorSign <- calculate_cor_sign(ars, corrcoef)
    
    # Create data frame from columns
    Arcs.Cor_streng_table <- data.frame(arc.strength(ave.dag, discretized_data), CorSign)
    
    # ---------------------------------
    #  can replace "save" and "write.csv" function
    Arcs.Cor_streng_table.alg[[algorithm]] <- Arcs.Cor_streng_table
    
    cat("************************************************************************", "\n")
    
    cat(sprintf("If the list Empty before directed algorithm <%s> ?  %s\n", algorithm, ifelse(list_numb_before == 0, "YES", "NO")))
    cat(sprintf("Number of arcs before: %d  || after: %d runing algorithm \n", list_numb_before, nrow(Blank_edge_list)))
    # cat(sprintf("Number of Edges:  ==>  before running algorithm: %d   || after algorithm: %d\n", Edge_count_before-1, Edge_count-1))
    
    if (Edge_count_before == Edge_count) {
      cat("-------This algorithm learned some existed Arcs but could not learn more!-------.", "\n")
    } else {
      # Insert code to execute if Edge_count_before and Edge_count are not equal
    }
    }
  return(list(Arcs.Cor_streng_table.alg = Arcs.Cor_streng_table.alg, 
              # arcs.strength.table.alg = arcs.strength.table.alg, 
              edge_list = Blank_edge_list, 
              edge_count = Edge_count, ave.dag = ave.dag))
  
}
