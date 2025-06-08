
calculate_loss_npar_table <- function(threshold, temp_list_merge, discretized_data, data, nboot, cl, Black_List, corrcoef) {

  # library(igraph)
  set.seed(2023)
  
  
  npar_list <- list()
  L1_list <- list()
  parents_list_per.thresh = list()
  
  # ------------- 
  # threshold <- c(0, threshold, "all")
  # threshold <- c(threshold, "all")
  
  num_sample <- nrow(discretized_data)
  
  # threshold <- c(as.character(0), as.character(threshold), "all")
  num_arcs_DAG_per.thresh <- c()
  num_arcs_DAG_per.thresh_name <- c()

  BIC.threshold.colname <- c()
  BIC.table_per.thresh <- c()
  
  parents_list_per.thresh_name_per.thresh <- c()
  
  pdf("DAG_plots_per_thresholds.pdf")
  
  
  # ------------- 
  for (i in 1: length(threshold)) {
    # for (i in 1: length(threshold)) {
    
    temp_white_thresh_no_cycle <- as.data.frame(temp_list_merge[[i]], row.names = NULL)
    # temp_white_thresh_no_cycle <- as.data.frame(temp_list_merge[[i+1]], row.names = NULL)
    
    
    # browser()
    #--------------------------------------------
    set.seed(2023)
    
    try({
      
    if ((!is.null(temp_white_thresh_no_cycle) && nrow(temp_white_thresh_no_cycle) > 0) &&
        (!is.null(Black_List) && nrow(Black_List) > 0)) {
      arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl,
                             algorithm.args = list(whitelist = temp_white_thresh_no_cycle, blacklist = Black_List))
    } else if ((!is.null(temp_white_thresh_no_cycle) && nrow(temp_white_thresh_no_cycle) > 0)) {
      arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl,
                             algorithm.args = list(whitelist = temp_white_thresh_no_cycle))
    } else if ((!is.null(Black_List) && nrow(Black_List) > 0)) {
      arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl,
                             algorithm.args = list(blacklist = Black_List))
    } else {
      arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl,
                             algorithm.args = list())
    }
    }, silent = FALSE)   
    # -------------------------------------------
    
    suppressWarnings( 
      ave.BRCA <- averaged.network(arstr)
    )
    # -------------- 
    arcs_before_remove_undirected <- arcs(ave.BRCA)
    arcs(ave.BRCA) <- directed.arcs(ave.BRCA)
    # -------------- 
    arcs <- arcs(ave.BRCA)
    
    source("calculate_cor_sign.R")
    CorSign <- calculate_cor_sign(arcs, corrcoef)
    
    HLarcs <- arcs[CorSign == "-",]
    
    BRCA_str = arc.strength(ave.BRCA, data = discretized_data)
    
    plot_temp <- strength.plot(ave.BRCA, BRCA_str, shape = "ellipse", highlight = list(arcs = HLarcs))
    title(main = paste("Temp DAG# ", threshold[i], sep = ""))
    print(paste("Temp DAG# ", threshold[i], sep = ""))
    print(plot_temp)  
    
    # -------------- 
    plot.new()
    grid.text(paste("Threshold: ", threshold[i]), x = 0.05, y = 0.95, just = "left")
    grid.text("Averaged Network (ave.BRCA):", x = 0.05, y = 0.90, just = "left")
    grid.text(capture.output(print(ave.BRCA)), x = 0.05, y = seq(0.90, 0.05, by = -0.05), just = "left", gp = gpar(fontsize = 8))
    
    grid.newpage()
    grid.text("arcs_before_remove_undirected:", x = 0.05, y = 0.95, just = "left")
    grid.text(capture.output(print(arcs_before_remove_undirected)), x = 0.05, y = seq(0.90, 0.05, by = -0.05), just = "left", gp = gpar(fontsize = 8))
    
    grid.newpage()
    grid.text("Arcs:", x = 0.05, y = 0.95, just = "left")
    grid.text(capture.output(print(arcs)), x = 0.05, y = seq(0.90, 0.05, by = -0.05), just = "left", gp = gpar(fontsize = 8))
    
    grid.newpage()
    grid.text("BRCA_str:", x = 0.05, y = 0.95, just = "left")
    grid.text(capture.output(print(BRCA_str)), x = 0.05, y = seq(0.90, 0.05, by = -0.05), just = "left", gp = gpar(fontsize = 8))
    
    
    # -------------- number of arcs in DAG for each thershold
    
    num_arcs_DAG_per.thresh_name <- c(num_arcs_DAG_per.thresh_name , paste("thresh_# ", threshold[i], sep = ""))
    
    
    # --------------
    # browser()
    # --------------
    
    row_name <- paste("thresh_# ", threshold[i], sep = "")
    # num_arcs.thresh <- length(directed.arcs(ave.BRCA))
    num_arcs.thresh <- nrow(directed.arcs(ave.BRCA))
    
    cat("------------------------------", "\n")
    print("num_arcs.thresh:")
    cat("------------------------------", "\n")
    print(num_arcs.thresh)
    
    
    num_arcs.DAG_per.thresh_colmn <- data.frame(row_name = row_name, num_arcs.thresh = num_arcs.thresh)
    # num_arcs.DAG_per.thresh_colmn <- data.frame(row_name = row_name, num_arcs.thresh = num_arcs.thresh)
    
    
    cat("------------------------------", "\n")
    print("num_arcs.DAG_per.thresh_colmn:")
    cat("------------------------------", "\n")
    print(num_arcs.DAG_per.thresh_colmn)
    
    # rownames(num_arcs.DAG_per.thresh_colmn) <- row_name
    
    num_arcs_DAG_per.thresh <- rbind(num_arcs_DAG_per.thresh, num_arcs.DAG_per.thresh_colmn)
    
    
    cat("------------------------------", "\n")
    print("num_arcs_DAG_per.thresh:")
    cat("------------------------------", "\n")
    print(num_arcs_DAG_per.thresh)
    
    fitted_network <- bn.fit(ave.BRCA, data = discretized_data)
    # fitted_network <- bn.fit(ave.BRCA, data = data)
    
    
    nodes = nodes(fitted_network)
    parents_list_per.thresh_thresh  <- list()
    
    for (node in nodes) {
      # browser()
      parents_list_per.thresh_thresh[[node]] = parents(fitted_network, node)
    }
    
    parents_list_per.thresh_name_per.thresh <- c(parents_list_per.thresh_name_per.thresh , paste("parent_list_thresh_# ", threshold[i], sep = ""))
    parents_list_per.thresh[[i]] <- parents_list_per.thresh_thresh
    
    Network_residual <- as.data.frame(residuals(fitted_network))
    # Network_residual <- select_if(Network_residual, is.numeric)
    
    non_numeric_cols <- !sapply(Network_residual, is.numeric)
    if (any(non_numeric_cols)) {
      Network_residual[, non_numeric_cols] <- lapply(Network_residual[, non_numeric_cols], as.numeric)
    }
    
    Loss_per.thresh <- colSums(abs(Network_residual)/nrow(discretized_data))
    
    npar <- sapply(nodes(ave.BRCA), function(x) length(parents(fitted_network,x)))
    
    
    cat("------------------------------", "\n")
    print("Loss_per.thresh:")
    cat("------------------------------", "\n")
    print(Loss_per.thresh)
    
    
    cat("------------------------------", "\n")
    print("npar:")
    cat("------------------------------", "\n")
    print(npar)
    
    # browser()
    
    # ------------- 
    BIC_per.thresh <- (Loss_per.thresh)*num_sample + (npar)*log10(num_sample)
    # -------------
    # BIC_per.thresh <- sum(Loss_per.thresh)*num_sample + sum(npar)* log(num_sample)
    # -------------
    
    BIC.table_per.thresh <- cbind(BIC.table_per.thresh, BIC_per.thresh)
    # -------------
    BIC.threshold.colname_i <- paste0("Threshold (", threshold[i], ")", sep = "")
    BIC.threshold.colname <- c(BIC.threshold.colname, BIC.threshold.colname_i)
    # -------------------
    # npar_name <- paste("npar_thresh_# ", threshold[i], sep = "")
    npar_name <- paste("Threshold (", threshold[i], ")", sep = "")
    
    # ------------- 
    npar_df <- data.frame(node = names(npar), npar = npar)  
    rownames(npar_df) <- NULL   
    colnames(npar_df)[2] <- npar_name   
    npar_list[[i]] <- npar_df  
    
    # ------------- 
    # npar_list[[i]] <- data.frame(node = names(npar), npar = npar)
    # rownames(npar_list[[i]]) <- NULL
    # colnames(npar_list[[i]])[2] <- npar_name
    # -------------
    
    #L1_name <- paste("L1_thresh_#", threshold[i], sep = "")
    L1_name <- paste("Threshold (", threshold[i], ")", sep = "")
    
    # ------------- 
    L1_df <- data.frame(node = names(Loss_per.thresh), L1 = Loss_per.thresh)  
    rownames(L1_df) <- NULL   # Set row names to NULL
    colnames(L1_df)[2] <- L1_name   # Set column name
    L1_list[[i]] <- L1_df  # Assign to list
    
    # ------------- 
    # L1_list[[i]] <- data.frame(node = names(Loss_per.thresh), L1 = Loss_per.thresh)
    # rownames(L1_list[[i]]) <- NULL
    # colnames(L1_list[[i]])[2] <- L1_name
    # -------------
  }
  
  dev.off()
  
  # ------------- 
  # Change the name of the last column in npar_list and L1_list to "All"
  # Change the name of the last column in npar_list and L1_list to "All"
  if (length(npar_list) > 0) {
    colnames(npar_list[[length(npar_list)]])[2] <- "All"
  }
  
  if (length(L1_list) > 0) {
    colnames(L1_list[[length(L1_list)]])[2] <- "All"
  }
  
  # names(npar_list)[length(npar_list)] <- "All"
  # names(L1_list)[length(L1_list)] <- "All"
  # -------------
  
  # browser()
  
  npar_per.thresh_table <- Reduce(function(x, y) merge(x, y, by = "node", all = TRUE), npar_list)
  Loss_per.thresh_table <- Reduce(function(x, y) merge(x, y, by = "node", all = TRUE), L1_list)
  
  
  
  # browser()
  # ------------- 
  
  ng <- empty.graph(nodes = (as.character(colnames(discretized_data))))
  fnBN <- bn.fit(ng, data = discretized_data)
  # fnBN <- bn.fit(ng, data = data)
  
  residnBN <- as.data.frame(residuals(fnBN))
  
  L1_nBN <- colSums(abs(residnBN)/nrow(discretized_data))
  # temp_L1_nBN <- L1_nBN
  # names(L1_nBN)
  L1_nBN <- data.frame(node = names(L1_nBN), L1_empty = L1_nBN)
  rownames(L1_nBN) <- NULL
  
  # -------------
  # Loss_table <- Loss_per.thresh_table
  Loss_table <- merge(L1_nBN, Loss_per.thresh_table, by = "node", all = TRUE)
  Loss_table[is.na(Loss_table)] <- 0
  # --------------
  
  # Loss_table <- Loss_per.thresh_table
  
  
  
  rownames(Loss_table) <- Loss_table$node  
  Loss_table <- Loss_table[, -1]  
  
  # ------------- 
  npar_nBN <- data.frame(node = npar_per.thresh_table$node, npar_empty_graph = rep(0, length(npar_per.thresh_table$node)))
  # npar_table <- npar_per.thresh_table
  npar_table <- merge(npar_nBN, npar_per.thresh_table, by = "node", all = TRUE)
  # -------------
  # npar_table <- npar_per.thresh_table
  
  # --------------
  rownames(npar_table) <- npar_table$node  
  npar_table <- npar_table[, -1] 
  # --------------
  
  # browser()
  # --------------
  
  colnames(num_arcs_DAG_per.thresh)[1] <- ""
  num_arcs_DAG_per.thresh <- as.data.frame(num_arcs_DAG_per.thresh)
  
  # ------------- 
  # L1_nBN$BIC_per.thresh_nBN <- L1_nBN$L1_empty * num_sample
  # BIC_per.thresh_nBN <- data.frame(node = L1_nBN$node, BIC_empty = L1_nBN$BIC_per.thresh_nBN)
  # 
  # BIC.table_per.thresh <- as.data.frame(BIC.table_per.thresh)
  # BIC.table_per.thresh$node <- rownames(BIC.table_per.thresh)
  # rownames(BIC.table_per.thresh) <- NULL
  # BIC.table <- merge(BIC_per.thresh_nBN, BIC.table_per.thresh, by = "node", all = TRUE)
  
  # # ------------- 
  # BIC_per.thresh_nBN <-  data.frame(node = names(L1_nBN), BIC_empty = (L1_nBN$L1_empty)*num_sample)
  # BIC_per.thresh_nBN <- (temp_L1_nBN)*num_sample   # this term is zero because empty graph has zero parents: + sum(npar_nBN)* log(num_sample)
  # BIC.table <- merge(BIC_per.thresh_nBN, BIC.table_per.thresh, by = "node", all = TRUE)
  
  #------------------------- 
  L1_nBN$BIC_per.thresh_nBN_2 <- L1_nBN$L1_empty * num_sample
  BIC_per.thresh_nBN_2 <- data.frame(node = L1_nBN$node, BIC_empty = L1_nBN$BIC_per.thresh_nBN_2)
  
  BIC.table_per.thresh_2 <- as.data.frame(BIC.table_per.thresh)
  BIC.table_per.thresh_2$node <- rownames(BIC.table_per.thresh)
  rownames(BIC.table_per.thresh_2) <- NULL
  BIC.table_per.thresh_2 <- BIC.table_per.thresh_2[, c(ncol(BIC.table_per.thresh_2), 1:(ncol(BIC.table_per.thresh_2) - 1))]
  # BIC.table_per.thresh_2 <- BIC.table_per.thresh_2[, c("node", setdiff(names(BIC.table_per.thresh_2), "node"))]
  
  BIC.table_2 <- merge(BIC_per.thresh_nBN_2, BIC.table_per.thresh_2, by = "node", all = TRUE)
  #------------------------- 
  
  BIC_per.thresh_nBN <-  L1_nBN$L1_empty*num_sample
  BIC.table <- cbind(BIC_per.thresh_nBN, BIC.table_per.thresh)
  # ------------- 
  BIC.threshold.colname[length(BIC.threshold.colname)] <- "All" # "all.arcs"
  
  BIC.threshold.colname <- c("empty_graph", BIC.threshold.colname)
  colnames(BIC.table)<- BIC.threshold.colname
  # -------------
  # names(BIC.table) <- num_arcs_DAG_per.thresh_name
  

  parents_list_per.thresh_name_per.thresh <- c("parent_list_empty_graph" , parents_list_per.thresh_name_per.thresh)
  
  
  parents_list_empty_graph <- vector(mode = "list", length = nrow(npar_table))
  names(parents_list_empty_graph) <- names(parents_list_per.thresh[[2]])
  parents_list_empty_graph[] <- rep(list(NULL), length(parents_list_empty_graph))
  
  
  print("-----------------parents_list_empty_graph:----------------\n")
  print((parents_list_empty_graph))
  print("-----------------End--parents_list_empty_graph:----------------\n")
  # -------------
  
  parents.list_all_threshold <- list()
  parents.list_all_threshold <- c(list(parents_list_empty_graph), parents_list_per.thresh)
  # ------------- 
  # parents.list_all_threshold <- parents_list_per.thresh
  
  # ------------- 
  parents_list_per.thresh_name_per.thresh[length(parents_list_per.thresh_name_per.thresh)] <- "All" # "all.arcs"
  
  names(parents.list_all_threshold) <- parents_list_per.thresh_name_per.thresh
  
  
  print("-----------------parents.list_all_threshold:----------------\n")
  print((parents.list_all_threshold))
  print("-----------------End--parents.list_all_threshold:----------------\n")
  
  # -------------------
  return(list(npar_table = npar_table, Loss_table = Loss_table,
              parents.list_all_threshold = parents.list_all_threshold,
              num_arcs_DAG_per.thresh = num_arcs_DAG_per.thresh,
              BIC.table = BIC.table))  
  
}  

# -----------------------------------------------------------------------------------------------------

