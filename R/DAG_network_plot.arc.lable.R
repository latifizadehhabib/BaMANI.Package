DAG_network_plot.arc.lable <- function (                                                        
                                                          augmented_edge_list, 
                                                          possible_seed_arcs_filter, 
                                                          data, 
                                                          discretized_data, 
                                                          possible.white.list, 
                                                          Black_List, 
                                                          nboot, cl, 
                                                          corrcoef) {
  
  
  cat("------------------------------", "\n")
  print("Starting 'DAG_network_plot.arc.lable' inputs:")
  cat("------------------------------", "\n")
  
    possible.white.list <- as.data.frame(possible.white.list)
    discretized_data <- as.data.frame(discretized_data)
    Black_List <- as.data.frame(Black_List)
    
    arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl, algorithm.args = list(whitelist = possible.white.list, blacklist = Black_List))
    ave.BRCA <- averaged.network(arstr)
    
    arcs(ave.BRCA) <- directed.arcs(ave.BRCA) # ignore undirected arcs
    arcs.BRCA <- arcs(ave.BRCA)
    arcs.BRCA <- as.data.frame(arcs.BRCA)
    
    fBRCABN <- bn.fit(ave.BRCA, data = discretized_data)
    
    BRCA_str = arc.strength(ave.BRCA, data = discretized_data)
    weight.strength <- BRCA_str$strength
    
    # ----------------------------------
    
    arc_slopes <- data.frame(from = character(), to = character(), slope = numeric())
    
    # iterate over all nodes in network
    for(node in nodes(fBRCABN)) {
      # get names of parent nodes
      parents <- parents(fBRCABN, node)
      
      # iterate over all parent nodes & get slope coefficients
      for(parent in parents) {
        # get slope coefficient
        slope <- coef(fBRCABN)[[node]][[parent]]
        
        # add arc & slope coefficient to data frame
        arc_slopes <- rbind(arc_slopes, data.frame(from = parent, to = node, slope = slope))
      }
    }
    
    arcs_strength <- data.frame(from = as.character(arcs.BRCA[, 1]), 
                                to = as.character(arcs.BRCA[, 2]), 
                                weight.strength = weight.strength)
    
    arc_slopes.strength <- merge(arcs_strength, arc_slopes, by = c("from", "to"), all = TRUE)
    
    
    # Create graph using renamed column names
    g <- graph_from_data_frame(d = arc_slopes.strength[, c("from", "to")], directed = TRUE)
    
    E(g)$weight.strength <- arc_slopes.strength$weight.strength
    E(g)$slope <- signif(arc_slopes.strength$slope, digits = 2)
    
    # Set layout of graph
    layout <- layout_with_kk(g)
    
    # colors for edges based on slope
    edge_colors <- ifelse(E(g)$slope < 0, "red", "black")
    
    vertex_shapes <- rep("circle", nrow(arc_slopes))
    
    E(g)$width <- signif(E(g)$weight.strength, digits = 2)
    
    # assign vertex shapes to graph object
    V(g)$shape <- vertex_shapes
    
    # change shape of "Cancer" vertex to "diamond"
    V(g)$shape[V(g)$name == "CCN4"] <- "square"
    V(g)$shape[V(g)$name == "Cancer"] <- "rectangle"
    
    # set arrowhead mode for edges based on slope
    # E(g)$arrow.mode <- ifelse(E(g)$slope >= 0, "triangle", "tooth")
    
    # Plot graph
    plot(g, layout = layout, edge.arrow.size = 0.6, vertex.color = "lightblue",
         # edge.width = E(g)$width, 
         edge.label = signif(E(g)$slope, digits = 2), 
         edge.label.dist = 2, edge.label.color = "black",
         edge.color = edge_colors,
         # vertex.shape = vertex_shapes,
         rescale = T, vertex.size = 15, vertex.label.cex = 0.95,
         width = 12, height = 14)
}