
plot_graph_with_and_without_cycles <- function(possible.white.list) {
  
  print("Starting 'plot_graph_with_and_without_cycles' inputs:")
  
  # library(igraph)
  g.initial <- graph_from_data_frame(d = possible.white.list, directed = TRUE)
  shared_layout <- layout_nicely(g.initial)
  
  plot_graph_from_table <- function(g, main="Graph", color="black", layout) {
    plot(g, layout=layout, main=main, vertex.size=30, vertex.label.cex=1.5, edge.arrow.size=0.71, vertex.color=color)
  }
  
  # capture first plot
  plot_graph_from_table(g.initial, "White list before Cycle check", "lightblue", shared_layout)
  plot_before <- recordPlot()
  
  remove_cycles <- function(g) {
    while(girth(g)$girth > 0) { # While graph has cycles
      scc <- clusters(g, mode = "strong")
      scc_ids <- which(scc$csize > 1) # Find strongly connected components
      for(scc_id in scc_ids) {
        vertices_in_scc <- V(g)[scc$membership == scc_id]
        edges_in_scc <- E(g)[inc(vertices_in_scc)]
        if(length(edges_in_scc) > 0) {
          g <- delete_edges(g, edges_in_scc[1]) # Remove one edge from each strongly connected component to break cycle
        }
      }
    }
    return(g)
  }
  
  # Remove cycles
  g.final <- remove_cycles(g.initial)
  
  # Plot &  capture second plot
  plot_graph_from_table(g.final, "Final White list after Cycle check", "lightcoral", shared_layout)
  plot_after <- recordPlot()
  
  final_white_list <- as.data.frame(g.final, what='edges')
  final_white_list <- as.data.frame(final_white_list)
  row.names(final_white_list) <- NULL
  
  
  return(list(final_white_list = final_white_list, plot_before = plot_before, plot_after = plot_after))
}
