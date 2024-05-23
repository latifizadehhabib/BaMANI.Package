diagnostic_plot_White_Final <- function(num.white_thresh, num_arcs.All.thresh, Total.BIC.thresh) {
  
  
  
  
  print("Starting 'diagnostic_plot_White_Final' inputs:")
  
  # unify name of both data frame to plot them
  num.white.thresh <- num.white_thresh
  num.arcs.All.thresh <- num_arcs.All.thresh
  Tot.BIC.thresh <- Total.BIC.thresh
  
  
  rownames(num.white.thresh) <- seq_len(nrow(num.white.thresh))
  rownames(num.arcs.All.thresh) <- seq_len(nrow(num.arcs.All.thresh))
  rownames(Tot.BIC.thresh) <- seq_len(nrow(Tot.BIC.thresh))
  
  # Debugging
  cat("num.white.thresh rows:", nrow(num.white.thresh), "\n")
  cat("num.arcs.All.thresh rows:", nrow(num.arcs.All.thresh), "\n")
  cat("Tot.BIC.thresh rows:", nrow(Tot.BIC.thresh), "\n")


  x1 <- seq_len(nrow(num.white.thresh))
  x2 <- seq_len(nrow(num.arcs.All.thresh))
  x3 <- seq_len(nrow(Tot.BIC.thresh))
  
  # function for min-max normalization to range of [0, 100],
  normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x))*1000)
    # return((x - min(x)) / (max(x) - min(x))*100)
  }
  
  y1 <- num.white.thresh$num.white_thresh
  y2 <- num.arcs.All.thresh$num_arcs.All.thresh
  y3 <- Tot.BIC.thresh$Total.BIC.thresh
  
  
  y1.normalize <- normalize(y1)
  y2.normalize <- normalize(y2)
  y3.normalize <- normalize(y3)
  
  
  num.white <- data.frame(x = x1, y = y1) # Attention 
  num.arcs <- data.frame(x = x2, y = y2)# Attention
  Tot.BIC <- data.frame(x = x3, y = y3)
  
  # png("Diagnostic_plot_bic normal_today.png", width = 1500, height = 1500, res = 300)
  
  # Combine data frames and add new column for group
  num.white$group <- "Number of WhiteList"
  num.arcs$group <- "Number of Arcs in Final DAG"
  Tot.BIC$group <- "Total BIC"
  
  combine <- rbind(num.white, num.arcs, Tot.BIC)
  
  # Plot data frames
  plot <- ggplot(combine, aes(x = x, y = y, color = group)) +
    geom_line()+
    
    # geom_point(size = 3, fill = "white") +
    # geom_line(color = "blue", linetype = 1, size = 1)+
    
    labs(title = "Diagnostic_plot")+
    # scale_x_continuous(breaks = seq(1, 8, by = 1), labels = seq(1, 8, by = 1))+
    xlab("Threshold Level") +
    ylab("Number of Arcs")
  # +
  #   scale_x_discrete(labels=c("Empty Graph", "0", "2e-148", "3e-43", "3e-23", 
  #                             "5e-16", "4e-12", "2e-08",
  
    # dev.off()
    # cat("----------------------------------------------------------------------------------'", "\n")
    # cat("Diagnostic_plot for 'Arcs inFinal DAG' & 'Number of WhiteList' for each threshold ploted", "\n")
    # cat("----------------------------------------------------------------------------------'", "\n")
    
  return(plot)
}
