plot_Algorithm.Count_arcs.strength <- function(Alg.Count_arcs.strength.data) {
  
  set.seed(2023) 
  
  print("Starting 'plot_Algorithm.Count_arcs.strength' inputs:")
  
  
  data <- Alg.Count_arcs.strength.data
  
  # categories based on condition
  data$category <- ifelse(data$Min.BIC_clear.direction == 1, "Min.BIC_clear.direction",
                        ifelse(data$Min.BIC.unclear.direction == 1, "Min.BIC.unclear.direction",
                               ifelse(data$Unclear.direction == 1, "Unclear.direction",
                                      ifelse(data$Excluded == 1, "Excluded", "Min.BIC.unclear.direction"))))
  
   arc_name <- as.numeric(data$Edge_No)
  algorithm_count <- as.numeric(data$Hit.Count)
  min_strength <- as.numeric(data$Min_strength)
  max_strength <- as.numeric(data$Max_strength)
  
  
  # plot with algorithm count as bars
  p1 <- ggplot(data, aes(x = arc_name, y = algorithm_count, color = category)) +
    # geom_col(position = "stack") +
    geom_point(size = 5, position = position_dodge(width = 0.5)) +  
    geom_segment(aes(xend = arc_name, yend = 0), position = position_dodge(width = 0.5), linetype = "solid", size = 0.6) +  
    labs(x = "Arcs", y = "Algorithm Count") +  
    scale_color_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728")) +
    theme_bw() +
    theme(panel.background = element_blank()) +
    scale_x_continuous(breaks = seq(min(arc_name), max(arc_name), 1)) + 
    scale_y_continuous(breaks = seq(floor(min(algorithm_count)), ceiling(max(algorithm_count)), 1))  # 
  
  print(p1)
}

