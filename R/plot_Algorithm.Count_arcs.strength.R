plot_Algorithm.Count_arcs.strength <- function(Alg.Count_arcs.strength.data) {
  
  print("Starting 'plot_Algorithm.Count_arcs.strength' inputs:")
  
  
  data <- Alg.Count_arcs.strength.data
  
  # categories based on condition
  data$category <- ifelse(data$Min.BIC_clear.direction == 1, "Min.BIC_clear.direction",
                        ifelse(data$Min.BIC.unclear.direction == 1, "Min.BIC.unclear.direction",
                               ifelse(data$Unclear.direction == 1, "Unclear.direction",
                                      ifelse(data$Excluded == 1, "Excluded", "Min.BIC.unclear.direction"))))
  
   arc_name <- as.numeric(data$Edge_No)
  algorithm_count <- as.numeric(data$Hit.Count)
  # ensure that the logarithmic transformation is applied only to positive values by adding small positive value
  min_strength <- as.numeric(data$Min_strength)#+0.00000001
  max_strength <- as.numeric(data$Max_strength)#+0.00000001
  
  
  # plot with algorithm count as bars
  p1 <- ggplot(data, aes(x = arc_name, y = algorithm_count, color = category)) +
    # geom_col(position = "stack") +
    geom_point(size = 5, position = position_dodge(width = 0.5)) +  # position_dodge to avoid overlap 
    geom_segment(aes(xend = arc_name, yend = 0), position = position_dodge(width = 0.5), linetype = "solid", size = 0.6) +  # Add segments from points to the x-axis
    labs(x = "Arcs", y = "Algorithm Count") +  # Combined both labels into one labs() function
    scale_color_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728")) +
    theme_bw() +
    theme(panel.background = element_blank()) +
    scale_x_continuous(breaks = seq(min(arc_name), max(arc_name), 1)) + 
    scale_y_continuous(breaks = seq(floor(min(algorithm_count)), ceiling(max(algorithm_count)), 1))  # 
  
  # p1 <- ggplot(data, aes(x = arc_name, y = algorithm_count, fill = category)) +
  #   geom_col(position = "stack") +
  #   labs(x = "Arcs") +
  #   labs(y = "Algorithm Count") +
  #   scale_fill_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728")) +
  #   theme_bw()+
  #   theme(panel.background = element_blank())+
  #   scale_x_continuous(breaks = seq(min(arc_name), max(arc_name), 1))# Add sequence of integers to x-axis labels
   
  # ----------------------------
  # p2 <- ggplot(data, aes(x = arc_name, y = min_strength, group = 1)) +
  #   geom_point(aes(color = "Min strength"), size = 2) +
  #   geom_point(aes(y = max_strength, color = "Max strength"), size = 2) +
  #   geom_line(aes(y = min_strength, group = 1), color = "red") +
  #   geom_line(aes(y = max_strength, group = 1), color = "blue") +
  #   labs(x = NULL, y = "Strength", color = NULL) +
  #   # scale_y_continuous(name = "Min / Max Strength")+
  #   scale_y_log10(name = "Min / Max Strength") +
  #   scale_color_manual(values = c("blue", "red")) +
  #   theme_bw()+
  #   theme(panel.background = element_blank())+
  #   scale_x_continuous(breaks = seq(min(arc_name), max(arc_name), 1))# Add sequence of integers to x-axis labels
  # 
  # # Combine plots using cowplot
  # plot_grid(p1, p2, nrow = 2, align = "v", axis = "tb", rel_widths = c(1, 0.7))
  print(p1)
}

