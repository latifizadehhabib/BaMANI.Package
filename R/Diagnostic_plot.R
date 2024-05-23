Diagnostic_plot <- function(num.white_thresh, num_arcs.All.thresh, Total.BIC.thresh, threshold){
  
  print("Starting 'Diagnostic_plot' inputs:")
  
  # Extract minimal length across all data sets
  min_len <- min(length(num.white_thresh[, 1]), length(num_arcs.All.thresh[, 1]), length(Total.BIC.thresh[, 1]))
  
  # Extract data
  x1 <- seq_len(min_len)
  y1 <- num.white_thresh[1:min_len, 1]
  y2 <- num_arcs.All.thresh[1:min_len, 1]
  y3 <- Total.BIC.thresh[1:min_len, 1]
  
  # Combine data into data frame
  Combine <- data.frame(x = as.numeric(x1),
                        num.white = as.numeric(y1),
                        num.arcs = as.numeric(y2),
                        Tot.BIC = as.numeric(y3))
  
  threshold.temp <- signif(threshold, digits = 1)
  
  # color palette and custom theme
  palette <- c("num.white" = "#0072B2", "num.arcs" = "#D55E00", "Tot.BIC" = "#009966")
  
  custom_theme <- theme_bw() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # First plot: Final DAG vs. WhiteList
  p1 <- ggplot(Combine, aes(x = x)) +
    geom_point(aes(y = num.white, fill = "num.white"), size = 2.5, color = palette["num.white"], shape = 21) +
    geom_line(aes(y = num.white), color = palette["num.white"], size = 1.2) +
    geom_point(aes(y = num.arcs, fill = "num.arcs"), size = 2.5, color = palette["num.arcs"], shape = 21) +
    geom_line(aes(y = num.arcs), color = palette["num.arcs"], size = 1.2) +
    labs(title = "Final DAG vs. WhiteList", y = "Number of Arcs") +
    scale_x_continuous(breaks = seq(min(Combine$x), max(Combine$x), by = 1), labels = c("empty.Graph", 0, threshold.temp, "all.arcs")) +
    xlab(NULL) +
    scale_fill_manual(values = palette) +
    custom_theme +
    guides(fill = guide_legend(title = NULL))
  
  # Second plot: Total BIC
  p2 <- ggplot(Combine, aes(x = x)) +
    geom_point(aes(y = Tot.BIC), size = 3, color = palette["Tot.BIC"], shape = 16) +
    geom_line(aes(y = Tot.BIC), color = palette["Tot.BIC"], size = 1.2) +
    labs(title = "Total BIC", y = "Bayesian Information Criteria (Total)") +
    scale_x_continuous(breaks = seq(min(Combine$x), max(Combine$x), by = 1), labels = c("empty.Graph", 0, threshold.temp, "all.arcs")) +
    scale_color_manual(values = palette["Tot.BIC"]) +
    xlab("Threshold Level") +
    custom_theme
  
  # Arrange and display plots
  gridExtra::grid.arrange(p1, p2, ncol = 1)
}
