Diagnostic_plot <- function(num.white_thresh, num_arcs_DAG_per.thresh, threshold){
  
  set.seed(2023) 
  
  print("Starting 'Diagnostic_plot' inputs:")
  
  new_row <- data.frame(num.white_thresh = 0, row.names = "empty.Graph")
  num.white_thresh <- rbind(new_row, num.white_thresh)
  # ------------- 
  num_arcs_DAG_per.thresh <- num_arcs_DAG_per.thresh[, -1, drop = FALSE]
  
  # ------------- 
  num_arcs_DAG_per.thresh_new_row <- data.frame(num_arcs.thresh = 0, row.names = "empty.Graph")
  num_arcs_DAG_per.thresh <- rbind(num_arcs_DAG_per.thresh_new_row, num_arcs_DAG_per.thresh)

  # min_len <- min(length(num.white_thresh[, 1]), length(num_arcs_DAG_per.thresh[, 1]))
  # ------------- 
  min_len <- min(nrow(num.white_thresh), nrow(num_arcs_DAG_per.thresh))
  # ------------- 
  
  x1 <- seq_len(min_len)
  y1 <- num.white_thresh[1:min_len, 1]
  y2 <- num_arcs_DAG_per.thresh[1:min_len, 1]
  
  Combine <- data.frame(x = as.numeric(x1),
                        Whitelist = as.numeric(y1),
                        DAG = as.numeric(y2))
  
  # threshold.temp <- signif(threshold, digits = 1)
  
  palette <- c("Whitelist" = "#0072B2", "DAG" = "#D55E00")
  
  custom_theme <- theme_bw() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  breaks_seq <- seq(min(Combine$x), max(Combine$x), by = 1)
  # labels_seq <- c("empty.Graph", threshold.temp, rep("all.arcs", length(breaks_seq) - 2))
  
  threshold.temp <- signif(threshold, digits = 1)
  # ------------- 
  threshold.temp[length(threshold.temp)] <- "All" # "all.arcs"
  # ------------- 
  print(threshold.temp)
  
  labels_seq <- c("empty.Graph", threshold.temp)
  # ------------- 
  # labels_seq <- c("empty.Graph", threshold.temp, "all.arcs")
  # ------------- 
  
  # First plot: Final DAG vs. WhiteList
  p1 <- ggplot(Combine, aes(x = x)) +
    geom_point(aes(y = Whitelist, fill = "Whitelist"), size = 2.5, color = palette["Whitelist"], shape = 21) +
    geom_line(aes(y = Whitelist), color = palette["Whitelist"], size = 1.2) +
    geom_point(aes(y = DAG, fill = "DAG"), size = 2.5, color = palette["DAG"], shape = 21) +
    geom_line(aes(y = DAG), color = palette["DAG"], size = 1.2) +
    labs(title = "DAG vs. WhiteList", y = "Number of Arcs") +
    scale_x_continuous(breaks = breaks_seq, labels = labels_seq) +
    xlab("Threshold Level") +
    scale_fill_manual(values = palette) +
    custom_theme +
    guides(fill = guide_legend(title = NULL))
  
  print(p1)
}