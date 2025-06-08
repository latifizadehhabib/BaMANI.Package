Contour_plot_userSelected_feature <- function(
    data, discretized_data,
    fitted_network,
    Status,
    userSelected_key_feature,
    selectedCellType) {
  
  set.seed(2023) 
  
  cat("------------------------------", "\n")
  print("Starting 'Contour_plot_userSelected_feature' inputs:")
  cat("------------------------------", "\n")
  
  # ----------------------------------
  source("generatePlot.R")
  # ----------------------------------
  # Cantour Plot 
  
  # if (!is.null(fitted_network)) {
  
  # key_feature <- names(data)[2] # column of data that is selected as ""key_feature"" by user
  key_feature <- userSelected_key_feature
  
  all.features <- colnames(discretized_data)
  
  cellTypes <- all.features[!all.features %in% c(key_feature, Status)]
  # cellTypes <- all.features[!all.features %in% c("B", "A")]
  
  plot <- generatePlot(Status, key_feature, selectedCellType, fitted_network, data)
  return(plot)
  # }
}

