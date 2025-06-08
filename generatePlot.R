generatePlot <- function(status, key_feature, cellType, fitted_network, data) {
  
  set.seed(2023) 
  cat("generatePlot IN", cellType)
  if(!is.character(cellType) || length(cellType) != 1)
    stop("cellType should be a single character value.")
  
  if(!inherits(fitted_network, "bn.fit"))
    stop("fitted_network should be a bn.fit object.")
  
  if(!is.data.frame(data) || !all(c(key_feature, cellType, status) %in% names(data))){
    stop("Invalid data frame.")
  }
  
  prepareSimData <- function(sim, node) data.frame(x = sim[[key_feature]], y = sim[[node]])
  
  # Perform linear regression & normalize data
  normalize <- function(var, condition) {
    (var[condition] - min(var)) / (max(var) - min(var))
  }
  # --------------------------------------------------------
  # status` is user-selected categorical column with multiple categories
  unique_categories <- sort(unique(data[[status]]))  # Get unique categories from selected column

  
  # Initialize lists to store simulation & regression results for each category
  simData <- list()  
  simData.list <- list()  
  Data.category.list <- list()
  regressionResults.list <- list()
  
  # generalize cpdist execution for all categories
  for(j in seq_along(unique_categories)) {
    set.seed(2023) 
    category <- unique_categories[j]
    # simData[[j]] <- tryCatch(cpdist(fitted_network, nodes = c(key_feature, cellType), n = 10^6,
    simData[[j]] <- tryCatch(cpdist(fitted_network, nodes = c(key_feature, cellType), n = 200,
                                                                    
                                    evidence = setNames(list(category), status), method="lw"), 
                             error = function(e) {message(e$message); NULL})
    # prepare simulation data for current category
    if(!is.null(simData[[j]])) {
      simData.list[[j]] <- prepareSimData(simData[[j]], cellType)
      
      # normalize & prepare data for regression
      xa <- normalize(data[[key_feature]], data[[status]] == category) 
      ya <- normalize(data[[cellType]], data[[status]] == category)   
      Data.category.list[[j]] <- data.frame(x = xa, y = ya)  
      
      # linear regression for current category & find  slopes & intercepts for lines
      lmResults <- lm(y ~ x, data = simData.list[[j]])
      regressionResults.list[[j]] <- list(slope = coef(lmResults)[2], intercept = coef(lmResults)[1])

      regressionResults.list[[j]]$adj_r_squared <- summary(lmResults)$adj.r.squared
    }
  }
  # --------------------------------------------------------
  Data.combined <- data.frame(x = numeric(), y = numeric(), type = character())
  
  simData.combined <- data.frame(x = numeric(), y = numeric(), type = character())
  
  for(j in seq_along(unique_categories)) {
    category <- unique_categories[j]
    category_label <- paste("Category", category) 
    
    # Assuming simData.list[[j]]$data contains 'x' & 'y' columns
    if (!is.null(simData.list[[j]])) {
      # Data.category <- simData.list[[j]]$data
      Data.category.list[[j]]$type <- category_label
      simData.list[[j]]$type <- category_label
      
      Data.combined <- rbind(Data.combined, Data.category.list[[j]])
      simData.combined <- rbind(simData.combined, simData.list[[j]])
    }
  }
  
  number_of_categories <- length(unique_categories)
  palette <- brewer.pal(min(number_of_categories, 9), "Set1")  # Using 'Set1', adjust if more than 9 categories
  
  color_values <- setNames(palette, paste("Category", unique_categories))
  
  abline_layers <- lapply(seq_along(regressionResults.list), function(j) {
    geom_abline(
      aes(slope = regressionResults.list[[j]]$slope, intercept = regressionResults.list[[j]]$intercept),
      color = palette[j], lty = "solid", lwd = 1.05
    )
  })
  
  # Create labels dynamically based on regression results & actual category names
  labels <- sapply(seq_along(regressionResults.list), function(j) {
    sprintf("Type '%s':  %1.3f", unique_categories[j], regressionResults.list[[j]]$slope)
    # sprintf("Line slope in Type '%s':  %1.3f", unique_categories[j], regressionResults.list[[j]]$slope)
    
    #sprintf("Cat [%s] :  %1.3f", unique_categories[j], regressionResults.list[[j]]$slope)

  })
  names(labels) <- paste("Category", unique_categories)
  
  # ---------------------------------  
  line_labels <- sapply(seq_along(regressionResults.list), function(j) {
    sprintf("Type '%s': %1.3f", unique_categories[j], regressionResults.list[[j]]$slope)
    # sprintf("Type '%s' line slope: %1.3f", unique_categories[j], regressionResults.list[[j]]$slope)
    
  })
  
  point_labels <- sapply(unique_categories, function(category) {
    sprintf("Data points for Category %s", category)
  })
  
  combined_labels <- c(line_labels, point_labels)
  
  p <- ggplot() +
    geom_density_2d(data = simData.combined, aes(x = x, y = y, color = type), size = 0.7) +
    geom_point(data = Data.combined, aes(x = x, y = y, color = type), alpha = 0.5, shape = 19, size = 2) +
    scale_color_manual(values = rep(color_values, 2), labels = combined_labels) +
    #labs(color = "Data Representation per Category") +    
    labs(color = paste("Regression Line Trend per Category in '",status,"'")) +
    # labs(color = paste('Data Representation per Category in"',status,'"')) +
    xlab(key_feature) + ylab(cellType) +
    theme_minimal() +
    theme(legend.position = "top", 
          plot.title = element_text(size = 15), 
          axis.text = element_text(size = 14), 
          axis.title = element_text(size = 15),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          plot.margin = margin(10, 10, 10, 10),  
          legend.box.margin = margin(10, 10, 10, 10),  
    )
  
  # --------------------------------- 
  for (layer in abline_layers) {
    p <- p + layer
  }
  # ------------
  return(p)
}
