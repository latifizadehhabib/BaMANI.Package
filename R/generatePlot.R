generatePlot <- function(status, key_feature, cellType, fBRCABN, data) {
  
  
  cat("------------------------------", "\n")
  print("Starting 'generatePlot' inputs:")
  cat("------------------------------", "\n")
  
  print(list(
    status = status,
    userSelected_key_feature = key_feature
  ))
  cat("------------------------------", "\n")
  
  
  # Validate inputs
  if(!is.character(cellType) || length(cellType) != 1)
    stop("cellType should be a single character value.")
  
  if(!inherits(fBRCABN, "bn.fit"))
    stop("fBRCABN should be a bn.fit object.")
  
  if(!is.data.frame(data) || !all(c(key_feature, cellType, status) %in% names(data))){
    stop("Invalid data frame.")
  }
  
  cat("------------------------------", "\n")
  print("Defining evidence_high & evidence_low")
  cat("------------------------------", "\n")
  
  #print(bnlearn::nodes(fBRCABN))
  #print(bnlearn::nodeStates(fBRCABN))
  cat("------------------------------", "\n")
  print("Current  status")
  cat("------------------------------", "\n")
  print(status)
  cat("------------------------------", "\n")
  
  # Prepare data for plotting
  prepareSimData <- function(sim, node) data.frame(x = sim[[key_feature]], y = sim[[node]])
  
  # Perform linear regression & normalize data
  normalize <- function(var, condition) {
    (var[condition] - min(var)) / (max(var) - min(var))
  }
  # --------------------------------------------------------
  # status` is user-selected categorical column with multiple categories
  unique_categories <- sort(unique(data[[status]]))  # Get unique categories from selected column
  cat("------------------------------", "\n")
  print("unique categories: ")
  cat("------------------------------", "\n")
  print(unique_categories)
  cat("------------------------------", "\n")
  
  # Initialize lists to store simulation & regression results for each category
  simData <- list()  #simData.tumor <- prepareSimData(sim1, cellType)      #simData.normal <- prepareSimData(sim2, cellType)
  simData.list <- list()  #simData.tumor <- prepareSimData(sim1, cellType)      #simData.normal <- prepareSimData(sim2, cellType)
  Data.category.list <- list()
  regressionResults.list <- list()
  
  # Generalize cpdist execution for all categories
  for(j in seq_along(unique_categories)) {
    category <- unique_categories[j]
    simData[[j]] <- tryCatch(cpdist(fBRCABN, nodes = c(key_feature, cellType), n = 10^5, 
                                    evidence = setNames(list(category), status), method="lw"), 
                             error = function(e) {message(e$message); NULL})
    # Prepare simulation data for current category
    if(!is.null(simData[[j]])) {
      simData.list[[j]] <- prepareSimData(simData[[j]], cellType)
      
      # Normalize & prepare data for regression
      xa <- normalize(data[[key_feature]], data[[status]] == category) # xa <- normalize(data[[key_feature]], data[[status]] == 1)   # xb <- normalize(data[[key_feature]], data[[status]] == 0)  
      ya <- normalize(data[[cellType]], data[[status]] == category)   # ya <- normalize(data[[cellType]], data[[status]] == 1)  # yb <- normalize(data[[cellType]], data[[status]] == 0)
      Data.category.list[[j]] <- data.frame(x = xa, y = ya)  # Data.tumor <- data.frame(x = xa, y = ya)      # Data.normal <- data.frame(x = xb, y = yb)
      
      # Perform linear regression for current category & find  slopes & intercepts for  lines
      lmResults <- lm(y ~ x, data = simData.list[[j]])
      regressionResults.list[[j]] <- list(slope = coef(lmResults)[2], intercept = coef(lmResults)[1])# cancer_slope <- coef(lm(y ~ x, data = simData.tumor))[2]     # cancer_intercept <- coef(lm(y ~ x, data = simData.tumor))[1]
      # normal_slope <- coef(lm(y ~ x, data = simData.normal))[2]     # normal_intercept <- coef(lm(y ~ x, data = simData.normal))[1]
      
      # Optionally, store additional results like adjusted R-squared
      regressionResults.list[[j]]$adj_r_squared <- summary(lmResults)$adj.r.squared
    }
  }
  # --------------------------------------------------------
  # Initialize an empty data frame for combined point data
  Data.combined <- data.frame(x = numeric(), y = numeric(), type = character())
  
  # Initialize an empty data frame for combined density data
  simData.combined <- data.frame(x = numeric(), y = numeric(), type = character())
  
  # Loop over each category to append to combined data frames
  for(j in seq_along(unique_categories)) {
    category <- unique_categories[j]
    category_label <- paste("Category", category)  # Custom label for each category
    
    # Assuming simData.list[[j]]$data contains 'x' & 'y' columns
    if (!is.null(simData.list[[j]])) {
      # Data.category <- simData.list[[j]]$data
      Data.category.list[[j]]$type <- category_label
      simData.list[[j]]$type <- category_label
      
      # Append to combined data frames
      Data.combined <- rbind(Data.combined, Data.category.list[[j]])
      simData.combined <- rbind(simData.combined, simData.list[[j]])
    }
  }
  
  # Define number of categories & palette first
  number_of_categories <- length(unique_categories)
  palette <- brewer.pal(min(number_of_categories, 9), "Set1")  # Using 'Set1', adjust if more than 9 categories
  
  # Create a named vector for color values mapping
  color_values <- setNames(palette, paste("Category", unique_categories))
  
  # Then, precompute geom_abline layers
  abline_layers <- lapply(seq_along(regressionResults.list), function(j) {
    geom_abline(
      aes(slope = regressionResults.list[[j]]$slope, intercept = regressionResults.list[[j]]$intercept),
      color = palette[j], lty = "solid", lwd = 1.05
    )
  })
  
  # Create labels dynamically based on regression results & actual category names
  labels <- sapply(seq_along(regressionResults.list), function(j) {
    sprintf("Cat [%s] :  %1.3f", unique_categories[j], regressionResults.list[[j]]$slope)
  })
  names(labels) <- paste("Category", unique_categories)
  
  # Build plot
  p <- ggplot() +
    geom_density_2d(data = simData.combined, aes(x = x, y = y, color = type)) +
    geom_point(data = Data.combined, aes(x = x, y = y, color = type), alpha = 0.5, shape = 19, size = 2) +
    scale_color_manual(values = color_values, labels = labels) +
    labs(color = paste('Slope of RL in simulated data per Categories in"',status,'"')) +
    xlab(key_feature) + ylab(cellType) +
    #xlab(paste("Normalized", key_feature)) + ylab(cellType) +
    
    theme_minimal() +
    theme(legend.position = "top", plot.title = element_text(size = 15), axis.text = element_text(size = 14), axis.title = element_text(size = 15))
  
  # Add abline layers to plot
  for (layer in abline_layers) {
    p <- p + layer
  }
  # Explicitly print plot
  print(p)
  
  # ------------
  return(p)
}