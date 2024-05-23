# function to create plot
createVariableDistributionPlot <- function(data) {
  # num_vars <- length(colnames(data))
  num_vars <- as.numeric(length(colnames(data)))
  if (is.na(num_vars)) {
    print("num_vars calculation failed")
    return()
  }
  
  num_cols <- if(num_vars > 20) 4 else 2
  num_rows <- ceiling(num_vars / num_cols)
  
  # plotting parameters
  par(mfrow = c(num_rows, num_cols), mar = c(3, 3, 2, 1))  # margins
  
  # Loop through each variable and create histograms with density lines
  for (var in colnames(data)) {
    x <- data[, var]
    if(!all(is.na(x))) {
      hist(x, prob = TRUE, xlab = "", ylab = "", main = var, col = "steelblue")
      lines(density(x, na.rm = TRUE), col = "darkred", lwd = 2)
    }
  }
}

