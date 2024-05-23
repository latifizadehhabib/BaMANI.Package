calculate_cor_sign <- function(ars, corrcoef) {
  
  
  
  cat("------------------------------", "\n")
  print("Starting 'calculate_cor_sign' inputs:")
  cat("------------------------------", "\n")
  cat(sprintf("class of ars:"), "\n")
  cat("------------------------------", "\n")
  print(class(ars))  # Should output 'data.frame'
  cat("------------------------------", "\n")
  cat(sprintf("class of corrcoef:"), "\n")
  cat("------------------------------", "\n")
  print(class(corrcoef))  # Should output 'matrix'
  cat("------------------------------", "\n")
  
  # Ensure ars is data frame with expected columns
  if (!("data.frame" %in% class(ars))) {
    ars <- as.data.frame(ars)
  }
  
  # Check if it contains expected columns
  if (!all(c("from", "to") %in% names(ars))) {
    stop("The 'ars' data frame must contain 'from' and 'to' columns.")
  }
  
  # Display first few rows to verify structure
  print(head(ars))
  
  
  
  # If either is not correct, may need to convert them:
  ars <- as.data.frame(ars)
  corrcoef <- as.matrix(corrcoef)
  
  
  
  # Check if 'ars' & 'corrcoef' are correctly provided
  if (!is.data.frame(ars) || !is.matrix(corrcoef)) {
    stop("Invalid input: 'ars' must be data frame and 'corrcoef' must be matrix.")
  }
  
  # Initialize sign vector
  CorSign <- character(nrow(ars))
  
  # Iterate through each row in arcs dataframe
  for (b in 1:nrow(ars)) {
    row_index <- match(ars[b, 1], colnames(corrcoef))
    col_index <- match(ars[b, 2], colnames(corrcoef))
    
    # Check if either index is NA, which means names didn't match
    if (is.na(row_index) || is.na(col_index)) {
      CorSign[b] <- "NA"
    } else {
      # Extract correlation coefficient
      corr_value <- corrcoef[row_index, col_index]
      
      # Determine sign based on correlation value
      if (is.na(corr_value)) {
        CorSign[b] <- "NA"
      } else if (corr_value > 0) {
        CorSign[b] <- "+"
      } else if (corr_value < 0) {
        CorSign[b] <- "-"
      } else {
        CorSign[b] <- "0"  # For exactly zero, might decide what to do based on context
      }
    }
  }
  
  return(CorSign)
}


# calculate_cor_sign <- function(ars, corrcoef) {
#   CorSign <- rep("+", nrow(ars))
#   for (b in 1:nrow(ars)) {
#     CorSign[b] <- ifelse(corrcoef[match(ars[b,1], colnames(corrcoef)), match(ars[b,2], colnames(corrcoef))] > 0, "+", "-")
#   }
#   return(CorSign)
# }

