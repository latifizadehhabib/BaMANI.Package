calculate_cor_sign <- function(ars, corrcoef) {
  
  
  set.seed(2023) 
  
  if (!("data.frame" %in% class(ars))) {
    ars <- as.data.frame(ars)
  }
  
  if (!all(c("from", "to") %in% names(ars))) {
    stop("The 'ars' data frame must contain 'from' and 'to' columns.")
  }
  
  cat("------------------------------", "\n")
  print("List of Arcs in Learning process:")
  cat("------------------------------", "\n")
  # print((ars))
  for (i in 1:nrow(ars)) {
    cat(ars$from[i], "\u27F6", ars$to[i], "\n")
  }
  
  
  ars <- as.data.frame(ars)
  corrcoef <- as.matrix(corrcoef)
  
  
  if (!is.data.frame(ars) || !is.matrix(corrcoef)) {
    stop("Invalid input: 'ars' must be data frame and 'corrcoef' must be matrix.")
  }
  
  CorSign <- character(nrow(ars))
  
  for (b in 1:nrow(ars)) {
    row_index <- match(ars[b, 1], colnames(corrcoef))
    col_index <- match(ars[b, 2], colnames(corrcoef))
    
    if (is.na(row_index) || is.na(col_index)) {
      CorSign[b] <- "NA"
    } else {
      corr_value <- corrcoef[row_index, col_index]
      
      if (is.na(corr_value)) {
        CorSign[b] <- "NA"
      } else if (corr_value > 0) {
        CorSign[b] <- "+"
      } else if (corr_value < 0) {
        CorSign[b] <- "-"
      } #else {
      
    }
  }
  
  return(CorSign)
}
