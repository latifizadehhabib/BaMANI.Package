data_process_Correlation <- function(data) {
  
  
  set.seed(2023) 
  

  cat("------------------------------", "\n")
  print("Starting 'data_process_Correlation' inputs:")
  cat("------------------------------", "\n")
  
  data[] <- lapply(data, as.numeric)
  print(class(data))
  
  
  data[] <- lapply(data, function(col) {
    # if (is.character(col)) {
    if (is.character(col) || is.integer(col)) {
      numeric_col <- suppressWarnings(as.numeric(col))
      if (!any(is.na(numeric_col))) {
        return(numeric_col)
      } else {
        return(col)  
      }
    }
    return(col)
  })
  
  
   data <- as.data.frame(data)
   
   
   tmp <- discretize(data, method = "interval", breaks = c(2,rep(15,16)))
   
   dis.data <- as.data.frame(lapply(tmp, function(x) (as.numeric(x) - 1)/max(as.numeric(x) -1)))
   dis.data <- dis.data[complete.cases(dis.data),]
   
   corrcoef <- cor(sapply(dis.data, as.numeric))
   colnames(corrcoef) <- colnames(dis.data)
   rownames(corrcoef) <- colnames(dis.data)
   
   
  
  cat("------------------------------", "\n")
  cat(sprintf("characteristics of 'corrcoef':"), "\n")
  cat("------------------------------", "\n")
  str(corrcoef)
  cat("------------------------------", "\n")

  return(list(corrcoef = corrcoef, discretized_data= dis.data, data = data))
  }
