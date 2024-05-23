Contour_plot_userSelected_feature <- function(
                                      data, discretized_data,
                                      fBRCABN,
                                      Status,
                                      userSelected_key_feature) {
  
  cat("------------------------------", "\n")
  print("Starting 'Contour_plot_userSelected_feature' inputs:")
  cat("------------------------------", "\n")
  
  
  print(list(
    Status = Status,
    userSelected_key_feature = userSelected_key_feature
  ))
  
  # print("hi im status ")
    # ---------------
  if (!is.null(fBRCABN)) {
    cat("fBRCABN is provided. Displaying learned parameters:\n")
    print(coef(fBRCABN))
  } else {
    cat("No fBRCABN provided. Skipping related calculations.\n")
    print(coef(fBRCABN))
    print((fBRCABN))
  }
  
  
  # ----------------------------------
  #source("generatePlot.R")
  # ----------------------------------
    # Cantour Plot 
  
  # if (!is.null(fBRCABN)) {

    # key_feature <- names(data)[2] # column of data that is selected as ""key_feature"" by user
    key_feature <- userSelected_key_feature
    
    all.features <- colnames(discretized_data)
    
    cellTypes <- all.features[!all.features %in% c(key_feature, Status)]
    # cellTypes <- all.features[!all.features %in% c("B", "A")]
    
    plots_list <- vector("list", length(cellTypes))
    names(plots_list) <- cellTypes
    
    for(cellType in cellTypes) {
      tryCatch({
        # Generate plot with userSelectedCell
        suppressWarnings({
          # plot <- generatePlot(cellType, fBRCABN, data)
          plot <- generatePlot(Status, key_feature, cellType, fBRCABN, data)
          # plot <- generatePlot(userSelectedCell, cellType, fBRCABN, data)
          
          # Store generated plot in list
          plots_list[[cellType]] <- plot
        })
      }, error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
      })
    }
    return(plots_list)
  # }
}

