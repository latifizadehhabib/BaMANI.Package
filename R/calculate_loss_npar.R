# Calculating loss & npar table
calculate_loss_npar <- function(threshold, temp_list_merge, discretized_data, nboot, cl, Black_List) {
  
  print("Starting 'calculate_loss_npar' inputs:")
  
  source("calculate_loss_npar_table.R")
  calculate_loss_npar_table <- calculate_loss_npar_table (threshold, 
                                                          temp_list_merge, 
                                                          discretized_data, 
                                                          nboot, cl, Black_List)
  return(calculate_loss_npar_table)
}