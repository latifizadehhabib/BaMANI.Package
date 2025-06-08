# Calculating BIC & find min BIC
calculate_bic_and_find_min <- function(discretized_data, npar_table, Loss_table, threshold, parents.list_all_threshold) {
  
  set.seed(2023) 
  
  cat("------------------------------", "\n")
  print("Starting 'calculate_bic_and_find_min' inputs:")
  cat("------------------------------", "\n")
  
  
  source("calculate_bic.R")
  source("find_min_bic.Parent_whitelist_acyclic.R")
  
  BIC_merged_table <- calculate_bic(discretized_data, npar_table, Loss_table, threshold)
  find_min_bic.Parent_whitelist_acyclic <- find_min_bic.Parent_whitelist_acyclic(BIC_merged_table, npar_table, parents.list_all_threshold)
  
  return(list(bic_merged_table = BIC_merged_table, find_min_bic = find_min_bic.Parent_whitelist_acyclic))
}