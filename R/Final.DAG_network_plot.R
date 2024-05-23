Final.DAG_network_plot <- function(augmented_edge_list,
                                        possible_seed_arcs_filter,
                                        data, discretized_data,
                                        possible.white.list,
                                        Black_List,
                                        nboot, cl,
                                        corrcoef
                                      ) {


  # library(bnlearn)
  cat("------------------------------", "\n")
  print("Starting Final.DAG_network_plot with inputs:")
  cat("------------------------------", "\n")
  
  print(nboot)
  
  print("WHITE LIST")
  print(possible.white.list)

  possible.white.list <- as.data.frame(possible.white.list)
  discretized_data <- as.data.frame(discretized_data)
  Black_List <- as.data.frame(Black_List)

  print("After discretized_data processing:")
  print(head(discretized_data))

  # structure & content of possible.white.list & Black_List
  print("Structure & content of possible.white.list:")
  print(str(possible.white.list))
  print(head(possible.white.list))

  print("Structure & content of Black_List:")
  print(str(Black_List))
  print(head(Black_List))

  print("Dimensions of possible.white.list:")
  print(dim(possible.white.list)) 

  print("Dimensions of Black_List:")
  print(dim(Black_List))
  print("nrow")
  print(nrow(possible.white.list))
  # print("condi2")
  print(names(possible.white.list))

  if (nrow(possible.white.list) > 0) {
  # print("i made it here 2")

    # Safe to proceed with operations on possible.white.list

  #  if possible.white.list is empty
  if (ncol(possible.white.list) == 0) {
    # Handle empty case - for example, by skipping intersection operation
    # Or initialize possible.white.list to match structure of Black_List but with no rows
    possible.white.list <- data.frame(from=character(), to=character())
  } else{
    # Now, proceed with intersection (or any other operation) as intended
    # common_arcs <- intersect(possible.white.list, Black_List)

    #  for common arcs & remove them from whitelist
    common_arcs <- intersect(possible.white.list, Black_List)
    if(length(common_arcs) > 0) {
      possible.white.list <- setdiff(possible.white.list, common_arcs)
    }
  }
  # print("i made it here 4")

  # ---------------
  arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl, algorithm.args = list(whitelist = possible.white.list, blacklist = Black_List))
  ave.BRCA <- averaged.network(arstr)

  arcs(ave.BRCA) <- directed.arcs(ave.BRCA) # ignore undirected arcs
  arcs.BRCA <- arcs(ave.BRCA)
  arcs.BRCA <- as.data.frame(arcs.BRCA)

  #fBRCABN <- bn.fit(ave.BRCA, data = data)
  fBRCABN <- bn.fit(ave.BRCA, data = discretized_data)

  


  # ----------------
  BRCA_str = arc.strength(ave.BRCA, data = discretized_data)
  weight.strength <- BRCA_str$strength


  # ----------------------------------
  arc_slopes <- data.frame(from = character(), to = character(), slope = numeric())

  print("i made it here 5")

  # iterate over all nodes in network
  for(node in nodes(fBRCABN)) {
    # get names of parent nodes
    parents <- parents(fBRCABN, node)
    
    # iterate over all parent nodes & get slope coefficients
    for(parent in parents) {
      # get slope coefficient
      slope <- coef(fBRCABN)[[node]][[parent]]
      
      # add arc & slope coefficient to data frame
      arc_slopes <- rbind(arc_slopes, data.frame(from = parent, to = node, slope = slope))
    }
  }
  arcs_strength <- data.frame(from = as.character(arcs.BRCA[, 1]), 
                              to = as.character(arcs.BRCA[, 2]), 
                              P_strength = weight.strength)

  # arc_slopes.strength <- merge(arcs_strength, arc_slopes)
  arc_slopes.strength <- merge(arcs_strength, arc_slopes, by = c("from", "to"), all = TRUE)


  #------------------------------------------------  visNetwork
  # Apply transformation
  transformed_values <- sapply(arcs_strength$P_strength, function(p) -log10(p))
  # transformed_values <- sapply(P_strength, function(p) -log10(p))


  # Handle infinite values after transformation
  max_value <- max(transformed_values[!is.infinite(transformed_values)], na.rm = TRUE)
  transformed_values[is.infinite(transformed_values)] <- max_value

  # Normalize transformed values
  normalized_weights <- rescale(transformed_values, to = c(0, 1))
  #------------------------------------------------ 
  # create nodes & edges datafrme

  nodes <- data.frame(id = unique(c(arc_slopes.strength$from, arc_slopes.strength$to)), 
                      label = unique(c(arc_slopes.strength$from, arc_slopes.strength$to)))

  # --------------------------
  # -----------------------------------
  #source("calculate_cor_sign.R")
  CorSign <- calculate_cor_sign(arcs.BRCA, corrcoef)
  
  HLarcs <- arcs.BRCA[CorSign == "-",]
  
  # ----------------------------------- Replace arc_slopes.strength$slope < 0  with  arcs.BRCA$CorSign == "-"
  
  # HLarcs' is subset datafrme containing only negative correlations
  # Create a key column in arc_slopes.strength & temporarily in HLarcs for comparison
  arc_slopes.strength$key <- with(arc_slopes.strength, paste(from, to, sep = "_"))
  HLarcs_with_key <- transform(HLarcs, key = paste(from, to, sep = "_"))
  
  # Create a logical vector for checking presence in HLarcs using temporary 'key' column
  in_HLarcs <- arc_slopes.strength$key %in% HLarcs_with_key$key
  
  # Remove key column from arc_slopes.strength after its use
  arc_slopes.strength$key <- NULL
  
  
  # Apply colors based on presence of arcs in HLarcs
  edges <- data.frame(
    from = arc_slopes.strength$from, 
    to = arc_slopes.strength$to, 
    arrows = 'to', 
    color = ifelse(in_HLarcs, "red", "black"), 
    label = paste0("  ", as.character(signif(arc_slopes.strength$slope, digits = 2)), "  "),  # Added spaces
    value = normalized_weights  # Use normalized arc strength as value
  )
  
  # Ensure final DAG detail also uses this color logic
  final_DAG_detail <- data.frame(
    from = arc_slopes.strength$from, 
    to = arc_slopes.strength$to, 
    color = ifelse(in_HLarcs, "red", "black"), 
    Effect_Size = paste0("  ", as.character(signif(arc_slopes.strength$slope, digits = 2)), "  "),  # Added spaces
    Arc_strength = paste0("  ", as.character(signif(normalized_weights, digits = 2)), "  ")   # Use normalized arc strength as value
  )
  # --------------------------

  network <- visNetwork(nodes, edges, width = "100%") %>%
    visNodes(shape = "circle",
            font = list(size = 12, 
                        vadjust = 0, 
                        bold = TRUE, 
                        color = "black")) %>%
    visEdges(smooth = TRUE, 
            value = "value",  
            font = list(size = 15, color = "black", background = 'rgba(255, 255, 255, 0.7)')) %>%
    visOptions(highlightNearest = list(enabled = T, hover = T),
              nodesIdSelection = TRUE) %>%
    visLayout(randomSeed = 123, 
              improvedLayout = TRUE)  %>%
    visPhysics(solver = "forceAtlas2Based",  # physics solver
              forceAtlas2Based = list(gravitationalConstant = -50,  # Adjust as needed
                                      centralGravity = 0.005,  # Adjust as needed
                                      springLength = 100,  # Adjust as needed
                                      springConstant = 0.18))  # Adjust as needed 


  print("Network object created:")
  print(summary(network))  # or simply print(network) if that's more informative
  
  # ----------------------------------- 
  temp <- augmented_edge_list %>%
    select(-contains("_CorSign"), -contains("_strength"))
  # library(dplyr)
  Max.min.Col <- temp %>% dplyr::select(contains(".strength"))
  Max.min.Col <- as.matrix(Max.min.Col)

  # ----------------------------
  for(i in unique(temp$Edge_No)){  # we use command "unique", because for each "Edge_No", we have two rows (A-->B & B-->A)
    inx<- which(temp$Edge_No == i)
    if (any(!is.na(as.numeric(Max.min.Col[inx, ])))) {
      temp$Min_strength[inx[1]] <- min(as.numeric(Max.min.Col[inx, ]), na.rm=TRUE)
      temp$Min_strength[inx[2]] <- min(as.numeric(Max.min.Col[inx, ]), na.rm=TRUE)
      temp$Max_strength[inx[1]]<- max(as.numeric(Max.min.Col[inx, ]), na.rm=TRUE)
      temp$Max_strength[inx[2]]<- max(as.numeric(Max.min.Col[inx, ]), na.rm=TRUE)
    } else {
      # If all values are missing, set min & max strength to NA
      temp$Min_strength[inx[1]] <- NA
      temp$Min_strength[inx[2]] <- NA
      temp$Max_strength[inx[1]]<- NA
      temp$Max_strength[inx[2]]<- NA
    }
  }
  # -------------------------
  # remove two columns "Min.strength & Min.strength" which is different from "Min_strength & Min_strength""
  temp <- temp %>% select(-c(Min.strength, Max.strength))
  #  add column "Clear_direction" with value 1 if that arc is in final DAG is in "possible_seed_arcs_filter"
  clear.direction <- ifelse(apply(as.matrix(temp[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")) %in% 
                              apply(as.matrix(possible_seed_arcs_filter[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")), "1", "")

  temp$clear.direction <- clear.direction

  #  add column "Min.BIC_white.list"
  Min.BIC_white.list <- ifelse(apply(as.matrix(temp[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")) %in% 
                                apply(as.matrix(possible.white.list[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")), "1", "")

  temp$Min.BIC_white.list <- Min.BIC_white.list  # sum(nchar(gsub("[^1]", "", temp$Black_list))) # how many arc is in blacklist

  #  add column "Final.DAG.Arcs"
  Final.DAG.Arcs <- ifelse(apply(as.matrix(temp[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")) %in% 
                            apply(as.matrix(arcs.BRCA[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")), "1", "")

  temp$Final.DAG.Arcs <- Final.DAG.Arcs    # sum(as.numeric(temp$Final.DAG.Arcs), na.rm = T)== nrow(arcs.BRCA)

  #  add column "Min.BIC_clear.direction"
  temp$Min.BIC_clear.direction <- ifelse(temp$clear.direction == "1" &
                                          # (temp$unclear_direction == "" | temp$Not_this_direction == "" | temp$clear.direction == "1") &
                                          temp$Min.BIC_white.list == "1" & 
                                          temp$Final.DAG.Arcs == "1",
                                        "1", "")
  # sum(as.numeric(temp$Min.BIC_clear.direction ), na.rm = T)


  #Min.BIC.unclear: Create a new column that checks if all three columns have a value of "1"
  temp$Min.BIC.unclear.direction <- ifelse( (temp$unclear_direction == "1" | temp$Not_this_direction == "1")  &
                                            # (temp$unclear_direction == "1" | temp$Not_this_direction == "1" | temp$clear.direction == "") &
                                            # temp$clear.direction == "" &
                                            temp$Min.BIC_white.list == "1" &
                                            temp$Final.DAG.Arcs == "1",
                                          "1", "")

  # Create a new column that checks if "Final.DAG.Arcs" & "unclear_direction" columns have a value of "1" & "Min.BIC_white.list" has an empty string ""
  temp$Unclear.direction <- ifelse(temp$Final.DAG.Arcs == "1" & 
                                    # temp$clear.direction == "" & 
                                    temp$Min.BIC_white.list == "" & 
                                    (temp$unclear_direction == "1" | temp$Not_this_direction == "1"),
                                    # temp$unclear_direction == "1" ,
                                    # (temp$unclear_direction == "1" | temp$Not_this_direction == "1" | temp$clear.direction == ""),
                                  "1", "")

  # sum(as.numeric(temp$Unclear.direction ), na.rm = T)

  #  we put NA for any row that have value "1" in column "Not_this_direction" then assign "" value to column "Hit.Count" of corresponding row
  temp$Hit.Count[temp$Not_this_direction == "1"] <- ""
  temp$Hit.Count[temp$Black_list == "1"] <- ""
  # ---------------------------:   # Add reverse of each row after current row

  arcs.BRCA <- as.data.frame(arcs.BRCA)
  # Create empty data frame with same column names
  Final.ars_both <- data.frame(from = character(), to = character())
  # Loop through each row of original data frame & add reverse as a new row after current row
  for (i in 1:nrow(arcs.BRCA)) {
    Final.ars_both <- rbind(Final.ars_both, arcs.BRCA[i, ])
    Final.ars_both <- rbind(Final.ars_both, data.frame(from = arcs.BRCA[i, "to"], to = arcs.BRCA[i, "from"]))
  }
  # Reset row names of new data frame
  rownames(Final.ars_both) <- NULL
  # --------------------------- 
  #  add column "Excluded"
  # converts first data frame into a matrix, concatenates "from" & "to" columns into a single string for each row using paste(x, collapse = "|"), & returns a vector of strings representing arcs in temp.
  Excluded <- ifelse(
    apply(as.matrix(temp[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")) %in% 
      # apply(as.matrix(arcs.BRCA[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")),
      apply(as.matrix(Final.ars_both[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")),
    
    "", "1")

  temp$Excluded <- Excluded    # sum(as.numeric(temp$Final.DAG.Arcs), na.rm = T)== nrow(arcs.BRCA)
  temp$Excluded[temp$Black_list == "1"] <- "1"
  temp$Excluded[temp$Min.BIC_clear.direction == "1"] <- ""
  temp$Excluded[temp$Min.BIC.unclear.direction == "1"] <- ""
  temp$Excluded[temp$Unclear.direction == "1"] <- ""

  Alg.count.table <- temp %>% select(c(Edge_No, Hit.Count, Min.BIC_clear.direction, Min.BIC.unclear.direction,
                                      Unclear.direction, Excluded, Min_strength, Max_strength ))

  # Move Min_strength & Max_strength to last columns: everything() selects all remaining columns. Finally, Min_strength & Max_strength are added to end of data frame.
  Alg.count.table <- as.data.frame(lapply(Alg.count.table, as.numeric))
  library(dplyr)
  temp2 <- Alg.count.table %>%
    group_by(Edge_No) %>%   #  if there are any non-missing values in vector using any function & is.na function. If there are no non-missing values, return NA instead of calling max function.
    summarize(Hit.Count = if (any(!is.na(Hit.Count))) max(Hit.Count, na.rm = TRUE) else NA,
              Min.BIC_clear.direction = ifelse(sum(Min.BIC_clear.direction, na.rm = TRUE) >= 1, "1", ""),
              Min.BIC.unclear.direction = ifelse(sum(Min.BIC.unclear.direction, na.rm = TRUE) >= 1, "1", ""),
              Unclear.direction = ifelse(sum(Unclear.direction, na.rm = TRUE) >= 1, "1", ""),
              Excluded = ifelse(sum(Excluded, na.rm = TRUE) >= 1, "1", ""),
              Min_strength = first(Min_strength),
              Max_strength = first(Max_strength))%>%
    ungroup()

  #   removes rows with NA, empty string, or zero values in Hit.Count column
  temp2 <- temp2[!(is.na(temp2$Hit.Count) | temp2$Hit.Count == "" | temp2$Hit.Count == 0), ]

  temp2$Excluded[temp2$Min.BIC_clear.direction == "1"] <- ""
  temp2$Excluded[temp2$Min.BIC.unclear.direction == "1"] <- ""
  temp2$Excluded[temp2$Unclear.direction == "1"] <- ""

  rownames(temp2) <- NULL 
  Alg.Count_arcs.strength.table <- temp2

  # Before returning from function
  print("Function return objects:")
  print(list(
    network = network,
    final_DAG_detail = final_DAG_detail
    # Include other relevant objects 
  ))

  return(list(fBRCABN = fBRCABN, 
              Alg.Count_arcs.strength.table = Alg.Count_arcs.strength.table, 
              network = network,
              final_DAG_detail = final_DAG_detail,
              
              arcs.BRCA = arcs.BRCA,
              P_strength = weight.strength,
              arc_slopes.strength= arc_slopes.strength, 
              plotFunction = function() {
                strength.plot(ave.BRCA, BRCA_str, shape = "ellipse", highlight = list(arcs = HLarcs))
                # graphviz.plot(ave.BRCA, BRCA_str, shape = "ellipse", highlight = list(arcs = HLarcs))
              }
              
              ))
  }
  }
