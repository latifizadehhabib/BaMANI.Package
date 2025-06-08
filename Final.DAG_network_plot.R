Final.DAG_network_plot <- function(augmented_edge_list,
                                        possible_seed_arcs_filter,
                                        data, discretized_data,
                                        possible.white.list,
                                        Black_List,
                                        nboot, cl,
                                        corrcoef
                                      ) {
  
  
  set.seed(2023) 


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

  
  print("Structure & content of possible.white.list:")
  print(str(possible.white.list))
  print(head(possible.white.list))

  # browser()
  # --------------------------------------- 
  #  if possible.white.list is empty
  if (is.null(possible.white.list) | ncol(possible.white.list) == 0 | nrow(possible.white.list) ==0 ) {
    possible.white.list <- data.frame(from=character(), to=character())
  } else{}
 
    set.seed(2023) 
  # mmhc
  arstr <- boot.strength(discretized_data, R = nboot, algorithm = "mmhc", cluster = cl, algorithm.args = list(whitelist = possible.white.list, blacklist = Black_List))
  ave.BRCA <- averaged.network(arstr)

  # --------------------------- 
  arcs(ave.BRCA) <- directed.arcs(ave.BRCA) 
  
  arcs.BRCA <- arcs(ave.BRCA)
  arcs.BRCA <- as.data.frame(arcs.BRCA)

  #fitted_network <- bn.fit(ave.BRCA, data = data)
  fitted_network <- bn.fit(ave.BRCA, data = discretized_data)


  # ----------------------------------  
  BRCA_str = arc.strength(ave.BRCA, data = discretized_data)
  # ----------------------------------  
  # weight.strength <- BRCA_str$strength

    #browser()
  
  # ----------------------------------
  arc_slopes <- data.frame(from = character(), to = character(), slope = numeric())

  # print("check 5")

  # iterate over all nodes in network
  for(node in nodes(fitted_network)) {
    parents <- parents(fitted_network, node)
    
    # iterate over all parent nodes & get slope coefficients
    for(parent in parents) {
      slope <- coef(fitted_network)[[node]][[parent]]
      
      arc_slopes <- rbind(arc_slopes, data.frame(from = parent, to = node, slope = slope))
    }
  }
  #browser()

  # ---------------------------------- 
  arc_slopes.strength <- merge(BRCA_str, arc_slopes, by = c("from", "to"), all = TRUE)
  
  cat("------------------------------", "\n")
  print("Arc slope and strength:")
  cat("------------------------------", "\n")
  print(arc_slopes.strength)
  
  # browser()
  
  # ----------------------------------  
  transformed_strength_values <- sapply(arc_slopes.strength$strength, function(p) -log10(p))
  
  
  # browser()
  
  max_value <- max(transformed_strength_values[is.finite(transformed_strength_values)], na.rm = TRUE)
  transformed_strength_values[is.infinite(transformed_strength_values)] <- max_value
  
  
  normalized_weights <- rescale(transformed_strength_values, to = c(0, 1)) # Adjust range as needed
  
  #browser()
  # --------------------------
  # Calculate intercepts for each node
  intercepts <- list()
  for(node in nodes(fitted_network)) {
    node_coefs <- coef(fitted_network)[[node]]
    intercepts[[node]] <- node_coefs["(Intercept)"]
  }
  
  # #-------------------------
  # nodes <- data.frame(id = unique(c(arc_slopes.strength$from, arc_slopes.strength$to)), 
  #                     label = unique(c(arc_slopes.strength$from, arc_slopes.strength$to)))
  
  nodes <- data.frame(id = colnames(discretized_data), 
                      label = colnames(discretized_data))
  
  
  #browser()
    # #-------------------------
  
  nodes_intercept <- data.frame(
    id = colnames(discretized_data), 
    label = sapply(colnames(discretized_data), 
                   function(x) paste(x, " (", formatC(intercepts[[x]], format = "f", digits = 3), ")", sep="")),
    group = 1
  )
  
  cat("------------------------------", "\n")
  print("nodes_intercept:")
  cat("------------------------------", "\n")
  print(nodes_intercept)
  
  #browser()
  # -----------------------------------
  source("calculate_cor_sign.R")
  CorSign <- calculate_cor_sign(arcs.BRCA, corrcoef)
  
  HLarcs <- arcs.BRCA[CorSign == "-",]
  
  #browser()
  # ----------------------------------- 
  
  arc_slopes.strength$key <- with(arc_slopes.strength, paste(from, to, sep = "_"))
  HLarcs_with_key <- transform(HLarcs, key = paste(from, to, sep = "_"))
  
  in_HLarcs <- arc_slopes.strength$key %in% HLarcs_with_key$key
  
  #browser()
  
  arc_slopes.strength$key <- NULL
  
  # -------------------------- 
  edges <- data.frame(
    from = arc_slopes.strength$from,
    to = arc_slopes.strength$to,
    arrows = 'to',
    #color = ifelse(in_HLarcs, "red", "black"),
    color = ifelse(arc_slopes.strength$slope > 0, "black", "red"),
    
    label = paste0("  ", signif(arc_slopes.strength$slope, digits = 3), "  "),
    # label = paste0("  ", as.character(signif(arc_slopes.strength$slope, digits = 3)), "  "),
    value = normalized_weights
    # ,    
  )
  #browser()
  # -------------------------- 
  final_DAG_detail <- data.frame(
    from = arc_slopes.strength$from, 
    to = arc_slopes.strength$to, 
    #Arc_Color = ifelse(in_HLarcs, "red", "black"), 
    Arc_Color = ifelse(arc_slopes.strength$slope > 0, "black", "red"),
    
    Effect_Size = paste0("  ", as.character(signif(arc_slopes.strength$slope, digits = 3)), "  "),  
    Arc_Strength = paste0("  ", as.character(signif(normalized_weights, digits = 6)), "  ")   
  )
  
  # --------------------------
  
  network <- visNetwork(nodes, edges, width = "100%") %>%
    visNodes(shape = "ellipse",
             font = list(size = 12,
                         vadjust = 0,
                         bold = FALSE,
                         color = "black")) %>%
    visEdges(smooth = TRUE,
             # value = "value",
             # label = "label",
             # value = "value",
             font = list(size = 12, align = "top", color = "black", background = 'rgba(255, 255, 255, 0.7)'),
             # font = list(size = edges$font.size, align = "top", color = "black", background = 'rgba(255, 255, 255, 0.7)'),
             # font = list(size = 12, align = "top", color = "black", background = 'rgba(255, 255, 255, 0.7)')) %>%
             # font = list(size = 12, align = "top", color = "black", background = 'rgba(255, 255, 255, 0.7)'),
             # width = edges$value) %>%
             width = "value",
             labelHighlightBold = FALSE) %>%
              # label = "label",
    visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE),
               nodesIdSelection = TRUE) %>%
    visLayout(randomSeed = 123,
              improvedLayout = TRUE)  %>%
    visPhysics(solver = "forceAtlas2Based", 
               forceAtlas2Based = list(gravitationalConstant = -30,  
                                       centralGravity = 0.0092,  
                                       springLength = 170,  
                                       springConstant = 0.0091)) 
  # --------------------------
  
  
  print("Network object created:")
  print(summary(network))  
  print(network)
  
  # -------------------------- 
  network_intercept <- visNetwork(nodes_intercept, edges, width = "100%") %>%
    visNodes(shape = "ellipse",
             font = list(size = 12,
                         vadjust = 0,
                         bold = F,
                         color = "black")) %>%
             # font = list(size = 12, face = "bold"),
             # fixed = list(x = TRUE, y = TRUE)) %>%
    visEdges(smooth = TRUE,
             # value = "value",
             # label = "label",
             # value = "value",
             font = list(size = 12, align = "top", color = "black", background = 'rgba(255, 255, 255, 0.7)'),
             # font = list(size = edges$font.size, align = "top", color = "black", background = 'rgba(255, 255, 255, 0.7)'),
             # font = list(size = edges$font.size, align = "top", color = "black", background = 'rgba(255, 255, 255, 0.7)')) %>%
             # font = list(size = 12, align = "top", color = "black", background = 'rgba(255, 255, 255, 0.7)')) %>%
             # font = list(size = 12, align = "top", color = "black", background = 'rgba(255, 255, 255, 0.7)'),
             # label = edges$label,  # Ensure the edge labels are used
             # width = edges$value) %>%
             width = "value",
             labelHighlightBold = FALSE) %>%
            #   label = "label",
            # width = "value") %>% 
    visOptions(highlightNearest = list(enabled = T, hover = T),
               nodesIdSelection = TRUE) %>%
    visLayout(randomSeed = 123, improvedLayout = TRUE) %>%
    visPhysics(solver = "forceAtlas2Based",
               forceAtlas2Based = list(gravitationalConstant = -30,
                                       centralGravity = 0.0092,
                                       springLength = 170,
                                       springConstant = 0.0091)) %>%
    # visPhysics(enabled = FALSE)
  # -------------------------- 
  # show network
  print("Network object created:")
  print(network_intercept)
  print(summary(network_intercept))  
  
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

  temp$Min.BIC_white.list <- Min.BIC_white.list  

  #  add column "Final.DAG.Arcs"
  Final.DAG.Arcs <- ifelse(apply(as.matrix(temp[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")) %in% 
                            apply(as.matrix(arcs.BRCA[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")), "1", "")

  temp$Final.DAG.Arcs <- Final.DAG.Arcs   

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
  Final.ars_both <- data.frame(from = character(), to = character())
  # Loop through each row of original data frame & add reverse as a new row after current row
  for (i in 1:nrow(arcs.BRCA)) {
    Final.ars_both <- rbind(Final.ars_both, arcs.BRCA[i, ])
    Final.ars_both <- rbind(Final.ars_both, data.frame(from = arcs.BRCA[i, "to"], to = arcs.BRCA[i, "from"]))
  }
  rownames(Final.ars_both) <- NULL
  # --------------------------- 
  #  add column "Excluded"
  # converts first data frame into a matrix, concatenates "from" & "to" columns into a single string for each row using paste(x, collapse = "|"), & returns a vector of strings representing arcs in temp.
  Excluded <- ifelse(
    apply(as.matrix(temp[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")) %in% 
      # apply(as.matrix(arcs.BRCA[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")),
      apply(as.matrix(Final.ars_both[, c("from", "to")]), 1, function(x) paste(x, collapse = "|")),
    
    "", "1")

  temp$Excluded <- Excluded   
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
    group_by(Edge_No) %>%   
    summarize(Hit.Count = if (any(!is.na(Hit.Count))) max(Hit.Count, na.rm = TRUE) else NA,
              Min.BIC_clear.direction = ifelse(sum(Min.BIC_clear.direction, na.rm = TRUE) >= 1, "1", ""),
              Min.BIC.unclear.direction = ifelse(sum(Min.BIC.unclear.direction, na.rm = TRUE) >= 1, "1", ""),
              Unclear.direction = ifelse(sum(Unclear.direction, na.rm = TRUE) >= 1, "1", ""),
              Excluded = ifelse(sum(Excluded, na.rm = TRUE) >= 1, "1", ""),
              Min_strength = first(Min_strength),
              Max_strength = first(Max_strength))%>%
    ungroup()

  temp2 <- temp2[!(is.na(temp2$Hit.Count) | temp2$Hit.Count == "" | temp2$Hit.Count == 0), ]

  temp2$Excluded[temp2$Min.BIC_clear.direction == "1"] <- ""
  temp2$Excluded[temp2$Min.BIC.unclear.direction == "1"] <- ""
  temp2$Excluded[temp2$Unclear.direction == "1"] <- ""

  rownames(temp2) <- NULL 
  Alg.Count_arcs.strength.table <- temp2

  print("Function return objects:")
  print(list(
    network = network,
    final_DAG_detail = final_DAG_detail,
    arc_slopes.strength = arc_slopes.strength
  ))
# highlight_arcs <- arc_slopes.strength[arc_slopes.strength$slope < 0, ]
highlight_arcs <- arc_slopes.strength[arc_slopes.strength$slope < 0, c("from", "to")]


  return(list(fitted_network = fitted_network, 
              Alg.Count_arcs.strength.table = Alg.Count_arcs.strength.table, 
              network = network,
              network_intercept = network_intercept,
              final_DAG_detail = final_DAG_detail,
              
              arcs.BRCA = arcs.BRCA,
              # # -------------
              # P_strength = weight.strength,
              arc_slopes.strength= arc_slopes.strength, 
              plotFunction = function() {
                #strength.plot(ave.BRCA, BRCA_str, shape = "ellipse", highlight = list(arcs = HLarcs))
                strength.plot(ave.BRCA, BRCA_str, shape = "ellipse", highlight = list(arcs = highlight_arcs))

                # graphviz.plot(ave.BRCA, BRCA_str, shape = "ellipse", highlight = list(arcs = HLarcs))
              }
              
              ))
  }
