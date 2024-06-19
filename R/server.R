# File: R/server.R

#' Server function for My Shiny App
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @export
server <- function(input, output, session) {

  # --------------------------
  # Source all the necessary files
  source_files <- c("run_single_algorithm_directed.R",
                    "createVariableDistributionPlot.R",
                    "data_process_Correlation.R",
                    "run_algorithm_directed.R",
                    "run_algorithm_Undirected.R",
                    "augmented_edge_table.R",
                    "uninformative_arcs_removal.R",
                    "finding_threshold_values.R",
                    "temp.white_thresh.cols.R",
                    "calculate_loss_npar_table.R",
                    "calculate_bic.R",
                    "find_min_bic.Parent_whitelist_acyclic.R",
                    "Final.DAG_network_plot.R",
                    "calculate_cor_sign.R",
                    "Contour_plot_userSelected_feature.R",
                    "generatePlot.R",
                    "DAG_network_plot.arc.lable.R",
                    "Diagnostic_plot.R",
                    "diagnostic_plot_White_Final.R",
                    "plot_Algorithm.Count_arcs.strength.R",
                    "renderStyledTable.R")

  lapply(source_files, function(file) source(file.path("R", file)))
  # lapply(source_files, source)
  # --------------------------



  # Reactive value to store file input
  fileInputState <- reactiveVal(NULL)

  # Function to render fileInput dynamically
  renderFileInput <- function() {
    output$dynamicFileInput <- renderUI({
      fileInput("dataFile", "Data File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
    })
  }

  # Initial rendering of fileInput
  renderFileInput()

  # Update fileInputState when fileInput changes
  observeEvent(input$dataFile, {
    fileInputState(input$dataFile)
  })
  #-------
  userSelected <- reactiveVal(FALSE)

  observeEvent(input$userSelected_Status, {
    userSelected(TRUE) # Set flag when user changes selection
  }, ignoreInit = TRUE) # ignoreInit ensures this doesn't trigger at app startup

  #------ # Initialize reactive values to store selections
  selectedInputs <- reactiveValues(status = NULL, keyFeature = NULL, secondaryFeature = NULL)
  contour_plot_initial <- reactiveVal(FALSE)
  #-------------
  update_clicked <- reactiveVal(TRUE)
  observeEvent(input$updateButton, {
    selectedInputs$status <- input$userSelected_Status
    # selectedInputs$status <- input$userSelectedStatus #(First version)
    selectedInputs$keyFeature <- input$userSelected_key_feature
    selectedInputs$secondaryFeature <- input$selectedCellType
    update_clicked(TRUE)
  })

  #-------- Reactive value to track if dataset contains categorical columns
  has.Categorical.Columns <- reactiveVal(FALSE)
  cycles_resolved <- reactiveVal(TRUE)


  #-------------------------------------------------------------------------------------------
  observe({
    currentTab <- input$sidebarMenu
    print(paste("Current tab:", currentTab))

    if(currentTab == "contour_plot" && !has.Categorical.Columns()) {
      print("Navigated to Comparative Analysis")
      showModal(modalDialog(
        tags$div(
          style = "font-size:18px; color:#34495E; padding: 20px 20px; background-color: #EAECEE; border-bottom-left-radius: 5px; border-bottom-right-radius: 5px;",
          tags$p(
            tags$span(
              style = "color: #d68910;",
              icon("exclamation-triangle", style = "margin-right: 6px; color: #d68910;"),
              tags$span(style = "font-weight: bold;", "Analysis Not Possible: ")
            ),
            "Your dataset does not contain any categorical columns, which are required for generating Comparative Analysis graphs. Please ensure that your dataset includes at least one categorical column to proceed with this analysis."
          )
        ),
        footer = tagList(
          tags$button("Close", type = "button", class = "btn btn-default", `data-dismiss`="modal",
                      style = "background-color: #58d68d; color: black; border: none; padding: 8px 18px; border-radius: 4px; margin: 5px;")
        ),
        easyClose = TRUE,
        size = "m" #
      ))
    }
    #--------
    # Force user back to "WhiteList Check Acyclicity" tab if cycles are not resolved and user tries to navigate away
    if (!cycles_resolved() && currentTab != "WhiteList_Check_acyclicity" && currentTab != "settings") {
      print("Unresolved cycles detected, redirecting back...")
      updateTabItems(session, "sidebarMenu", "WhiteList_Check_acyclicity")

      showModal(modalDialog(
        # title = "Unresolved Cycles Detected",
        tags$div(
          style = "font-size:18px; color:#34495E; padding: 20px 20px; background-color: #EAECEE; border-bottom-left-radius: 5px; border-bottom-right-radius: 5px;",
          tags$p(
            tags$span(
              style = "color: #c0392b;",
              icon("exclamation-circle", style = "margin-right: 6px; color: #c0392b;"),
              tags$span(style = "font-weight: bold;", "Action Required: ")
            ),
            "You must resolve all cycles before proceeding. Please check and resolve any cycles in the network."
          )
        ),
        footer = tagList(
          tags$button("OK", type = "button", class = "btn btn-default", `data-dismiss`="modal",
                      style = "background-color: #e74c3c; color: white; border: none; padding: 8px 18px; border-radius: 4px; margin: 5px;")
        ),
        easyClose = TRUE,
        size = "m" #
      ))
    }
    #--------
  })
  #-------------------------------------------------------------------------------------------
  observeEvent(input$show_nboot, {
    showModal(
      modalDialog(
        title = tags$span(
          tags$i(class = "fas fa-info-circle", style = "color: white; padding-right: 10px;"),
          "Number of bootstrap samples",
          style = "font-size: smaller; color: white; background-color: #3c8dbc; padding: 10px;"
        ),
        size = "m",
        tagList(
          tags$p(
            HTML("Enter the number of bootstrap samples to be generated, represented by the variable <code>nboot</code>. Bootstrapping is a statistical resampling method used to estimate the distribution of a statistic by repeatedly sampling, with replacement, from the observed data. The <code>nboot</code> value determines how many resampled datasets will be created during this process. Please ensure a numeric value is assigned to <code>nboot</code>; failure to do so or assigning a NULL value may result in errors during execution."),
            style = "font-size: medium; padding: 5px;")
        ),
        easyClose = TRUE
      )
    )
  })
  # ---------------------------------- Instruction
  observeEvent(input$show_threshold_level, {
    showModal(
      modalDialog(
        title = tags$span(
          tags$i(class = "fas fa-info-circle", style = "color: white; padding-right: 10px;"),
          "Threshold Level",
          style = "font-size: smaller; color: white; background-color: #3c8dbc; padding: 10px;"
        ),
        size = "m",
        tagList(
          tags$p(
            HTML("
            The <code>Threshold Level = N</code> is used to divide the arc strength interval into <code>N quantiles</code>, each representing a threshold level. The user-inputted <code>Threshold Level</code> in Bayesian network inference is for refining the consensus seed network or <code>whitelist</code> in a Directed Acyclic Graph (<code>DAG</code>). This threshold acts as a filter, determining which arcs, based on strength, are included in the final network. Arcs below the user-defined threshold are typically included, improving both network and node connectivity. The threshold aids in balancing model complexity and regression accuracy, quantified using the Bayesian Information Criterion (<code>BIC</code>). By adjusting the threshold, users can influence BIC values and the inclusion of specific arcs, facilitating the creation of a more refined and accurate model. This also ensures the exclusion of inconsistent arcs, preserving the network's consistency and reliability."),
            style = "font-size: medium; padding: 5px;")          # tags$p("Specify threshold level for significance.", style = "font-size: medium; padding: 5px;")

        ),
        easyClose = TRUE
      )
    )
  })
  # ---------------------------------- Instruction
  observeEvent(input$show_dataFile, {
    showModal(
      modalDialog(
        title = tags$span(
          tags$i(class = "fas fa-info-circle", style = "color: white; padding-right: 10px;"),
          "Data File and format",
          style = "font-size: smaller; color: white; background-color: #3c8dbc; padding: 10px;"
        ),
        size = "m",
        tagList(
          tags$p(
            HTML("Upload the <code>data</code> file in CSV format."),
            br(),
            HTML("The user-supplied dataset should be structured with each row corresponding to a unique <code>study sample</code>.
    The first row must contain the names of the features, serving as column headers. At least one of these columns should represent a
    <code>binary</code> feature, often categorizing the samples into distinct classes such as <code>diseased</code>  or <code>normal</code>.
    This binary feature is
    typically integral, representing categorical data. The remaining columns should be <code>numeric</code>, quantifying the levels or counts
    of various attributes or markers in each sample. These numeric columns can represent a variety of biological or clinical
    measurements, each providing distinctive insights into the characteristics of the study samples. It is crucial that the
    data is well-structured, with consistent and complete entries for each feature across all samples, ensuring the
    reliability and accuracy of any subsequent analyses or interpretations."),
            style = "font-size: medium; padding: 5px;")
        ),
        easyClose = TRUE
      )
    )
  })
  # ---------------------------------- Instruction
  observeEvent(input$show_BlackListFile, {
    showModal(
      modalDialog(
        title = tags$span(
          tags$i(class = "fas fa-info-circle", style = "color: white; padding-right: 10px;"),
          "BlackList File  and format",
          style = "font-size: smaller; color: white; background-color: #3c8dbc; padding: 10px;"
        ),
        size = "m",
        tagList(
          tags$p(
            HTML("Upload the <code>BlackList</code>  file in CSV format."),
            br(),
            HTML("The <code>BlackList</code> serves as a user-defined input and plays a pivotal role in the <code>structural learning</code> and network inference processes within Bayesian networks, which are illustrated as Directed Acyclic Graphs (DAGs). This component is instrumental in molding the network structure, relying primarily on <code>prior knowledge </code> regarding the causal relationships amongst the data features. This list encompasses specific arcs, or <code>directed edges </code>, that are deliberately excluded from the proposed network, serving as constraints based on established or acknowledged information. This ensures the resultant structure and its causal interactions or arcs amongst nodes are in harmony with existing knowledge and logical constraints, thereby maintaining the integrity and accuracy of the network's representation of causal relationships."),
            style = "font-size: medium; padding: 5px;")
        ),
        easyClose = TRUE
      )
    )
  })
  # ---------------------------------- Instruction
  observeEvent(input$show_Dir_AlgoDescriptions, {
    showModal(
      modalDialog(
        title = tags$span(
          tags$i(class = "fas fa-code-branch", style = "color: white; padding-right: 10px;"),
          "Directed Algorithm Descriptions",
          style = "font-size: smaller; color: white; background-color: #3c8dbc; padding: 10px;"
        ),
        size = "l",
        tagList(
          tags$p(HTML("Select Algorithms from the list:
          Incremental association with false discovery rate control - <code>IAMB.FDR</code>,
                      Practical constraint - <code>PC.STABLE</code>,
                      Grow-shrink Markov Blanket - <code>GS</code>,
                      Incremental association Markov Blanket - <code>IAMB</code>,
                      Hill climbing - <code>HC</code>,
                      Tabu search - <code>Tabu</code>,
                      Max-min hill-climbing - <code>MMHC</code>,
                      Restricted maximization - <code>RSMAX2</code>"),
                 style = "font-size: medium; padding: 5px;"),
          br(),

          tags$p(HTML("The \'Directed Algorithm\' in <code>Bayesian network inference</code>  is crucial for deducing the structure and causality in a network, represented as a Directed Acyclic Graph (<code>DAG</code>). Users employ an ensemble of different structural learning algorithms to accurately identify potential causal interactions or arcs among nodes and to estimate the <code>conditional probability distributions</code> from datasets. These algorithms contribute to constructing <code>blacklists</code> and/ or <code>whitelists</code>  of arcs, serving as a basis for exclusion and inclusion based on <code>prior knowledge</code> and <code>arc strength</code>."),
                 style = "font-size: medium; padding: 5px;")
        ),
        easyClose = TRUE
      )
    )
  })
  # ---------------------------------- Instruction
  observeEvent(input$show_UnDir_AlgoDescriptions, {
    showModal(
      modalDialog(
        title = tags$span(
          tags$i(class = "fas fa-code-branch", style = "color: white; padding-right: 10px;"),
          "Undirected Algorithm Descriptions",
          style = "font-size: smaller; color: white; background-color: #3c8dbc; padding: 10px;"
        ),
        size = "m",
        tagList(
          tags$p(HTML("* Max-Min Parents and Children - <code>MMPC</code>"), style = "font-size: medium; padding: 5px;"),
          tags$p(HTML("* Stable and Interpretable HITON Parents and Children - <code>SI.HITON.PC</code>"), style = "font-size: medium; padding: 5px;")
        ),
        easyClose = TRUE
      )
    )
  })
  # ----------------------------------
  observe({
    print(input$sidebarItemExpanded)
  })

  observe({
    print(plot_done())
  })
  #-------------------------------------------------------------------------------------------

  datapath <- "./"
  # reactive values
  data_uploaded <- reactiveVal(FALSE)
  rv <- reactiveValues()
  default_data_used <- reactiveVal(FALSE)  # to track if default data is used

  data <- reactiveVal(NULL)

  Black_List <- reactiveVal(NULL)
  White_List <- reactiveVal(NULL)

  #  files are uploaded or not?
  data_present <- reactiveVal(FALSE)
  Black_List_present <- reactiveVal(FALSE)
  # White_List_present <- reactiveVal(FALSE)

  possible_whitelist_reactiveVal <- reactiveVal(NULL)
  final_white_list <- reactiveVal(NULL)

  No_Cycle_plot <- reactiveVal(FALSE)


  # plot_done <- reactiveVal(TRUE)
  plot_done <- reactiveVal(FALSE)
  # finished_running <- reactiveVal(FALSE)
  # Status_var <- reactiveVal(NULL)

  plots_list = reactiveVal(NULL)
  fBRCABN = reactiveVal(NULL)
  final_DAG_detail_reactiveVal = reactiveVal(NULL)

  # Reactive values to store details for each single algorithm run DAG
  DAG_data_list <- reactiveValues()

  # reactive_plot_list <- reactiveVal(plot_list)


  # Reactive values to store precomputed plots and details
  precomputed_plots <- reactiveValues()
  precomputed_details <- reactiveValues()

  #-------------------------------------------------------------------------------------------
  # New observeEvent blocks:
  observeEvent(input$dataFile, {
    tryCatch({
      data.check <- input$dataFile
      if (!is.null(data.check$datapath) && file.exists(data.check$datapath)) {
        data(read.csv(data.check$datapath))
        print("Data uploaded")
      }
    }, error = function(e) {
      showNotification(paste("Error reading data File:", e$message), type = "error")
    })
  })
  #-------------------------------------------------------------------------------------------
  observeEvent(input$BlackListFile, {
    tryCatch({
      Black_List.check <- input$BlackListFile
      if (!is.null(Black_List.check$datapath) && file.exists(Black_List.check$datapath)) {
        Black_List(read.csv(Black_List.check$datapath))
        print("Black_List uploaded")
      }
    }, error = function(e) {
      showNotification(paste("Error reading BlackList File:", e$message), type = "error")
    })
  })
  #-------------------------------------------------------------------------------------------
  observeEvent(input$runButton, {
    print("Run Discovery clicked")

    # browser()
    # --------
    if((length(input$algorithm_directed) < 1) || is.null(input$algorithm_directed)) {
      # if((length(input$algorithm_directed) < 2) || is.null(input$algorithm_directed)) {
      showModal(modalDialog(
        tags$div(
          style = "font-size:18px; color:#34495E; padding: 10px 20px; background-color: #EAECEE; border-bottom-left-radius: 5px; border-bottom-right-radius: 5px;",
          tags$p(
            tags$span(
              style = "color: #B71C1C;",
              icon("exclamation-triangle", style = "margin-right: 6px; color: #B71C1C;"),
              tags$span(style = "font-weight: bold;", "Error: ")
            ),
            "Please select at least one directed algorithms to proceed."
            # "Please select at least two directed algorithms to proceed."

          )
        ),
        easyClose = TRUE,
        footer = tags$div(
          actionButton("close", "Acknowledge",
                       style = "background-color: #2980B9; color: white; border: none; padding: 10px 20px; border-radius: 5px; margin: 0px; cursor: pointer; box-shadow: 2px 2px 8px rgba(0,0,0,0.1);
                      &:hover {
                        background-color: #2471A3;
                      }"
          ),
          style = "text-align: center; background-color: #fdfefe; border-bottom-left-radius: 5px; border-bottom-right-radius: 5px; padding: 0px;"
        )
      ))
    }
    else {
      # -------------------
      data_present <- reactive({ !is.null(input$dataFile) })
      Black_List_present <- reactive({ !is.null(input$BlackListFile) })
      # White_List_present <- reactive({ !is.null(input$WhiteListFile) })

      # data is uploaded and set data_uploaded reactive value?

      #print("here 1 ")
      cat("---------------", "\n")
      print("Data")
      cat("---------------", "\n")
      print(data_present())
      cat("---------------", "\n")
      print(Black_List_present())
      cat("---------------", "\n")
      # print(White_List_present())

      # if (data_present()) {
      if (data_present() & Black_List_present()) {
        # if (data_present() & (Black_List_present() || White_List_present())) {
        # print("im here 2")
        data_uploaded(TRUE)
        cat("---------------", "\n")
        print(data_uploaded())
        cat("---------------", "\n")
      } else {
        data_uploaded(FALSE)
      }
      # ------------------- main functionality
      cat("---------------", "\n")
      print("uploaded data")
      cat("---------------", "\n")
      print(data_uploaded())
      cat("---------------", "\n")

      if (isTRUE(data_uploaded()) || isTRUE(default_data_used())) {

        showModal(modalDialog(
          tags$div(
            style = "font-size:18px; color:#34495E; padding: 10px 20px; background-color: #EAECEE; border-radius: 5px; text-align: center; box-shadow: 0 4px 8px rgba(0,0,0,0.1);  margin: auto;",
            tags$p(
              tags$span(
                style = "color: #2980b9;",
                icon("sync", style = "margin-right: 6px; color: #2980b9; animation: spin 2s linear infinite;"),
                tags$span(style = "font-weight: bold;", "Generating Plots: ")
              ),
              "Please wait, the initial plots and tables are getting generated..."
            )
          ),
          footer = NULL,  # No footer
          easyClose = FALSE,  # Prevent closing by clicking outside modal
          size = "s"  # Small size, but width is adjusted using custom styles
        ))


        cluster <- 6
        # Initial operations
        cl <- makeCluster(cluster, type = "SOCK")  # Create a cluster with specified nodes
        onStop(function() {
          stopCluster(cl)
        })

        set.seed(2023)  # Set seed for reproducibility
        rand <- clusterEvalQ(cl, runif(10))  # Generate random numbers using cluster

        # Data processing
        data_process_result <- data_process_Correlation(data())

        # -----------------
        output$viewDataStructure <- renderPrint({
          str(data_process_result)
          # Summary(data_process_result)
        })
        # -----------------
        if("discretized_data" %in% names(data_process_result)) {
          discretized_data <- as.data.frame(data_process_result$discretized_data)
        } else {
          stop("discretized_data not found in data_process_result")
        }
        # -------------------------------
        output$variable_distribution <- renderPlot({
          num_vars <- length(colnames(data()))
          num_cols <- if(num_vars > 20) 4 else 2
          num_rows <- ceiling(num_vars / num_cols)

          # Smaller margins may help in fitting plots within available space
          mar_vector <- c(3, 3, 2, 1)  # Reduce margin sizes

          par(mfrow = c(num_rows, num_cols), mar = mar_vector)

          for (var in colnames(data())) {
            x <- data()[, var]
            if(!all(is.na(x))) {
              hist(x, prob = TRUE, xlab = "", ylab = "", main = var, col = "steelblue")
              lines(density(x, na.rm = TRUE), col = "darkred", lwd = 2)
            }
          }
        })
        # -------------------------------
        output$correlation_structure <- renderPlot({
          rho <- as.matrix(data_process_result$corrcoef)

          # Properly check for NA or Inf values using any()
          if (any(is.na(rho) | is.infinite(rho))) {
            cat("Data contains NA or Inf values\n")
            return()  # Stop further execution if data is not clean
          }
          #library(RColorBrewer)
          color_palette <- colorRampPalette(brewer.pal(11, "RdBu"))(100)
          breaks <- seq(-1, 1, length.out = length(color_palette) + 1)

          # par(mar = c(1, 1, 1, 1))  # Adjust margins if necessary
          par(mar = c(2, 2, 2, 2))

          # Use heatmap.2 with correct parameters
          heatmap.2(rho, scale = "none", trace = "none", revC = TRUE, col = rev(color_palette),
                    breaks = breaks,
                    keysize = 1, symm = FALSE, dendrogram = "row", margins = c(10, 10), cexRow = 0.7, cexCol = 0.7)
        })
        # -------------------------------

        # -------------------------------------------------
        output$correlation_structure_pi_sig <- renderPlot({
          rho <- as.matrix(data_process_result$corrcoef)

          # Properly check for NA or Inf values using any()
          if (any(is.na(rho) | is.infinite(rho))) {
            cat("Data contains NA or Inf values\n")
            return()  # Stop further execution if data is not clean
          }

          library(corrplot)
          library(RColorBrewer)

          # Define a function to calculate p-values for correlation matrix
          cor.mtest <- function(mat, conf.level = 0.95) {
            mat <- as.matrix(mat)
            n <- ncol(mat)
            p.mat <- matrix(NA, n, n)
            diag(p.mat) <- 0
            for (i in 1:(n - 1)) {
              for (j in (i + 1):n) {
                tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
                p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
              }
            }
            colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
            p.mat
          }

          # Calculate p-values
          p_matrix <- cor.mtest(data_process_result$discretized_data)



          # Define color palette
          color_palette <- colorRampPalette(brewer.pal(11, "RdBu"))(200)

          # Set plot margins
          # par(mar = c(1, 1, 1, 1))  # Adjust margins if necessary
          par(mar = c(2, 2, 2, 2))

          # Hierarchical clustering
          hc <- hclust(as.dist(1 - rho))

          # Reorder correlation matrix based on hierarchical clustering
          rho_reordered <- rho[hc$order, hc$order]


          # Create correlation plot using corrplot
          corrplot(rho_reordered, method = "pie", type = "lower", order = "hclust",
                   hclust.method = "complete", addrect = 2, # Add clusters
                   tl.col = "black", tl.srt = 45, # Rotate text labels for better readability
                   col = color_palette, cl.lim = c(-1, 1), # Set color limits
                   addCoef.col = "black", number.cex = 0.7, # Add correlation coefficients
                   p.mat = p_matrix, sig.level = 0.05, insig = "blank", # Add significance levels
                   diag = TRUE) # Exclude diagonal elements
          # diag = TRUE, title = "Significant Correlation") # Exclude diagonal elements


        })
        # -------------------------------------------------
        output$correlation_structure_sig <- renderPlot({
          rho <- as.matrix(data_process_result$corrcoef)
          p.mat <- cor.mtest(rho)  # Generate p-values

          # Mask non-significant correlations
          alpha <- 0.05
          rho[p.mat > alpha] <- NA

          if (any(is.na(rho) | is.infinite(rho))) {
            cat("Data contains NA or Inf values\n")
            return()  # Stop further execution if data is not clean
          }

          color_palette <- colorRampPalette(brewer.pal(11, "RdBu"))(100)
          breaks <- seq(-1, 1, length.out = length(color_palette) + 1)

          par(mar = c(2, 2, 2, 2))  # Adjust margins if necessary

          heatmap.2(rho, scale = "none", trace = "none", revC = TRUE, col = rev(color_palette),
                    breaks = breaks,
                    keysize = 1, symm = TRUE, dendrogram = "row", margins = c(10, 10), cexRow = 0.7, cexCol = 0.7,
                    na.rm = TRUE)  # Ensure NA values are handled
        })
        # -------------------------------
        corrcoef<- as.data.frame(data_process_result$corrcoef)
        data<- as.data.frame(data_process_result$data) # newly added
        # -------------------------------
        output$Black_List <- renderStyledTable(Black_List(), rownames = TRUE, download_version = c('csv', 'excel'))
        # -------------------------------
        # Initial edge list
        Blank_edge_list <- data.frame(from = character(), to = character(), Edge_No= integer())
        Edge_count <- 1
        # -------------------------------------------------------------
        run_algorithm_directed_with_messages <- function(algorithm_directed, Blank_edge_list, Edge_count, discretized_data, nboot, cl, Black_List, White_List, corrcoef) {
          # Start capturing output
          output_messages <- capture.output(
            result_directed <- run_algorithm_directed(input$algorithm_directed, Blank_edge_list, Edge_count, discretized_data, input$nboot, cl, Black_List(), White_List(), corrcoef)
          )
          # Return both result and messages
          return(list(result_directed = result_directed, messages = output_messages))
        }
        # ----
        run_algorithm_Undirected_with_messages <- function(algorithm_undirected= NULL, updated_edge_list_directed, updated_edge_count_directed, discretized_data, nboot, cl, corrcoef) {
          # Start capturing output
          output_messages <- capture.output(
            all_arc_list <- run_algorithm_Undirected(input$algorithm_undirected, updated_edge_list_directed, updated_edge_count_directed, discretized_data, input$nboot, cl, corrcoef)
          )
          # Return both result and messages
          return(list(all_arc_list = all_arc_list, messages = output_messages))
        }

        if (!is.null(input$algorithm_directed) & !is.null(input$algorithm_undirected)) {


          output_algorithm_directed <- run_algorithm_directed_with_messages(input$algorithm_directed, Blank_edge_list, Edge_count, discretized_data, input$nboot, cl, Black_List(), White_List(), corrcoef)

          # Extract results
          Arcs.Cor_streng_table.alg <- output_algorithm_directed$result_directed$Arcs.Cor_streng_table.alg
          updated_edge_list_directed <- output_algorithm_directed$result_directed$edge_list
          updated_edge_count_directed <- output_algorithm_directed$result_directed$edge_count
          names(updated_edge_list_directed) <- c("from", "to", "Edge_No")

          # -----------

          output_algorithm_Undirected <- run_algorithm_Undirected_with_messages(input$algorithm_undirected, updated_edge_list_directed, updated_edge_count_directed, discretized_data, input$nboot, cl, corrcoef)


          output$directed_Undirected_algorithm_messages <- renderPrint({
            # "Algorithm Progression Messages Here."
            messages_combined <- c(output_algorithm_directed$messages,
                                   "\n",
                                   output_algorithm_Undirected$messages)
            cat(messages_combined, sep = "\n")
          })

          Arcs.Cor_table.alg <- output_algorithm_Undirected$all_arc_list$Arcs.Cor_table.alg
          all_edge_list <- output_algorithm_Undirected$all_arc_list$all_edge_list

        } else if(!is.null(input$algorithm_directed)) {


          output_algorithm_directed <- run_algorithm_directed_with_messages(input$algorithm_directed, Blank_edge_list, Edge_count, discretized_data, input$nboot, cl, Black_List(), White_List(), corrcoef)

          # Extract results
          Arcs.Cor_streng_table.alg <- output_algorithm_directed$result_directed$Arcs.Cor_streng_table.alg
          updated_edge_list_directed <- output_algorithm_directed$result_directed$edge_list
          updated_edge_count_directed <- output_algorithm_directed$result_directed$edge_count
          names(updated_edge_list_directed) <- c("from", "to", "Edge_No")

          # -----------
          output$directed_Undirected_algorithm_messages <- renderPrint({
            messages_combined <- output_algorithm_directed$messages
            cat(messages_combined, sep = "\n")
          })

          Arcs.Cor_table.alg <- Arcs.Cor_streng_table.alg
          all_edge_list <- updated_edge_list_directed
        } else {
          return()
        }
        # ------------------------------  updated version
        observeEvent(input$run, {
          req(input$alg_directed)
          cat("------------------------------", "\n")
          print("alg_directed:")
          cat("------------------------------", "\n")

          # run requested algorithm (or recover pre-generated results if already available)
          preData <- DAG_data_list[[input$alg_directed]]
          if (!is.null(preData)) {
            # recover pre-stored data
            run_single_algorithm_directed_result <- preData

          } else {
            # generate new data
            run_single_algorithm_directed_result <- run_single_algorithm_directed(input$alg_directed,
                                                                                  discretized_data,
                                                                                  input$nboot, cl, corrcoef, Black_List())
            # store new data
            DAG_data_list[[input$alg_directed]] <- run_single_algorithm_directed_result

          }

          # render data table
          # output$DAG_detail <- renderDataTable(run_single_algorithm_directed_result$DAG_detail)
          output$DAG_detail <- renderStyledTable(run_single_algorithm_directed_result$DAG_detail, rownames = TRUE, download_version = c('csv', 'excel'))

          # prepare and render plot
          output$plot_output <- renderPlot({
            ave.dag <- run_single_algorithm_directed_result$ave.dag
            ars <- run_single_algorithm_directed_result$ars
            ars <- as.data.frame(ars)
            CorSign <- calculate_cor_sign(ars, corrcoef)
            HLarcs <- ars[CorSign == "-",]
            arc_str <- arc.strength(ave.dag, discretized_data)
            plot_title <- paste("DAG Network (Algorithm", input$alg_directed, ")")
            strength.plot(ave.dag, arc_str, shape = "ellipse", highlight = list(arcs = HLarcs), main = plot_title)
          })
        })

        # ------------------------------ updated version
        # Precompute at start or via an isolated trigger
        observe({
          for (alg in c("iamb", "iamb.fdr", "pc.stable", "tabu", "mmhc", "gs", "rsmax2", "hc")) {
            # generate new data
            run_single_algorithm_directed_result <- run_single_algorithm_directed(alg, discretized_data,
                                                                                  input$nboot, cl, corrcoef, Black_List())
            # store new data
            DAG_data_list[[alg]] <- run_single_algorithm_directed_result
          }
        })


        # observeEvent(req(DAG_data_list), {
        observeEvent(req(DAG_data_list, final_DAG_detail_reactiveVal()), {

          cat("------------------------------", "\n")
          print("Starting Ensemble 'DAG_data_list':")
          cat("------------------------------", "\n")

          # # -------------------
          # # Function to safely extract DAG details from the list
          # safe_extract <- function(list, key) {
          #   if (!is.null(list[[key]])) {
          #     return(list[[key]]$DAG_detail)
          #   } else {
          #     warning(paste("No data found for key:", key))
          #     return(data.frame(from = character(), to = character(), Arc_strength = numeric()))
          #   }
          # }
          #
          # iamb_arcs <- safe_extract(DAG_data_list, "iamb")
          # iamb_fdr_arcs <- safe_extract(DAG_data_list, "iamb.fdr")
          # pc_stable_arcs <- safe_extract(DAG_data_list, "pc.stable")
          # tabu_arcs <- safe_extract(DAG_data_list, "tabu")
          # mmhc_arcs <- safe_extract(DAG_data_list, "mmhc")
          # gs_arcs <- safe_extract(DAG_data_list, "gs")
          # rsmax2_arcs <- safe_extract(DAG_data_list, "rsmax2")
          # hc_arcs <- safe_extract(DAG_data_list, "hc")
          #
          #
          # # Debugging outputs
          # cat("iamb_arcs:\n")
          # print(head(iamb_arcs))
          # cat("iamb_fdr_arcs:\n")
          # print(head(iamb_fdr_arcs))
          # cat("pc_stable_arcs:\n")
          # print(head(pc_stable_arcs))
          # cat("tabu_arcs:\n")
          # print(head(tabu_arcs))
          # cat("mmhc_arcs:\n")
          # print(head(mmhc_arcs))
          # cat("gs_arcs:\n")
          # print(head(gs_arcs))
          # cat("rsmax2_arcs:\n")
          # print(head(rsmax2_arcs))
          # cat("hc_arcs:\n")
          # print(head(hc_arcs))
          # # -------------------

          iamb_arcs <- DAG_data_list[["iamb"]]$DAG_detail
          iamb_fdr_arcs <- DAG_data_list[["iamb.fdr"]]$DAG_detail
          pc_stable_arcs <- DAG_data_list[["pc.stable"]]$DAG_detail
          tabu_arcs <- DAG_data_list[["tabu"]]$DAG_detail
          mmhc_arcs <- DAG_data_list[["mmhc"]]$DAG_detail
          gs_arcs <- DAG_data_list[["gs"]]$DAG_detail
          rsmax2_arcs <- DAG_data_list[["rsmax2"]]$DAG_detail
          hc_arcs <- DAG_data_list[["hc"]]$DAG_detail

          # -------------------
          # Assume ensemble_dag is updated elsewhere appropriately
          ensemble_dag <- final_DAG_detail_reactiveVal()
          # cat("ensemble_dag:\n")
          # print(head(ensemble_dag))



          # List of algorithm results
          algorithms <- list(
            IAMB = iamb_arcs,
            IAMB.FDR = iamb_fdr_arcs,
            PC.STABLE = pc_stable_arcs,
            TABU = tabu_arcs,
            MMHC = mmhc_arcs,
            GS = gs_arcs,
            RSMAX2 = rsmax2_arcs,
            HC = hc_arcs
          )

          compare_arcs <- function(individual, ensemble) {

            individual$from <- as.character(individual$from)
            individual$to <- as.character(individual$to)
            ensemble$from <- as.character(ensemble$from)
            ensemble$to <- as.character(ensemble$to)


            individual_set <- paste(individual$from, individual$to, sep = " -> ")
            ensemble_set <- paste(ensemble$from, ensemble$to, sep = " -> ")

            common_arcs <- intersect(individual_set, ensemble_set)
            unique_arcs_individual <- setdiff(individual_set, common_arcs)
            # unique_arcs_individual <- setdiff(individual_set, ensemble_set)
            unique_arcs_ensemble <- setdiff(ensemble_set, common_arcs)
            # unique_arcs_ensemble <- setdiff(ensemble_set, individual_set)

            common_arcs_strength <- data.frame(from = character(), to = character(),
                                               individual_strength = numeric(), ensemble_strength = numeric())
            for (arc in common_arcs) {
              from_to <- strsplit(arc, " -> ")[[1]]
              individual_strength <- individual[individual$from == from_to[1] & individual$to == from_to[2], "Arc_strength"]
              ensemble_strength <- ensemble[ensemble$from == from_to[1] & ensemble$to == from_to[2], "Arc_strength"]
              common_arcs_strength <- rbind(common_arcs_strength,
                                            data.frame(from = from_to[1], to = from_to[2],
                                                       individual_strength = individual_strength,
                                                       ensemble_strength = ensemble_strength))
            }

            list(common_arcs = length(common_arcs), unique_arcs_individual = length(unique_arcs_individual), unique_arcs_ensemble = length(unique_arcs_ensemble), common_arcs_strength = common_arcs_strength)
          }

          # Compare each algorithm with ensemble method
          results <- lapply(algorithms, compare_arcs, ensemble = ensemble_dag)
          names(results) <- names(algorithms)

          comparison_summary <- data.frame(
            Algorithm = names(results),
            Common_Arcs = sapply(results, function(x) x$common_arcs),
            Unique_Arcs_Individual = sapply(results, function(x) x$unique_arcs_individual),
            Unique_Arcs_Ensemble = sapply(results, function(x) x$unique_arcs_ensemble)
          )

          # Add a dummy variable: Total Arcs
          comparison_summary <- comparison_summary %>%
            mutate(Total_Arcs = Common_Arcs + Unique_Arcs_Individual + Unique_Arcs_Ensemble)

          # Normalize data for radar plot
          comparison_summary_normalized <- comparison_summary %>%
            mutate(Common_Arcs = Common_Arcs / max(Common_Arcs),
                   Unique_Arcs_Individual = Unique_Arcs_Individual / max(Unique_Arcs_Individual),
                   Unique_Arcs_Ensemble = Unique_Arcs_Ensemble / max(Unique_Arcs_Ensemble),
                   Total_Arcs = Total_Arcs / max(Total_Arcs))

          # Prepare data for strength comparison
          common_arcs_strength <- do.call(rbind, lapply(names(results), function(alg) {
            data <- results[[alg]]$common_arcs_strength
            data$Algorithm <- alg
            data
          }))

          #----------
          # Ensure strength columns are numeric
          common_arcs_strength <- common_arcs_strength %>%
            mutate(
              individual_strength = as.numeric(individual_strength),
              ensemble_strength = as.numeric(ensemble_strength)
            )

          # Handle NA values that might result from conversion
          common_arcs_strength <- common_arcs_strength %>%
            filter(!is.na(individual_strength) & !is.na(ensemble_strength))

          # Categorize points
          common_arcs_strength <- common_arcs_strength %>%
            mutate(
              category = case_when(
                individual_strength < ensemble_strength ~ "Above",
                individual_strength > ensemble_strength ~ "Below",
                TRUE ~ "On"
              )
            )

          #---------------------
          # Convert data to long format for plotting
          comparison_summary_long <- comparison_summary %>%
            pivot_longer(cols = c(Common_Arcs, Unique_Arcs_Individual, Unique_Arcs_Ensemble), names_to = "Arc_Type", values_to = "Count")

          comparison_summary_long$Arc_Type <- factor(comparison_summary_long$Arc_Type, levels = c("Common_Arcs", "Unique_Arcs_Individual", "Unique_Arcs_Ensemble"))


          # # ---------
          # Bar plot for common and unique arcs
          output$bar_plot <- renderPlot({
            ggplot(comparison_summary_long, aes(x = Algorithm, y = Count, fill = Arc_Type)) +
              geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black", size = 0.3) +
              geom_text(aes(label = Count), vjust = -0.3, position = position_dodge(0.8), size = 3.5) +
              scale_fill_manual(name = "Arc Type",
                                values = c("Common_Arcs" = "#b2df8a", "Unique_Arcs_Individual" = "#1f78b4", "Unique_Arcs_Ensemble" = "#ff7f00"),
                                labels = c("Common Arcs with Ensemble", "Unique Arcs (Individual)", "Unique Arcs (Ensemble)")) +
              labs(title = "Comparison of Common and Unique Arcs between Individual Algorithms and Ensemble Method",
                   x = "Algorithms", y = "Number of Arcs") +
              theme_minimal(base_size = 15) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1),
                    panel.grid.major = element_blank(),  # Remove major grid lines# panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = 'grey80'),
                    panel.grid.minor = element_blank(),  # Remove minor grid lines# panel.grid.minor = element_line(size = 0.25, linetype = 'solid', color = 'grey90'),
                    plot.title = element_text(hjust = 0.5, size = 16),
                    axis.title = element_text(size = 12))
          })

          # # -----------------------------------------------

          # Customize colors using viridis
          # colors_all_arcs <- viridis::viridis(length(unique(radar_data_all_arcs$group)))
          colors_all_arcs <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#b2df8a")
          pie_colors <- c("Arcs with Higher Strength" = "#ff7f00", "Arcs with Lower Strength" = "#a6cee3", "Arcs with Same Strength" = "#b2df8a")


          output$scatter_plot <- renderPlot({
            # Calculate number of points above, below, and on line
            above_line <- sum(common_arcs_strength$ensemble_strength > common_arcs_strength$individual_strength)
            below_line <- sum(common_arcs_strength$ensemble_strength < common_arcs_strength$individual_strength)
            on_line <- sum(common_arcs_strength$ensemble_strength == common_arcs_strength$individual_strength)

            # Calculate percentages
            total_points <- nrow(common_arcs_strength)
            percent_above <- (above_line / total_points) * 100
            percent_below <- (below_line / total_points) * 100
            percent_on <- (on_line / total_points) * 100

            # Prepare data for pie chart
            pie_data <- data.frame(
              category = c("Arcs with Higher Strength", "Arcs with Lower Strength", "Arcs with Same Strength"),
              count = c(percent_above, percent_below, percent_on),
              label = c(paste0(above_line, " (", round(percent_above, 2), "%)"),
                        paste0(below_line, " (", round(percent_below, 2), "%)"),
                        paste0(on_line, " (", round(percent_on, 2), "%)"))
            )

            # Create pie chart with annotations
            pie_chart <- ggplot(pie_data, aes(x = 2, y = count, fill = category)) +
              geom_bar(width = 1, stat = "identity", color = "white") +
              coord_polar(theta = "y") +
              scale_fill_manual(values = pie_colors) +
              xlim(0.5, 2.5) +  # Adjust x-axis limits to create donut hole
              theme_void() +
              # labs(title = "Percentage Distribution") +
              theme(plot.title = element_text(hjust = 0.5, size = 16)) +
              geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5)

            # Scatter plot for strength comparison with log scales and adjusted limits
            scatter_plot <- ggplot(common_arcs_strength, aes(x = individual_strength, y = ensemble_strength, color = Algorithm)) +
              geom_point(size = 3, alpha = 0.6) +
              geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
              scale_color_manual(values = colors_all_arcs) +
              scale_x_log10(limits = c(0.001, 1)) +  # Adjust limits to avoid cut-off points
              scale_y_log10(limits = c(0.001, 1)) +  # Adjust limits to avoid cut-off points
              labs(title = "Strength Comparison of Common Arcs between Individual Algorithms and Ensemble Method",
                   x = "Individual Algorithm Strength", y = "Ensemble Algorithm Strength") +
              theme_minimal(base_size = 15) +
              theme(
                axis.line = element_line(size = 0.5, color = "black"),  # Add axis lines
                plot.title = element_text(hjust = 0.5, size = 16),
                axis.title = element_text(size = 12),
                legend.position = "right",
                legend.box = "vertical",
                legend.spacing.y = unit(1, "cm"))

            # Combine scatter plot and pie chart using cowplot
            combined_plot <- plot_grid(scatter_plot, pie_chart, ncol = 2, rel_widths = c(3, 2))

            # Display combined plot
            print(combined_plot)

          })
          #-----------------
        })

        # -------------------------------------------------------------
        output$all_edge_list <- renderStyledTable(all_edge_list, rownames = FALSE, download_version = c('csv', 'excel'))

        augmented_edge_list<- augmented_edge_table(Black_List(),
                                                   all_edge_list,
                                                   input$algorithm_directed,
                                                   Arcs.Cor_streng_table.alg,
                                                   input$algorithm_undirected,
                                                   Arcs.Cor_table.alg)
        # ---------------------
        # For each column want to round, if it's numeric. round it
        cols_to_round <- grep("\\.strength|_strength", names(augmented_edge_list))

        for(col in cols_to_round){
          augmented_edge_list[[col]] <- signif(as.numeric(augmented_edge_list[[col]]), 2)
        }
        # ---------------------
        output$augmented_edge_list <- renderStyledTable(augmented_edge_list, rownames = TRUE, download_version = c('csv', 'excel'))

        possible_seed_arcs_filter<- uninformative_arcs_removal(augmented_edge_list, input$algorithm_undirected)
        # ---------------------
        cols_to_round <- grep("\\.strength|_strength", names(possible_seed_arcs_filter))

        for(col in cols_to_round){
          possible_seed_arcs_filter[[col]] <- signif(as.numeric(possible_seed_arcs_filter[[col]]), 2)
        }
        # ---------------------
        output$possible_seed_arcs_filter <- renderStyledTable(possible_seed_arcs_filter, rownames = TRUE, download_version = c('csv', 'excel'))

        threshold <- finding_threshold_values(possible_seed_arcs_filter, input$threshold_level)
        # ------------------
        # threshold <- as.numeric(sprintf("%.3f", threshold))
        temp.white_thresh.cols <- temp.white_thresh.cols(possible_seed_arcs_filter, threshold)

        temp_list_merge<- temp.white_thresh.cols$temp_list_merge
        augmneted.thresh.cols <- temp.white_thresh.cols$augmneted.thresh.cols

        output$augmneted.thresh.cols <- renderStyledTable(augmneted.thresh.cols, rownames = TRUE, download_version = c('csv', 'excel'))

        num.white_thresh <- temp.white_thresh.cols$num.white_thresh
        (num.white_thresh <- as.data.frame(num.white_thresh))


        # ------------------
        data_val <- data()  # Get current data
        data_val[] <- lapply(data_val, function(col) {
          # Attempt to convert each column to numeric
          numeric_col <- suppressWarnings(as.numeric(col))
          if (!any(is.na(numeric_col))) {
            return(numeric_col)  # Return converted column if no NA produced
          } else {
            return(col)  # Return original column if conversion fails
          }
        })
        data(data_val)
        # ------------------

        calculate_loss_npar_table <- calculate_loss_npar_table (threshold,
                                                                temp_list_merge,
                                                                discretized_data,
                                                                data(),
                                                                input$nboot, cl, Black_List())

        npar_merged  <- calculate_loss_npar_table$npar_merged
        L1_merged  <- calculate_loss_npar_table$L1_merged
        # ------------------
        # For each column want to round, if it's numeric. If it is, round it.
        for (col in names(L1_merged)) {
          if (is.numeric(L1_merged[[col]])) {
            L1_merged[[col]] <- as.numeric(sprintf("%.2f", L1_merged[[col]]))
          }
        }
        # ------------------
        parents_list_merged  <- calculate_loss_npar_table$parents_list_merged

        Total.BIC.thresh  <- calculate_loss_npar_table$Total.BIC.thresh
        Total.BIC.thresh <- as.data.frame(Total.BIC.thresh)

        output$L1_merged <- renderStyledTable(L1_merged, rownames = TRUE, download_version = c('csv', 'excel'))
        output$npar_merged <- renderStyledTable(npar_merged, rownames = TRUE, download_version = c('csv', 'excel'))

        num_arcs.All.thresh <- calculate_loss_npar_table$num_arcs.All.thresh
        num_arcs.All.thresh <- as.data.frame(num_arcs.All.thresh)

        BIC_merged_table <- calculate_bic (discretized_data, npar_merged, L1_merged, threshold)

        # For each column want to round, if it's numeric? round it.
        for (col in names(BIC_merged_table)) {
          if (is.numeric(BIC_merged_table[[col]])) {
            BIC_merged_table[[col]] <- as.numeric(sprintf("%.2f", BIC_merged_table[[col]]))
          }
        }

        output$BIC_merged_table <- renderStyledTable(BIC_merged_table, rownames = TRUE, download_version = c('csv', 'excel'))
        # browser()

        min_bic.Parent_whitelist_acyclic <- find_min_bic.Parent_whitelist_acyclic(BIC_merged_table, npar_merged, parents_list_merged)

        bic_min_table <- min_bic.Parent_whitelist_acyclic$bic_min_table
        possible.white.list <- min_bic.Parent_whitelist_acyclic$possible.white.list
        # final_white_list <- min_bic.Parent_whitelist_acyclic$final_white_list


        possible.white.list$from <- as.character(possible.white.list$from)
        possible.white.list$to <- as.character(possible.white.list$to)

        possible_whitelist_reactiveVal(possible.white.list)
        print("possible whitelist:")
        print(possible.white.list) # printed data frame with 2 columns


        print("possible whitelist reactiveVal:")
        print(possible_whitelist_reactiveVal())# we printed data frame with 2 columns
        print("Number of Columns of 'possible whitelist reactiveVal':")
        print(length(colnames(possible_whitelist_reactiveVal()))) # we have 2 columns
        # --------------------------------
        output$bic_min_table <- renderStyledTable(bic_min_table, rownames = TRUE, download_version = c('csv', 'excel'))
        output$possible.white.list <- renderStyledTable(possible_whitelist_reactiveVal(), rownames = TRUE, download_version = c('csv', 'excel'))
        #---------------------------
        g <- reactiveVal({
          if (ncol(possible_whitelist_reactiveVal()) < 2) {
            (graph.empty())
          } else {
            (graph_from_data_frame(possible_whitelist_reactiveVal(), directed = TRUE))
          }
        })
        # -----------------------
        # Convert igraph object to a visNetwork suitable format
        vis_graph <- reactive({
          nodes_data <- data.frame(id = V(g())$name, label = V(g())$name)
          edges_data <- get.data.frame(g(), what = "edges")
          list(nodes = nodes_data, edges = edges_data)
        })

        output$initialPlot <- renderVisNetwork({
          visNetwork(nodes = vis_graph()$nodes, edges = vis_graph()$edges) %>%
            visNodes(shape = "circle",
                     font = list(size = 12,
                                 vadjust = 0,
                                 bold = TRUE,
                                 color = "black")) %>%
            visEdges(arrows = "to",
                     smooth = T,
                     font = list(size = 15, color = "black", background = 'rgba(255, 255, 255, 0.7)')) %>%
            visOptions(highlightNearest = list(enabled = F, hover = F),
                       nodesIdSelection = F)%>%
            visLayout(randomSeed = 123,
                      improvedLayout = TRUE)  %>%
            visPhysics(solver = "forceAtlas2Based",  # physics solver
                       forceAtlas2Based = list(gravitationalConstant = -30,  #
                                               centralGravity = 0.005,  #
                                               springLength = 100,  #
                                               springConstant = 0.18))  #
        })
        # # -----------------------
        observeEvent(plot_done(), {
          print("Plot done event triggered")
          if (plot_done() == TRUE) {
            removeModal()  # Ensure "Resolving Cycles" modal is removed
            content <- paste0(
              "<div style='font-size:18px; color:#34495E; padding: 3px 5px; background-color: #EAECEE; border-bottom-left-radius: 5px; border-bottom-right-radius: 5px;'>",
              "<p>",
              "<span style='color: #FF0000;'>",
              # "<i class='fa fa-exclamation-triangle' style='margin-right: 6px; color: #FF0000;'></i>",
              "<span style='font-weight: bold;'>Proceed to Further Analysis </span>",
              "</span>",
              "<br><br>",
              "<span style='font-weight: bold;'> Navigate to the tab 'Whitelisted Arcs & Acyclicity' <a href='#' id='goToTab'>Click here</a>",
              "</p>",
              "</div>"
            )
            shinyalert::shinyalert(
              # title = "Proceed to Further Analysis",
              text = content,
              html = TRUE,
              showConfirmButton = FALSE,
              # confirmButtonColor = "#3498db" # This is shade of blue.
              # showConfirmButton = FALSE
            )
          }
        })
        # --------------------------------------2
        FindCycles = function(g) {
          Cycles = NULL
          for(v1 in V(g)) {
            if(igraph::degree(g, v1, mode="in") == 0) { next }
            # if(degree(g, v1, mode="in") == 0) { next }
            GoodNeighbors = neighbors(g, v1, mode="out")
            GoodNeighbors = GoodNeighbors[GoodNeighbors > v1]
            for(v2 in GoodNeighbors) {
              TempCyc = lapply(all_simple_paths(g, v2,v1, mode="out"), function(p) c(v1,p))
              TempCyc = TempCyc[which(sapply(TempCyc, length) > 2)] # check A->B and B->A as cycle: cycle length of two
              TempCyc = TempCyc[sapply(TempCyc, min) == sapply(TempCyc, `[`, 1)]
              Cycles  = c(Cycles, TempCyc)
            }
          }
          Cycles
        }
        # -------------------------------------- FIRST SECTION
        observe({
          print("Starting cycle detection")
          cycles <- FindCycles(g())
          if (length(cycles) == 0) {
            print("No cycles found, updating final white list")
            No_Cycle_plot(TRUE)
            final_white_list(possible_whitelist_reactiveVal())
            output$final_white_list <- renderStyledTable(final_white_list(), rownames = TRUE, download_version = c('csv', 'excel'))

            # Convert igraph object to visNetwork suitable format
            vis_graph <- reactive({
              nodes_data <- data.frame(id = V(g())$name, label = V(g())$name)
              edges_data <- get.data.frame(g(), what = "edges")
              list(nodes = nodes_data, edges = edges_data)
            })

            output$plot <- renderVisNetwork({
              print("Rendering network plot")
              visNetwork(nodes = vis_graph()$nodes, edges = vis_graph()$edges) %>%
                visNodes(shape = "circle", color = list(background = "#32a89e")) %>%
                visEdges(arrows = "to",
                         smooth = TRUE,
                         font = list(size = 15, color = "black", background = '#32a89e')) %>%
                visOptions(highlightNearest = list(enabled = F, hover = F),
                           nodesIdSelection = F)%>%
                visLayout(randomSeed = 123,
                          improvedLayout = TRUE)  %>%
                visPhysics(solver = "forceAtlas2Based",  # physics solver
                           forceAtlas2Based = list(gravitationalConstant = -50,  #
                                                   centralGravity = 0.005,  #
                                                   springLength = 100,  #
                                                   springConstant = 0.18))  #
            })

            # --------------------------------------
            # categorical_cols <- sapply(data(), function(x) { all(x %in% as.integer(x)) })
            observe({
              req(fileInputState()) # Ensure file is selected

              categorical_cols <- sapply(data(), function(x) { all(x %in% as.integer(x)) })
              categorical_col_names <- names(categorical_cols[categorical_cols])
              print(categorical_col_names)
              print(length(categorical_col_names))

              if (length(categorical_col_names) == 0) {
                # No categorical columns found, update var
                has.Categorical.Columns(FALSE)
              } else if (length(categorical_col_names) == 1) {

                has.Categorical.Columns(TRUE)
                # If there's exactly one categorical column, use it as Status
                updateSelectInput(session, "userSelected_Status", choices = categorical_col_names, selected = categorical_col_names[1])
              } else {
                has.Categorical.Columns(TRUE)
                # If there are multiple categorical columns, let user choose
                current_status <- input$userSelected_Status
                if (!is.null(current_status) && current_status %in% categorical_col_names) {
                  # Current selection is valid, do nothing
                  print("Current selection is already valid.")
                } else {
                  # Update because current selection is not valid or not set
                  updateSelectInput(session, "userSelected_Status", choices = categorical_col_names)
                }
              }
            })


            observeEvent(input$userSelected_Status, {
              # Fetch all column names except for selected status
              valid_features <- setdiff(names(data()), input$userSelected_Status)

              # Determine valid choices for 'Choose Key Feature:' dropdown
              current_key_feature <- input$userSelected_key_feature

              # If current key feature is not in list of valid features, update it to first valid feature
              if (!current_key_feature %in% valid_features && length(valid_features) > 0) {
                current_key_feature <- valid_features[1]
              }

              # Update 'Choose Key Feature:' dropdown with valid features and currently selected key feature
              updateSelectInput(session, "userSelected_key_feature", choices = valid_features, selected = current_key_feature)

              # Now update 'Select a secondary feature:' dropdown based on newly selected key feature
              valid_secondary_features <- setdiff(names(data()), c(input$userSelected_Status, current_key_feature))

              # Preserve current secondary selection if it's still valid; otherwise, update to first valid choice
              current_secondary_selection <- input$selectedCellType
              if (!current_secondary_selection %in% valid_secondary_features && length(valid_secondary_features) > 0) {
                current_secondary_selection <- valid_secondary_features[1]
              }
              updateSelectInput(session, "selectedCellType", choices = valid_secondary_features, selected = current_secondary_selection)
            })


            observeEvent(input$userSelected_key_feature, {
              print("Key feature changed to: ")
              print(input$userSelected_key_feature)

              # Determine valid choices for 'Select secondary feature:' dropdown
              valid_secondary_features <- setdiff(names(data()), c(input$userSelected_Status, input$userSelected_key_feature))
              print("Valid secondary features: ")
              print(valid_secondary_features)

              # if current selection of 'Select secondary feature:' is still valid
              current_secondary_selection <- input$selectedCellType
              print("Current secondary selection: ")
              print(current_secondary_selection)

              if(!current_secondary_selection %in% valid_secondary_features) {
                # If not valid, update to first valid choice
                current_secondary_selection <- valid_secondary_features[1]
                updateSelectInput(session, "selectedCellType", choices = valid_secondary_features, selected = current_secondary_selection)
                print("Secondary selection updated to: ")
                print(current_secondary_selection)
              } else {
                # If still valid, ensure UI is updated to reflect current state without changing selection
                updateSelectInput(session, "selectedCellType", choices = valid_secondary_features, selected = current_secondary_selection)
                print("Secondary selection remains unchanged.")
              }
            })

            print("WHITE LISTE ")
            print(final_white_list())

            # -----------------------------------
            observe({
              # Ensure possible.white.list is not NULL
              # req(selectedStatus(), selectedKeyFeature(), selectedSecondaryFeature())
              req(final_white_list(), !contour_plot_initial() )

              Final.DAG_network_plot <- Final.DAG_network_plot (augmented_edge_list,
                                                                possible_seed_arcs_filter,
                                                                data(), discretized_data,
                                                                final_white_list(),
                                                                Black_List(),
                                                                input$nboot, cl,
                                                                corrcoef)
              plot_done(TRUE)  # updated

              print(class(Final.DAG_network_plot))
              print(names(Final.DAG_network_plot))

              Alg.Count_arcs.strength.table <- Final.DAG_network_plot$Alg.Count_arcs.strength.table


              fBRCABN(Final.DAG_network_plot$fBRCABN)

              Contour_plot_userSelected_feature <- Contour_plot_userSelected_feature (data(), discretized_data,
                                                                                      fBRCABN(),
                                                                                      input$userSelected_Status,
                                                                                      input$userSelected_key_feature)

              plots_list(Contour_plot_userSelected_feature)
              # -----------------
              # -----------------

              output$DAG.Plot <- renderPlot({
                req(Final.DAG_network_plot)  # Ensures that Final.DAG_network_plot is loaded and not NULL
                plot_function <- Final.DAG_network_plot[["plotFunction"]]
                if (is.function(plot_function)) {
                  plot_function()  # Executes plotting function
                } else {
                  print("plot_function is not a function.")  # Debugging line to check if plot_function is correct
                }
              })
              # -----------------
              arcs <- Final.DAG_network_plot$arcs.BRCA
              P_strength <- Final.DAG_network_plot$P_strength

              arc_slopes.strength <- Final.DAG_network_plot$arc_slopes.strength

              # -----------------
              output$Diagnostic_plot <- renderPlot({
                # Assuming Diagnostic_plot() returns a ggplot object
                p <- Diagnostic_plot(num.white_thresh, num_arcs.All.thresh, Total.BIC.thresh, threshold)
                p
              })

              output$downloadPlot_Diagnostic <- downloadHandler(
                filename = function() {
                  paste("Diagnostic-plot-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".png", sep="")
                },
                content = function(file) {
                  # Generate plot object
                  p <- Diagnostic_plot(num.white_thresh, num_arcs.All.thresh, Total.BIC.thresh, threshold)

                  ggsave(file, plot = p, device = "png", width = 10, height = 8, dpi = 600)

                }
              )
              # --------------
              output$Plot.Algorithm.Count_arcs.strength <- renderPlot({
                p <- plot_Algorithm.Count_arcs.strength(Alg.Count_arcs.strength.table)
                p
              })

              output$downloadPlot_Algorithm.Count <- downloadHandler(
                filename = function() {
                  paste("Algorithm.Count_arcs.strength-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".png", sep="")
                },
                content = function(file) {
                  # Generate plot object
                  p <- plot_Algorithm.Count_arcs.strength(Alg.Count_arcs.strength.table)
                  #
                  ggsave(file, plot = p, device = "png", width = 10, height = 8, dpi = 600)
                }
              )
              # --------------
              output$DAGNetworkPlot <- renderVisNetwork({
                Final.DAG_network_plot$network
              })
              # --------------
              output$DAGNetworkPlot_intercept <- renderVisNetwork({
                Final.DAG_network_plot$network_intercept
              })
              # --------------
              final_DAG_detail <- Final.DAG_network_plot$final_DAG_detail
              final_DAG_detail_reactiveVal(final_DAG_detail)

              output$arc_slopes_strength <- renderStyledTable(final_DAG_detail, rownames = TRUE, download_version = c('csv', 'excel'))
              # --------------
            }
            )} else{
              print("Cycles detected, resolving cycles")
              plot_done(TRUE)
              cycles_resolved(FALSE)
            }
        })
        # --------------------------------------
        # Dynamic content based on No_Cycle_plot
        output$dynamicContent <- renderUI({
          if (No_Cycle_plot()) {
            tags$div(
              style = "position: relative;",
              # style = "width: 1400px; height: 700px; overflow: auto;"
              tags$div(visNetworkOutput("initialPlot"), style = "padding-top: 50px;"),
              tags$div(
                "Final Whitelist (No Cycle found)",
                br(),
                style = "position: absolute; top: 10px; left: 50%; transform: translate(-50%, 0); background-color: rgba(255,255,255,0.7); padding: 5px 10px; border-radius: 5px; font-weight: bold; white-space: nowrap;"
              )
            )
          } else {
            fluidRow(
              column(6,
                     style = "position: relative; border-left: 2px solid rgba(0, 0, 0, 0.1);",
                     style = "position: relative; border-right: 2px solid rgba(0, 0, 0, 0.1);",
                     visNetworkOutput("initialPlot"),
                     style = "padding-top: 50px;",
                     tags$div(
                       "Graph of possible whitelist",
                       br(),
                       style = "position: absolute; top: 10px; left: 50%; transform: translate(-50%, 0); background-color: rgba(255,255,255,0.7); padding: 5px 10px; border-radius: 5px; font-weight: bold; white-space: nowrap;"
                     )
              ),
              column(6,
                     style = "padding-left: 20px;",
                     tags$div(
                       style = "position: relative;",
                       visNetworkOutput("plot"),
                       style = "padding-top: 50px;",
                       tags$div(
                         "Final Whitelist after Cycle check",
                         br(),
                         style = "position: absolute; top: 10px; left: 50%; transform: translate(-50%, 0); background-color: rgba(255,255,255,0.7); padding: 5px 10px; border-radius: 5px; font-weight: bold; white-space: nowrap;"
                       )
                     )
              )
            )
          }
        })
        # --------------------------------------
        output$checkboxUI <- renderUI({
          tryCatch({
            # Inside tryCatch to catch potential errors
            cycles <- FindCycles(g())
            #-------------
            observe({
              print("Cycles found:")
              print(FindCycles(g()))
            })
            #-------------
            print(paste("Number of cycles found:", length(cycles)))

            #-------------
            output$cycleMessage <- renderUI({
              if (length(cycles) == 0) {
                No_Cycle_plot(TRUE)
                return(NULL)  # No message will be printed
              } else {
                num_cycles <- length(cycles)
                if (num_cycles == 1) {
                  HTML("<div style='display: flex; justify-content: center; align-items: center; height: 100%;'><div style='text-align: center; font-family: Arial; font-size: 14px; color: white;'><strong>There is one cycle</strong></div></div>")
                } else {
                  HTML(paste0("<div style='display: flex; justify-content: center; align-items: center; height: 100%;'><div style='text-align: center; font-family: Arial; font-size: 14px; color: white;'><strong>There are ", num_cycles, " cycles</strong></div></div>"))
                }
              }
            })
            # --------------------------------------------------
            ids <- paste0("cycle_", seq_along(cycles))
            # Convert cycles to arcs using graph for vertex names
            cycle_arcs = lapply(cycles, cycle_to_arcs, g())

            ui_elems <- lapply(seq_along(cycle_arcs), function(i) {
              choices = setNames(cycle_arcs[[i]], cycle_arcs[[i]])
              column(6, checkboxGroupInput(ids[i], label = paste0("Cycle ", i), choices = choices))
            })

            # Wrap every two cycles in a fluidRow
            rows <- split(ui_elems, ceiling(seq_along(ui_elems)/2))
            tagList(lapply(rows, function(row) fluidRow(row)))
            # ui_layout <- tagList(lapply(rows, function(row) fluidRow(row)))

          }, error = function(e) {
            # Print error message
            print(paste("Error in rendering checkbox UI:", e$message))
          })
        })
        # --------------------------------------------------new
        output$removeButtonUI <- renderUI({
          # length of cycles?
          cycles <- FindCycles(g())

          if (length(cycles) > 0) {
            # If cycles exist, render button
            div(
              style = "display: flex; justify-content: center;", # Use flexbox to center div
              actionButton(inputId = "remove",
                           label = tags$strong(tags$span(icon("fas fa-hand-pointer", style="margin-right: 4px; color: red;")), "Remove selected arcs"), # Wrapped in tags$strong
                           style = "background-color: #3498db; color: white; padding: 10px 20px; font-family: 'Arial'; font-size: 14px; border-radius: 5px; cursor: pointer; margin-left: 10px;")
            )
          } else {            # Else, don't render anything
            return(NULL)
          }
        })
        # --------------------------------------
        observeEvent(input$remove, {

          # Show modal indicating cycle resolution process is starting
          showModal(modalDialog(
            tags$div(
              style = "font-size:18px; color:#34495E; padding: 20px; background-color: #EAECEE; border-radius: 5px; text-align: center;",
              tags$p(
                tags$span(
                  style = "color: #3498db;",
                  icon("sync", style = "margin-right: 6px; color: #3498db; animation: spin 2s linear infinite;"),
                  tags$span(style = "font-weight: bold;", "Resolving Cycles: ")
                ),
                "Please wait, we're resolving the cycles..."
              )
            ),
            footer = NULL, # No footer to prevent user from closing modal
            easyClose = FALSE, # Prevent closing by clicking outside modal
            size = "s"
          ))

          cycles <- FindCycles(g())
          # ----------------
          # if no cycles
          if (length(cycles) == 0) {
            No_Cycle_plot(TRUE)
            print("If condition:possible_whitelist_reactiveVal:")
            print(possible_whitelist_reactiveVal())
            print(possible.white.list)

          } else {

            g_final <- g()
            # List to store all edges to be removed
            edges_to_remove_names <- c()

            for (i in seq_along(cycles)) {
              id <- paste0("cycle_", i)
              selected_edges <- input[[id]]
              if(is.null(selected_edges)) {
                edges_to_remove_names <- c(edges_to_remove_names, cycle_to_arcs(cycles[[i]], g())[[1]])
              } else {
                edges_to_remove_names <- c(edges_to_remove_names, selected_edges)
              }
            }
            # Convert edge names to edge indices
            all_edge_names <- getEdgeNames(g_final)
            edges_to_remove_indices <- which(all_edge_names %in% edges_to_remove_names)
            g_final <- delete_edges(g_final, edges_to_remove_indices)

            # final_white_list(as.data.frame(get.edgelist(g_final, names = TRUE)))
            g_arcs_final <- as.data.frame(get.edgelist(g_final, names = TRUE))
            colnames(g_arcs_final) <- c("from", "to")
            g_arcs_final$from <- as.character(g_arcs_final$from)
            g_arcs_final$to <- as.character(g_arcs_final$to)
            final_white_list(g_arcs_final)

            print("final_white_list:")
            print(final_white_list()) # we printed data frame with 2 columns

            output$final_white_list <- renderStyledTable(final_white_list(), rownames = TRUE, download_version = c('csv', 'excel'))

            # Convert igraph object to visNetwork suitable format
            vis_graph <- reactive({
              nodes_data <- data.frame(id = V(g_final)$name, label = V(g_final)$name)
              edges_data <- get.data.frame(g_final, what = "edges")
              list(nodes = nodes_data, edges = edges_data)
            })



            # -------------------------------------
            output$plot <- renderVisNetwork({
              visNetwork(nodes = vis_graph()$nodes, edges = vis_graph()$edges) %>%
                visNodes(shape = "circle", color = list(background = "#32a89e")) %>%
                visEdges(arrows = "to",
                         smooth = TRUE,
                         font = list(size = 15, color = "black", background = '#32a89e')) %>%
                visOptions(highlightNearest = list(enabled = F, hover = F),
                           nodesIdSelection = F)%>%
                visLayout(randomSeed = 123,
                          improvedLayout = TRUE)  %>%
                visPhysics(solver = "forceAtlas2Based",  # physics solver
                           forceAtlas2Based = list(gravitationalConstant = -50,  #
                                                   centralGravity = 0.005,  #
                                                   springLength = 100,  #
                                                   springConstant = 0.18))  #
            })
          }
          #   # ----------------------------------
          shinyjs::delay(1000, {  # Delay in milliseconds
            removeModal()  # Ensure "Resolving Cycles" modal is removed

            shinyalert::shinyalert(
              title = "Cycles Resolved Successfully",
              # title = "Cycles Resolved",
              # text = "All cycles were removed successfully.",
              # text = "All cycles were removed successfully.",
              type = "success"
            )

            cycles_resolved(TRUE)  # Update cycle resolution state

          })

          Final.DAG_network_plot <- Final.DAG_network_plot (augmented_edge_list,
                                                            possible_seed_arcs_filter,
                                                            data(), discretized_data,
                                                            final_white_list(),
                                                            Black_List(),
                                                            input$nboot, cl,
                                                            corrcoef)

          plot_done(TRUE)

          print(class(Final.DAG_network_plot))
          print(names(Final.DAG_network_plot))

          Alg.Count_arcs.strength.table <- Final.DAG_network_plot$Alg.Count_arcs.strength.table

          fBRCABN(Final.DAG_network_plot$fBRCABN)

          Contour_plot_userSelected_feature <- Contour_plot_userSelected_feature (data(), discretized_data,
                                                                                  fBRCABN(),
                                                                                  input$userSelected_Status,
                                                                                  input$userSelected_key_feature)

          plots_list(Contour_plot_userSelected_feature)
          # -----------------
          output$DAG.Plot <- renderPlot({
            req(Final.DAG_network_plot)  # Ensures that Final.DAG_network_plot is loaded and not NULL
            plot_function <- Final.DAG_network_plot[["plotFunction"]]
            if (is.function(plot_function)) {
              plot_function()  # Executes plotting function
            } else {
              print("plot_function is not a function.")  # Debugging line to check if plot_function is correct
            }
          })
          # -----------------
          arcs <- Final.DAG_network_plot$arcs.BRCA
          P_strength <- Final.DAG_network_plot$P_strength

          arc_slopes.strength <- Final.DAG_network_plot$arc_slopes.strength

          # -----------------
          output$Diagnostic_plot <- renderPlot({
            # Assuming Diagnostic_plot() returns a ggplot object
            p <- Diagnostic_plot(num.white_thresh, num_arcs.All.thresh, Total.BIC.thresh, threshold)
            p
          })

          output$downloadPlot_Diagnostic <- downloadHandler(
            filename = function() {
              paste("Diagnostic-plot-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".png", sep="")
            },
            content = function(file) {
              # Generate plot object
              p <- Diagnostic_plot(num.white_thresh, num_arcs.All.thresh, Total.BIC.thresh, threshold)

              ggsave(file, plot = p, device = "png", width = 10, height = 8, dpi = 600)

            }
          )
          # --------------
          output$Plot.Algorithm.Count_arcs.strength <- renderPlot({
            p <- plot_Algorithm.Count_arcs.strength(Alg.Count_arcs.strength.table)
            p
          })

          output$downloadPlot_Algorithm.Count <- downloadHandler(
            filename = function() {
              paste("Algorithm.Count_arcs.strength-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".png", sep="")
            },
            content = function(file) {
              # Generate plot object
              p <- plot_Algorithm.Count_arcs.strength(Alg.Count_arcs.strength.table)
              #
              ggsave(file, plot = p, device = "png", width = 10, height = 8, dpi = 600)
            }
          )
          # --------------
          output$DAGNetworkPlot <- renderVisNetwork({
            Final.DAG_network_plot$network
          })
          # --------------
          output$DAGNetworkPlot_intercept <- renderVisNetwork({
            Final.DAG_network_plot$network_intercept
          })
          # --------------
          final_DAG_detail <- Final.DAG_network_plot$final_DAG_detail
          final_DAG_detail_reactiveVal(final_DAG_detail)

          output$arc_slopes_strength <- renderStyledTable(final_DAG_detail, rownames = TRUE, download_version = c('csv', 'excel'))
          # --------------
        })
        # --------------------------------------
        # Detect categorical columns and set up Status options
        observe({
          req(fileInputState()) # Ensure file is selected


          categorical_cols <- sapply(data(), function(x) { all(x %in% as.integer(x)) })
          categorical_col_names <- names(categorical_cols[categorical_cols])
          print(categorical_col_names)
          print(length(categorical_col_names))

          if (length(categorical_col_names) == 0) {
            # No categorical columns found, UPDATE VAR
            has.Categorical.Columns(FALSE)

          } else if (length(categorical_col_names) == 1) {
            has.Categorical.Columns(TRUE)

            # If there's exactly one categorical column, use it as Status
            updateSelectInput(session, "userSelected_Status", choices = categorical_col_names, selected = categorical_col_names[1])
          } else {
            has.Categorical.Columns(TRUE)
            # If there are multiple categorical columns, let user choose
            current_status <- input$userSelected_Status
            if (!is.null(current_status) && current_status %in% categorical_col_names) {
              # Current selection is valid, do nothing
              print("Current selection is already valid.")
            } else {
              # Update because current selection is not valid or not set
              updateSelectInput(session, "userSelected_Status", choices = categorical_col_names)
            }
          }
        })
        # -----------------------------------
        # Reactive expression for network plot
        observe({
          currentTab <- input$sidebarMenu

          if( (update_clicked() || !contour_plot_initial()) && (currentTab == "contour_plot" )){
            print("UPDATE IS CLICKED")

            if(has.Categorical.Columns()){

              showModal(modalDialog(
                tags$div(
                  style = "font-size:18px; color:#34495E; padding: 10px 20px; background-color: #EAECEE; border-radius: 5px; text-align: center; box-shadow: 0 4px 8px rgba(0,0,0,0.1);  margin: auto;",
                  tags$p(
                    tags$span(
                      style = "color: #2980b9;",
                      icon("sync", style = "margin-right: 6px; color: #2980b9; animation: spin 2s linear infinite;"),
                      tags$span(style = "font-weight: bold;", "Generating the contour Plot: ")
                    ),
                    # "Please wait, we're generating plot.."
                    "Please wait, BaMANI is generating the plot..."
                  )
                ),
                footer = NULL,  # No footer
                easyClose = FALSE,  # Prevent closing by clicking outside modal
                size = "s"  # Small size, but width is adjusted using custom styles
              ))
            }

            Contour_plot_userSelected_feature <- Contour_plot_userSelected_feature (data(), discretized_data,
                                                                                    fBRCABN(),
                                                                                    input$userSelected_Status,
                                                                                    input$userSelected_key_feature)

            plots_list(Contour_plot_userSelected_feature)
            # --------------

            print("I got in the contour plot!!")
            selectedCellType <- NULL
            if(!contour_plot_initial()){
              selectedCellType <- input$selectedCellType
            }else{
              selectedCellType <- selectedInputs$secondaryFeature
            }
            #print("THIS IS SELECTED CELL TYPE")
            print(selectedCellType)


            output$contour_plot <- renderPlot({
              # make sure user has selected  key feature and plots_list is not null
              req(selectedCellType, !is.null(plots_list()))
              print("I got in contour plot!! 2")
              # Access user-selected key feature
              print("I got in the contour plot!! 3")
              print("Accessing plots list")
              print( names(plots_list()))
              print("I got in the contour plot!! 4")
              if(selectedCellType %in% names(plots_list())) {
                cat("selected cell type found in plots_list\n")
              } else {
                cat("selected cell type NOT found in plots_list\n")
              }

              # Assuming plots_list contains ggplot objects or similar
              # Render plot corresponding to selected key feature
              plot_to_render <- plots_list()[[selectedCellType]]


              if(!is.null(plot_to_render)) {
                print(plot_to_render)
              } else {
                cat("Plot for selected key feature is NULL\n")
              }

              plot_to_render  # static plot

              if(has.Categorical.Columns()){
                contour_plot_initial(TRUE)
                removeModal()
                # update_clicked(FALSE)
                shinyalert::shinyalert(
                  title = "Process Completed",
                  text = "The analysis based
       on user selections has been completed successfully!",
                  type = "success"
                )
                update_clicked(FALSE)
              }
            })
            #--------------
            output$downloadPlot_contour <- downloadHandler(
              req(selectedCellType, !is.null(plots_list())),
              filename = function() {
                # Access reactive value directly from input
                if (!is.null(selectedCellType)) {
                  paste("Contour-Plot-", selectedCellType, "-", input$userSelected_Status, "-", input$userSelected_key_feature, "-",format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".png", sep="")
                } else {
                  paste("Contour-Plot-", "default", "-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".png", sep="")
                }
              },
              content = function(file) {
                # Reactive values should be accessed directly from input
                if (!is.null(selectedCellType) && selectedCellType %in% names(plots_list())) {
                  plot_to_render <- plots_list()[[selectedCellType]]
                  if (!is.null(plot_to_render)) {
                    # Save plot as a PNG file with high resolution
                    ggsave(file, plot = plot_to_render, device = "png", width = 10, height = 8, dpi = 600)
                  } else {
                    cat("Plot for selected cell type is NULL or not available.\n")
                  }
                } else {
                  cat("Selected cell type is invalid or missing.\n")
                }
              }
            )
            #--------------
          } # if
        })
        # END HERE
        # --------------------------------------
        # Function to convert cycle to list of arcs using original vertex names
        cycle_to_arcs <- function(cycle, graph) {
          arc_list = c()
          for (i in 1:(length(cycle) - 1)) {
            arc_list = c(arc_list, paste(V(graph)[cycle[i]]$name, "\u2192", V(graph)[cycle[i+1]]$name))
          }
          return(arc_list)
        }
        # --------------------------------------
        getEdgeNames <- function(graph) {
          ends <- get.edges(graph, E(graph))
          paste(V(graph)[ends[, 1]]$name, "\u2192", V(graph)[ends[, 2]]$name)
        }
        # --------------------------------------
        edgeNamesToVertices <- function(graph, edge_names = NULL) {
          if(is.null(edge_names)) {
            edge_names <- getEdgeNames(graph)
          }

          edge_names <- as.character(edge_names)  # Ensure it's character

          lapply(edge_names, function(edge) {
            vertex_names <- unlist(strsplit(edge, "\u2192"))
            sapply(vertex_names, function(vname) {
              which(V(graph)$name == vname)
            })
          })
        }
        # --------------------------------------
        edgeNamesToIndices <- function(graph, edge_names = NULL) {
          if (is.null(edge_names)) {
            return(NULL)
          }
          e_names <- getEdgeNames(graph)
          indices <- which(e_names %in% edge_names)

          return(indices)
        }
        # -------------
      } else {
        # } else if (!data_present() & (!Black_List_present() | !White_List_present())) {
        # } else if ( !default_data_used() & !data_present() & (!Black_List_present() | !White_List_present())) {
        # } else if (!data_present || !Black_List_present || !White_List_present) {
        showModal(modalDialog(
          # ---------------------
          tags$div(
            style = "font-size:18px; color:#34495E; padding: 10px 20px; background-color: #EAECEE; border-bottom-left-radius: 5px; border-bottom-right-radius: 5px;",
            tags$p(
              tags$span(
                style = "color: #d68910 ;",
                icon("exclamation-triangle", style = "margin-right: 6px; color: #d68910 ;"),
                tags$span(style = "font-weight: bold;", "Data Notice: ")
              ),
              "User data files have not been uploaded. Would you like to proceed with the default files to learn about the process?"            )
          ),
          # ----------------
          footer = tagList(
            actionButton("ok", "Yes, Use Default", icon = icon("check-circle"),
                         style = "background-color: #58d68d; color: black; border: none; padding: 8px 18px; border-radius: 4px; margin: 5px;"),
            tags$button("No, Upload User Data", type = "button", class = "btn btn-default shiny-modal-action-button", `data-dismiss`="modal",
                        icon = icon("upload"),
                        style = "background-color: #f1948a; color: black; border: none; padding: 8px 18px; border-radius: 4px; margin: 5px;")
          ),
          # ----------------
          easyClose = TRUE
        ))
      }
    }
  })
  #-------------------------------------------------------------------------------------------
  observeEvent(input$ok, {
    data_default <- read.csv(paste0(datapath, "BRCA_Updated_Data.csv"), header = TRUE)
    data(data_default)  # Set new value for data

    Black_List_default <- read.csv(paste0(datapath, "BRCA_BlackList.csv"), header = TRUE)
    Black_List(Black_List_default)  # Set new value for data

    # White_List_default <- read.csv(paste0(datapath, "WhiteList.csv"), header = TRUE)
    # White_List(White_List_default)  # Set new value for data

    default_data_used(TRUE)

    removeModal()
    showModal(modalDialog(
      # ---------------------
      tags$div(
        style = "font-size:18px; color:#34495E; padding: 10px 20px; background-color: #EAECEE; border-bottom-left-radius: 5px; border-bottom-right-radius: 5px;",
        tags$p(
          tags$span(
            style = "color: #d68910;",
            icon("exclamation-triangle", style = "margin-right: 6px; color: #d68910;"),
            tags$span(style = "font-weight: bold;", "Notification: ")
          ),
          "Default files have been successfully loaded.", tags$br(),
          "Please hit ",
          tags$span(
            style = "font-weight: bold; color:#2980B9;",
            "Run Discovery"
          ),
          " button to proceed.",
          tags$span(
            class = "fas fa-hand-pointer",
            style = "transform: rotate(180deg); margin-left: 4px; display: inline-block;"
          )
        )
      ),
      # ----------------
      easyClose = TRUE,
      footer = NULL
    ))
  })
  # -------------------------------
  observe({
    req(data()) # Ensure file is selected

    # Current selection
    current_status <- input$userSelected_Status

    categorical_cols <- sapply(data(), function(x) { all(x %in% as.integer(x)) })
    categorical_col_names <- names(categorical_cols[categorical_cols])
    print(categorical_col_names)
    print(length(categorical_col_names))

    print("current status")
    print(current_status)

    userChangedStatus <- isTRUE(userSelected())


    if (length(categorical_col_names) == 0) {
      has.Categorical.Columns(FALSE)

    } else if (length(categorical_col_names) == 1) {
      has.Categorical.Columns(TRUE)

      # If there's exactly one categorical column, use it as Status
      updateSelectInput(session, "userSelected_Status", choices = categorical_col_names, selected = categorical_col_names[1])
    } else {
      has.Categorical.Columns(TRUE)

      # If there are multiple categorical columns, let user choose
      current_status <- input$userSelected_Status
      if (!is.null(current_status) && current_status %in% categorical_col_names) {
        # Current selection is valid, do nothing
        print("Current selection is already valid.")
      } else {
        # Update because current selection is not valid or not set
        updateSelectInput(session, "userSelected_Status", choices = categorical_col_names)
      }
    }

    valid_features <- setdiff(names(data()), input$userSelected_Status)

    # Preserve current selection if still valid after update
    current_key_feature <- input$userSelected_key_feature
    if (!current_key_feature %in% valid_features && length(valid_features) > 0) {
      current_key_feature <- valid_features[1]
    }
    updateSelectInput(session, "userSelected_key_feature", choices = valid_features, selected = current_key_feature)

    valid_secondary_features <- setdiff(names(data()), c(input$userSelected_Status, current_key_feature))

    # Preserve current secondary selection if still valid after update
    current_secondary_selection <- input$selectedCellType
    if (!current_secondary_selection %in% valid_secondary_features && length(valid_secondary_features) > 0) {
      current_secondary_selection <- valid_secondary_features[1]
    }
    updateSelectInput(session, "selectedCellType", choices = valid_secondary_features, selected = current_secondary_selection)
  })

  # -------------------------------
  observeEvent(input$close, {
    # Close modal if "Acknowledge" clicked
    removeModal()
  })
  # --------------------------
}
