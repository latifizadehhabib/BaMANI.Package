# ------------
tabsetPanel(id = "tabset20",
            tabPanel(
              title = "Min BIC table",
              fluidRow(
                column(
                  12,
                  box(
                    title = "Min BIC table",
                    status = "primary",
                    solidHeader = TRUE,
                    div(
                      style = "width: 900px; height: 400px; overflow: auto;",
                      DTOutput("bic_min_table")
                    )
                  ),
                  width = 12,
                  collapsible = TRUE
                )
              )
            ),
            tabPanel(
              title = "Plot Before Cycle Check",
              fluidRow(
                column(
                  12,
                  box(
                    title = "Plot Before Cycle Check",
                    status = "info",
                    solidHeader = TRUE,
                    div(
                      style = "width: 900px; height: 400px; overflow: auto;",
                      plotOutput("plot_before")
                  )
                  ),
                  width = 12,
                  collapsible = TRUE
                )
              )
            )
)
# ------------
tabsetPanel(id = "tabset3",
            tabPanel(
              title = "DAG Network Plot",
              fluidRow(
                column(
                  8,
                  box(
                    title = "DAG Network Plot",
                    status = "primary",
                    solidHeader = TRUE,
                    div(
                      style = "width: 700px; height: 400px; overflow: auto;",
                      visNetworkOutput("DAGNetworkPlot")
                  ),
                    width = 12,
                    collapsible = TRUE
                  )
                )
              )
            ),
            tabPanel(
              title = "Arc Slopes Strength Table",
              fluidRow(
                column(
                  8,
                  box(
                    title = "Arc Slopes Strength Table",
                    status = "info",
                    solidHeader = TRUE,
                    div(
                      style = "width: 700px; height: 300px; overflow: auto;",
                      DTOutput("arc_slopes_strength")
                  ),
                    width = 12,
                    collapsible = TRUE
                  )
                )
              )
            )
)
# ------------


# ------------
tabsetPanel(id = "tabset12",
            tabPanel(
              # title = "Diagnostic plot",
              fluidRow(
                column(
                  12,
                  box(
                    title = "Arcs and thier corresponding info",
                    status = "primary",
                    solidHeader = TRUE,
                    div(
                      style = "width: 1000px; height: 400px; overflow: auto;",
                      DTOutput("possible_seed_arcs_filter")
                    ),
                    width = 12,
                    collapsible = TRUE
                  )
                )
              )
            )
)
# ------------

tabItem(tabName = "WhiteList_Check_acyclicity", 
        h3("Min BIC score for a node with min number of parents in corresponding final DAG / threshold "), 
        tags$p("Description: Min BIC score for a node with min number of parents in corresponding final DAG / threshold ", 
               style = "font-family: 'Arial'; font-size: 14px; color: #333;"),
        
        fluidRow(
          # First Column with a Tabset Panel having two tabs
          column(6, 
                 tabsetPanel(
                   id = "tabset1",
                   tabPanel(
                     title = "Plot Before Cycle Check",
                     box(
                       width = 12,
                       plotOutput("plot_before")
                     )
                   ),
                   tabPanel(
                     title = "Possible White List",
                     box(
                       width = 12,
                       DTOutput("possible.white.list")
                     )
                   )
                 )
          ),
          
          # Second Column with a Tabset Panel having two tabs
          column(6, 
                 tabsetPanel(
                   id = "tabset2",
                   tabPanel(
                     title = "Plot After Cycle Check",
                     box(
                       width = 12,
                       plotOutput("plot_after")
                     )
                   ),
                   tabPanel(
                     title = "Final White List",
                     box(
                       width = 12,
                       DTOutput("Final_white_list")
                     )
                   )
                 )
          )
        )
)

# ----------------------------------

tabsetPanel(id = "tabset4",
            tabPanel(
              title = "Comparative Analysis",
              fluidRow(
                column(
                  12,
                  box(
                    status = "primary",
                    solidHeader = TRUE, # You can decide whether to have a solid header or not.
                    collapsible = TRUE, # You can decide whether to make it collapsible or not.
                    width = 12,
                    fluidRow(
                      box(
                        # title = "Select feature",
                        width = 3,
                        selectInput("selectedCellType", "Select a feature:", choices = NULL)
                        # selectInput("userSelectedCell", "Choose a Cell Type:", choices = names(data()))
                        # selectInput("userSelectedCell", "Choose CCN4:", choices = NULL),  # Empty 
                        # selectInput("X.axis.cell", "Choose a Cell Type:", choices = NULL)  # Empty 
                      ),
                      box(
                        title = "", # title = "Contour Plot",
                        width = 9,
                        plotOutput("contour_plot")
                      )
                    )
                  )
                )
              )
            )
)


