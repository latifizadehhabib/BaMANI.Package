# File: R/ui.R

#' UI function for My Shiny App
#'
#' @importFrom shiny fluidPage
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody sidebarMenu menuItem menuSubItem tabItems tabItem tabsetPanel tabPanel
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets actionButton
#' @importFrom htmltools tags
#' @importFrom DT DTOutput
#' @importFrom visNetwork visNetworkOutput
#' @importFrom plotly plotOutput
#' @importFrom shinyalert useShinyalert
#' @export
ui <- function() {

  # Styled title function
  styled_title <- function(text, bgcolor = "#007BFF", color = "white", padding = "6px 6px", border_radius = "10px", font_size = "14px", box_shadow = "2px 2px 10px #888888") {
    HTML(paste0('<div style="background-color: ', bgcolor,
                '; color: ', color,
                '; padding: ', padding,
                '; border-radius: ', border_radius,
                '; font-size: ', font_size,
                '; font-weight: bold;
                   text-align: center;
                   box-shadow: ', box_shadow, ';">', text, '</div>'))
  }

  # Define UI for application
  dashboardPage(

    dashboardHeader(title = "BaMANI"),

    dashboardSidebar(
      useShinyjs(),
      sidebarMenu(
        id = "sidebarMenu",
        menuItem("Settings", tabName = "settings", icon = icon("gear")),
        menuItem("User Guide", tabName = "user_guide", icon = icon("file-pdf")),

        menuItem("Data", icon = icon("database"),

                 menuSubItem("Data Structure", tabName = "data_characteristics_Distribution_Correlation_BlackList")
        ),

        menuItem("Algorithms Training", icon = icon("cogs"),
                 menuSubItem("Learning Dynamics", tabName = "directed_Undirected_algorithm_messages")
        ),

        menuItem("Arcs Info", icon = icon("project-diagram"),
                 # menuItem("Edges", icon = icon("project-diagram"),


                 menuSubItem("Augmented Arcs Table", tabName = "augmented_edge_list_thresh_cols_possible_seed_arcs_table")
        ),

        menuItem("Whitelisting Arcs", icon = icon("vial"),
                 # menuItem("Diagnostics", icon = icon("vial"),


                 menuSubItem("Model Selection", tabName = "L1_Parent_bic_table"),
                 menuSubItem("Whitelisted Arcs & Acyclicity", tabName = "WhiteList_Check_acyclicity")
        ),

        menuItem("DAG Network Analysis", icon = icon("chart-line"),

                 menuSubItem("Diagnostic Plot", tabName = "Algorithm_Count__Arc_Strength_diagnostic_plot"), # icon = icon("analysis-panel")
                 menuSubItem("Ensemble DAG Network", tabName = "DAG_network_plot"),
                 menuSubItem("Ensemble vs. Single Algorithm", tabName = "run_algorithms_plot"), # icon = icon("bar-chart-o")
                 menuSubItem("Comparative Analysis", tabName = "contour_plot")
        )
      )
    ),
    # -----------
    dashboardBody(
      # -----------
      useShinyjs(),
      # ----------- download Radar plot
      # tags$head(
      #   tags$script(HTML("
      #     $(document).ready(function() {
      #         $('#downloadPlot_radar_plot').on('click', function(event) {
      #             event.preventDefault(); // Stop the button from doing any default submission
      #             var img = document.querySelector('#radar_plot img'); // Correctly escape the dot in the ID
      #             if (img && img.src) {
      #                 var date = new Date();
      #                 var timestamp = date.getFullYear() + '-' +
      #                                 ('0' + (date.getMonth() + 1)).slice(-2) + '-' +
      #                                 ('0' + date.getDate()).slice(-2) + '-' +
      #                                 ('0' + date.getHours()).slice(-2) + '-' +
      #                                 ('0' + date.getMinutes()).slice(-2) + '-' +
      #                                 ('0' + date.getSeconds()).slice(-2);
      #                 var link = document.createElement('a');
      #                 link.download = 'Radar-Plot-' + timestamp + '.png'; // Create a file name with timestamp
      #                 link.href = img.src; // Directly use the image's URL
      #                 document.body.appendChild(link);
      #                 link.click(); // Trigger the download
      #                 document.body.removeChild(link);
      #             } else {
      #                 console.log('No image found for download.'); // If no image, log error
      #             }
      #         });
      #     });
      #   "))),
      # -----------download Bar Plot
      tags$head(
        tags$script(HTML("
        $(document).ready(function() {
            $('#downloadBarPlot').on('click', function(event) {
                event.preventDefault(); // Prevent the form from submitting via the default action
                var img = document.querySelector('#bar_plot img'); // Select the image within the bar_plot div
                if (img && img.src) {
                    var date = new Date();
                    var timestamp = date.getFullYear() + '-' +
                                    ('0' + (date.getMonth() + 1)).slice(-2) + '-' +
                                    ('0' + date.getDate()).slice(-2) + '-' +
                                    ('0' + date.getHours()).slice(-2) + '-' +
                                    ('0' + date.getMinutes()).slice(-2) + '-' +
                                    ('0' + date.getSeconds()).slice(-2);
                    var link = document.createElement('a');
                    link.download = 'Bar-Plot-' + timestamp + '.png'; // Set the download filename with timestamp
                    link.href = img.src; // Set the href to the source of the image
                    document.body.appendChild(link);
                    link.click(); // Programmatically click the link to trigger the download
                    document.body.removeChild(link); // Remove the link when done
                } else {
                    console.log('No image found for download.'); // Log an error if no image was found
                }
            });
        });
      "))),
      # ----------- download Scatter Plot
      tags$head(
        tags$script(HTML("
        $(document).ready(function() {
            $('#downloadScatterPlot').on('click', function(event) {
                event.preventDefault(); // Prevent the form from submitting via the default action
                var img = document.querySelector('#scatter_plot img'); // Select the image within the scatter_plot div
                if (img && img.src) {
                    var date = new Date();
                    var timestamp = date.getFullYear() + '-' +
                                    ('0' + (date.getMonth() + 1)).slice(-2) + '-' +
                                    ('0' + date.getDate()).slice(-2) + '-' +
                                    ('0' + date.getHours()).slice(-2) + '-' +
                                    ('0' + date.getMinutes()).slice(-2) + '-' +
                                    ('0' + date.getSeconds()).slice(-2);
                    var link = document.createElement('a');
                    link.download = 'Scatter-Plot-' + timestamp + '.png'; // Set the download filename with timestamp
                    link.href = img.src; // Set the href to the source of the image
                    document.body.appendChild(link);
                    link.click(); // Programmatically click the link to trigger the download
                    document.body.removeChild(link); // Remove the link when done
                } else {
                    console.log('No image found for download.'); // Log an error if no image was found
                }
            });
        });
      "))),
      # ----------- download Single DAG plot tab color
      # tags$head(
      #   tags$style(HTML("
      #   /* Matching the tab headers to box colors in singleAlg.DAG */
      #   #singleAlg\\.DAG .nav-tabs > li.active > a,
      #   #singleAlg\\.DAG .nav-tabs > li.active > a:focus,
      #   #singleAlg\\.DAG .nav-tabs > li.active > a:hover {
      #     color: white !important;
      #   }
      #   #singleAlg\\.DAG .nav-tabs > li > a[data-value='Single Algorithm DAG network'] {
      #     background-color: #337ab7 !important; /* Bootstrap primary color for 'primary' */
      #     color: white !important;
      #   }
      #   #singleAlg\\.DAG .nav-tabs > li > a[data-value='DAG network Table'] {
      #     background-color: #5cb85c !important; /* Bootstrap success color for 'success' */
      #     color: white !important;
      #   }
      # "))
      # ),
      # ----------- Single DAG plot tab color

      # tags$head(
      #   tags$style(HTML("
      #   /* Matching the tab headers to box colors in singleAlg.DAG */
      #   #singleAlg\\.DAG a[data-value='DAG network'] {
      #     background-color: #337ab7 !important; /* Bootstrap primary color for 'primary' */
      #     color: white !important;
      #   }
      #   #singleAlg\\.DAG a[data-value='Single Algorithm DAG Summary Table'] {
      #     background-color: #5cb85c; /* Bootstrap success color for 'success' */
      #     color: white !important;
      #   }
      # "))
      # ),
      # ----------- Single DAG plot tab color

      # tags$head(
      #   tags$style(HTML("
      #     /* Matching the tab headers to box colors in singleAlg.DAG */
      #     #singleAlg.DAG a[data-value='DAG network'] {
      #         background-color: #337ab7 !important; /* Bootstrap primary color for 'primary' */
      #         color: white !important;
      #     }
      #     #singleAlg.DAG a[data-value='Single Algorithm DAG Summary Table'] {
      #         background-color: #5cb85c; /* Bootstrap success color for 'success' */
      #         color: white;
      #     }
      # "))
      # ),

      # ----------- Single DAG plot download


      tags$head(
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.3.2/html2canvas.min.js"),
        tags$script(HTML("
    $(document).ready(function() {
        $('#downloadPlot_run').on('click', function() {
            var algorithmName = $('#alg_directed').val(); // Fetching the selected algorithm name

            // Logging for debugging
            console.log('Button clicked, generating canvas...');

            // Fetching the dimensions of the plot container
            var plotContainer = document.querySelector('#plot_output_container');
            var plotContainerWidth = plotContainer.offsetWidth;
            var plotContainerHeight = plotContainer.offsetHeight;

            // Set the canvas dimensions
            var canvas = document.createElement('canvas');
            canvas.width = plotContainerWidth * 2; // Increase the resolution by scaling
            canvas.height = plotContainerHeight * 2; // Increase the resolution by scaling
            canvas.style.width = plotContainerWidth + 'px';
            canvas.style.height = plotContainerHeight + 'px';
            var context = canvas.getContext('2d');
            context.scale(2, 2);

            html2canvas(plotContainer, {
                canvas: canvas,
                useCORS: true,
                scrollY: -window.scrollY,
                scrollX: -window.scrollX,
                width: plotContainerWidth,
                height: plotContainerHeight
            }).then(function(canvas) {
                // Logging for debugging
                console.log('Canvas generated, creating download link...');

                var date = new Date();
                var timestamp = date.getFullYear() + '-' +
                                ('0' + (date.getMonth() + 1)).slice(-2) + '-' +
                                ('0' + date.getDate()).slice(-2) + '-' +
                                ('0' + date.getHours()).slice(-2) + '-' +
                                ('0' + date.getMinutes()).slice(-2) + '-' +
                                ('0' + date.getSeconds()).slice(-2);
                var link = document.createElement('a');
                link.download = algorithmName + '-Plot-' + timestamp + '.png'; // Including algorithm name in the file name
                link.href = canvas.toDataURL('image/png');
                link.click();

                // Logging for debugging
                console.log('Download link clicked, PNG should start downloading...');
            }).catch(function(error) {
                console.error('Error generating canvas:', error);
            });
        });
    });
  "))
      ),

      # ----------- Single DAG plot
      #   tags$head(
      #     tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.3.2/html2canvas.min.js"),
      #     tags$script(HTML("
      #   $(document).ready(function() {
      #       $('#downloadPlot_run').on('click', function() {
      #           var algorithmName = $('#alg_directed').val(); // Fetching the selected algorithm name
      #
      #           // Logging for debugging
      #           console.log('Button clicked, generating canvas...');
      #
      #           html2canvas(document.querySelector('#plot_output_container'), {
      #               scale: 6,
      #               useCORS: true,
      #               scrollY: -window.scrollY,
      #               scrollX: -window.scrollX,
      #               windowWidth: document.querySelector('#plot_output_container').scrollWidth,
      #               windowHeight: document.querySelector('#plot_output_container').scrollHeight
      #           }).then(function(canvas) {
      #               // Logging for debugging
      #               console.log('Canvas generated, creating download link...');
      #
      #               var date = new Date();
      #               var timestamp = date.getFullYear() + '-' +
      #                               ('0' + (date.getMonth() + 1)).slice(-2) + '-' +
      #                               ('0' + date.getDate()).slice(-2) + '-' +
      #                               ('0' + date.getHours()).slice(-2) + '-' +
      #                               ('0' + date.getMinutes()).slice(-2) + '-' +
      #                               ('0' + date.getSeconds()).slice(-2);
      #               var link = document.createElement('a');
      #               link.download = algorithmName + '-Plot-' + timestamp + '.png'; // Including algorithm name in the file name
      #               link.href = canvas.toDataURL('image/png');
      #               link.click();
      #
      #               // Logging for debugging
      #               console.log('Download link clicked, PNG should start downloading...');
      #           }).catch(function(error) {
      #               console.error('Error generating canvas:', error);
      #           });
      #       });
      #   });
      # "))
      #   ),
      # ----------- for renderStyledTable function
      # tags$head(
      #   tags$style(HTML("
      #   /* Center align text in column headers and apply light blue background */
      #   table.dataTable thead th {
      #     text-align: center !important;
      #     background-color: #e0f7fa; /* Light blue color */
      #   }
      #   /* Style for cell borders, hover, and stripes */
      #   .cell-border, .stripe, .hover {
      #     border-top: 1px solid #dee2e6;
      #     border-bottom: 1px solid #dee2e6;
      #   }
      # "))
      # ),
      # # -----------
      # tags$style(type = "text/css",
      #            ".box {
      #               border: 1px solid #ccc;
      #               box-shadow: 2px 2px 8px #aaa;
      #             }
      #             .info-icon {
      #               font-size: 16px;
      #               color: blue;
      #               cursor: pointer;
      #             }"),
      #-----------

      #-----------
      tags$head(
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.3.2/html2canvas.min.js"),
        tags$script(HTML("
    $(document).ready(function() {
        $('#downloadPlot_Correlation_Dendrograms').on('click', function() {
            html2canvas(document.querySelector('#correlation_structure'), {
                scale: 4, // Increasing scale for higher resolution
                useCORS: true,
                scrollY: -window.scrollY,
                scrollX: -window.scrollX,
                windowWidth: document.querySelector('#correlation_structure').scrollWidth,
                windowHeight: document.querySelector('#correlation_structure').scrollHeight
            }).then(function(canvas) {
                var date = new Date();
                var timestamp = date.getFullYear() + '-' +
                                ('0' + (date.getMonth() + 1)).slice(-2) + '-' +
                                ('0' + date.getDate()).slice(-2) + '-' +
                                ('0' + date.getHours()).slice(-2) + '-' +
                                ('0' + date.getMinutes()).slice(-2) + '-' +
                                ('0' + date.getSeconds()).slice(-2);
                var link = document.createElement('a');
                link.download = 'Correlation-Dendrograms-' + timestamp + '.png'; // Adjusted filename
                link.href = canvas.toDataURL('image/png');
                document.body.appendChild(link);
                link.click();
                document.body.removeChild(link);
            });
        });
    });
  "))
      ),
      # -----------
      tags$head(
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.3.2/html2canvas.min.js"),
        tags$script(HTML("
    $(document).ready(function() {
        $('#downloadPlot_Significant_Correlation').on('click', function() {
            html2canvas(document.querySelector('#correlation_structure_pi_sig'), {
                scale: 4, // Increasing scale for higher resolution
                useCORS: true,
                scrollY: -window.scrollY,
                scrollX: -window.scrollX,
                windowWidth: document.querySelector('#correlation_structure_pi_sig').scrollWidth,
                windowHeight: document.querySelector('#correlation_structure_pi_sig').scrollHeight
            }).then(function(canvas) {
                var date = new Date();
                var timestamp = date.getFullYear() + '-' +
                                ('0' + (date.getMonth() + 1)).slice(-2) + '-' +
                                ('0' + date.getDate()).slice(-2) + '-' +
                                ('0' + date.getHours()).slice(-2) + '-' +
                                ('0' + date.getMinutes()).slice(-2) + '-' +
                                ('0' + date.getSeconds()).slice(-2);
                var link = document.createElement('a');
                link.download = 'Significant-Correlation-' + timestamp + '.png'; // Adjusted filename
                link.href = canvas.toDataURL('image/png');
                document.body.appendChild(link);
                link.click();
                document.body.removeChild(link);
            });
        });
    });
  "))
      ),
      # -----------
      tags$head(
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.3.2/html2canvas.min.js"),
        tags$script(HTML("
    $(document).ready(function() {
        $('#downloadPlot_Data_Distribution').on('click', function() {
            html2canvas(document.querySelector('#variable_distribution'), {
                scale: 4, // Increasing scale for higher resolution
                useCORS: true,
                scrollY: -window.scrollY,
                scrollX: -window.scrollX,
                windowWidth: document.querySelector('#variable_distribution').scrollWidth,
                windowHeight: document.querySelector('#variable_distribution').scrollHeight
            }).then(function(canvas) {
                var date = new Date();
                var timestamp = date.getFullYear() + '-' +
                                ('0' + (date.getMonth() + 1)).slice(-2) + '-' +
                                ('0' + date.getDate()).slice(-2) + '-' +
                                ('0' + date.getHours()).slice(-2) + '-' +
                                ('0' + date.getMinutes()).slice(-2) + '-' +
                                ('0' + date.getSeconds()).slice(-2);
                var link = document.createElement('a');
                link.download = 'Data-Distribution-' + timestamp + '.png'; // Adjusted filename
                link.href = canvas.toDataURL('image/png');
                document.body.appendChild(link);
                link.click();
                document.body.removeChild(link);
            });
        });
    });
  "))
      ),
      # -----------
      tags$head(
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.3.2/html2canvas.min.js"),
        tags$script(HTML("
    $(document).ready(function() {
        $('#downloadPlot_DAG_flow_intercept').on('click', function() {
            html2canvas(document.querySelector('#DAGNetworkPlot_intercept'), {
                scale: 4, // Increasing scale for higher resolution
                useCORS: true,
                scrollY: -window.scrollY,
                scrollX: -window.scrollX,
                windowWidth: document.querySelector('#DAGNetworkPlot_intercept').scrollWidth,
                windowHeight: document.querySelector('#DAGNetworkPlot_intercept').scrollHeight
            }).then(function(canvas) {
                var date = new Date();
                var timestamp = date.getFullYear() + '-' +
                                ('0' + (date.getMonth() + 1)).slice(-2) + '-' +
                                ('0' + date.getDate()).slice(-2) + '-' +
                                ('0' + date.getHours()).slice(-2) + '-' +
                                ('0' + date.getMinutes()).slice(-2) + '-' +
                                ('0' + date.getSeconds()).slice(-2);
                var link = document.createElement('a');
                link.download = 'DAG-Network-Intercept-Plot-' + timestamp + '.png'; // Adjusted filename
                link.href = canvas.toDataURL('image/png');
                document.body.appendChild(link);
                link.click();
                document.body.removeChild(link);
            });
        });
    });
  "))
      ),
      # -----------
      tags$head(
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.3.2/html2canvas.min.js"),
        tags$script(HTML("
    $(document).ready(function() {
        $('#downloadPlot_DAG_flow').on('click', function() {
            html2canvas(document.querySelector('#DAGNetworkPlot'), {
                scale: 4, // Increasing scale for higher resolution
                useCORS: true,
                scrollY: -window.scrollY,
                scrollX: -window.scrollX,
                windowWidth: document.querySelector('#DAGNetworkPlot').scrollWidth,
                windowHeight: document.querySelector('#DAGNetworkPlot').scrollHeight
            }).then(function(canvas) {
                // Create a new date object and format the date and time for the filename
                var date = new Date();
                var timestamp = date.getFullYear() + '-' +
                                ('0' + (date.getMonth() + 1)).slice(-2) + '-' +
                                ('0' + date.getDate()).slice(-2) + '-' +
                                ('0' + date.getHours()).slice(-2) + '-' +
                                ('0' + date.getMinutes()).slice(-2) + '-' +
                                ('0' + date.getSeconds()).slice(-2);

                var link = document.createElement('a');
                link.download = 'DAG-Network-Plot-' + timestamp + '.png'; // Appending timestamp to filename
                link.href = canvas.toDataURL('image/png');
                document.body.appendChild(link);
                link.click();
                document.body.removeChild(link);
            });
        });
    });
  "))
      ),
      # -----------
      tags$head(
        tags$script(HTML("
            $(document).ready(function() {
                $('#downloadPlot_DAG').on('click', function(event) {
                    event.preventDefault(); // Stop the button from doing any default submission
                    var img = document.querySelector('#DAG\\\\.Plot img'); // Correctly escape the dot in the ID
                    if (img && img.src) {
                        var date = new Date();
                        var timestamp = date.getFullYear() + '-' +
                                        ('0' + (date.getMonth() + 1)).slice(-2) + '-' +
                                        ('0' + date.getDate()).slice(-2) + '-' +
                                        ('0' + date.getHours()).slice(-2) + '-' +
                                        ('0' + date.getMinutes()).slice(-2) + '-' +
                                        ('0' + date.getSeconds()).slice(-2);
                        var link = document.createElement('a');
                        link.download = 'DAG-Network-Plot-' + timestamp + '.png'; // Create a file name with timestamp
                        link.href = img.src; // Directly use the image's URL
                        document.body.appendChild(link);
                        link.click(); // Trigger the download
                        document.body.removeChild(link);
                    } else {
                        console.log('No image found for download.'); // If no image, log error
                    }
                });
            });
        "))
      ),
      # -----------
      #   tags$head(
      #     tags$style(HTML("
      #       /* Matching the tab headers to box colors in tabset33 */
      #       #tabset33 a[data-value='DAG network flow'] {
      #           background-color: #337ab7 !important; /* Bootstrap primary color for 'primary' */
      #           color: white !important;
      #       }
      #       #tabset33 a[data-value='DAG Network Plot'] {
      #           background-color: #5cb85c; /* Bootstrap success color for 'success' */
      #           color: white;
      #       }
      #       #tabset33 a[data-value='Single Algorithm DAG Summary Table'] {
      #           background-color: #5bc0de; /* Bootstrap info color for 'info' */
      #           color: white;
      #       }
      #   "))
      #   ),
      #   # -----------
      #   tags$head(
      #     tags$style(HTML("
      #   #tabset6 a[data-value='Whitelist and BIC per threshold'] {
      #       background-color: #337ab7 !important; /* Bootstrap primary color for 'primary' */
      #       color: white !important;
      #   }
      #   #tabset6 a[data-value='Algorithm count and arc strength'] {
      #       background-color: #5bc0de !important; /* Bootstrap info color for 'info' */
      #       color: white !important;
      #   }
      # "))
      #   ),
      # -----------
      # tags$head(
      #   tags$style(HTML("
      #     #tabset14 a[data-value='Data Distribution'] {
      #         background-color: #337ab7; /* Blue for 'primary' */
      #         color: white;
      #     }
      #     #tabset14 a[data-value='Correlation Structure'] {
      #         background-color: #5cb85c; /* Green for 'success' */
      #         color: white;
      #     }
      #     #tabset14 a[data-value='Data characteristics'] {
      #         background-color: #f0ad4e; /* Yellow for 'warning' */
      #         color: white;
      #     }
      #     #tabset14 a[data-value='BlackList'] {
      #         background-color: #d9534f; /* Red for 'danger' */
      #         color: white;
      #     }
      # "))
      # ),
      # # -----------
      # tags$head(
      #   tags$style(HTML("
      #     /* Matching the tab headers to box colors in tabset12 */
      #     #tabset12 a[data-value='Augmented Arcs'] {
      #         background-color: #337ab7; /* Bootstrap primary color for 'primary' */
      #         color: white;
      #     }
      #     #tabset12 a[data-value='Threshold & Arcs'] {
      #         background-color: #5cb85c; /* Bootstrap success color for 'success' */
      #         color: white;
      #     }
      #     #tabset12 a[data-value='Possible seed Arcs'] {
      #         background-color: #5bc0de; /* Bootstrap info color for 'info' */
      #         color: white;
      #     }
      # "))
      # ),
      #   # -----------
      #   tags$head(
      #     tags$style(HTML("
      #       #tabset10 a[data-value='Loss function table'] {
      #           background-color: #337ab7; /* Blue */
      #           color: white;
      #       }
      #       #tabset10 a[data-value='Number parent table'] {
      #           background-color: #5cb85c; /* Green */
      #           color: white;
      #       }
      #       #tabset10 a[data-value='BIC table'] {
      #           background-color: #5bc0de; /* Light Blue */
      #           color: white;
      #       }
      #       #tabset10 a[data-value='Min BIC table'] {
      #           background-color: #f0ad4e; /* Yellow */
      #           color: white;
      #       }
      #       #tabset10 a[data-value='Possible WhiteList'] {
      #           background-color: #d9534f; /* Red */
      #           color: white;
      #       }
      #   "))
      #   ),
      #   # -----------
      #   tags$head(
      #     tags$style(HTML("
      #   /* Matching the tab headers to box colors in tabset81 */
      #   #tabset81 a[data-value='WhiteList Check Acyclicity'] {
      #     background-color: #337ab7; /* Bootstrap primary color for 'primary' status */
      #     color: white;
      #   }
      #   #tabset81 a[data-value='Final Whitelist'] {
      #     background-color: #5bc0de; /* Bootstrap info color for 'info' status */
      #     color: white;
      #   }
      # "))
      #   ),
      #   # -----------
      #   tags$head(
      #     tags$style(HTML("
      #   /* Matching the tab headers to box colors in tabset3 */
      #   #tabset3 a[data-value='Algorithm Progression'] {
      #     background-color: #337ab7; /* Bootstrap primary color for 'primary' status */
      #     color: white;
      #   }
      #   #tabset3 a[data-value='List of Arc'] {
      #     background-color: #5bc0de; /* Bootstrap info color for 'info' status */
      #     color: white;
      #   }
      # "))
      #   ),
      #-----------
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
      # -----------
      tags$head(tags$style(HTML("
    .sweet-alert.alert-size-s {
      width: 600px !important; /* Increase  width as needed */
    }
    .swal-button--confirm {
      background-color: #3498db;
    }
  "))),
      # -----------
      tags$script(HTML("
$(document).on('click', '#goToTab', function() {
  $('a[data-value=\"WhiteList_Check_acyclicity\"]').click();
  // Close the shinyalert dialog
 swal.close();
 });
")),

      # -----------

      tags$style(HTML("
  #variable_distribution, #correlation_structure {
    overflow: hidden;
  }
  .shiny-plot-output {
    box-shadow: 0 0 8px rgba(0,0,0,0.15);
    border-radius: 5px;
  }
")),

      # -----------
      tags$head(
        tags$style(HTML("
    .alg-progression-panel .tab-content {
      background-color: #f2f2f2; /* Light grey background */
      color: #333; /* Dark grey text */
    }
    .list-of-arc-panel .tab-content {
      background-color: #e6e6fa; /* Lavender background */
      color: #333; /* Dark grey text */
    }
    /* Additional styling for tab headers if needed */
    #tabset1 a[data-value='alg_progression'] {
      background-color: #d1e8ff; /* Light blue for Algorithms Progression tab header */
    }
    #tabset1 a[data-value='list_of_arc'] {
      background-color: #ffccd5; /* Light red/pink for List of Arcs tab header */
    }
  "))
      ),
      # -----------
      # Add Font Awesome link here
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "https://use.fontawesome.com/releases/v5.15.1/css/all.css")
      ),
      # -----------
      # Initialize Bootstrap Tooltip
      tags$script(HTML("
      $(function () {
        $('[data-toggle=\"tooltip\"]').tooltip();
      });
    ")),
      # -----------------------------------------------------
      tabItems(
        tabItem(tabName = "user_guide",
                tags$iframe(style="height:1000px; width:100%", src="User_Guide.pdf")
        ),
        tabItem(tabName = "settings",
                fluidRow(
                  # ---------------------
                  # UI
                  box(
                    width = 4,
                    title = styled_title("Settings", bgcolor = "#4CAF50"),
                    solidHeader = TRUE,
                    status = "primary",
                    collapsible = TRUE,
                    # ----------------
                    fluidRow(
                      column(
                        5,
                        numericInput(inputId = "nboot",
                                     label = "Nboot",
                                     value = 10, min = 10, max = 1000000000000)
                      ),
                      column(
                        1,
                        div(
                          HTML("<i id='info_nboot' class='fas fa-question-circle' style='color:blue; cursor:pointer;'></i>"),
                          tags$script(
                            HTML("
          $(document).on('click', '#info_nboot', function(e) {
            e.stopPropagation();
            Shiny.setInputValue('show_nboot', Math.random());
          });
          ")
                          )
                        )
                      )
                    ),
                    # ----------------
                    fluidRow(
                      column(
                        5, #
                        numericInput(inputId = "threshold_level",
                                     label = "Threshold Level",
                                     value = 5)
                      ),
                      column(
                        1,
                        div(
                          HTML("<i id='info_threshold_level' class='fas fa-question-circle' style='color:blue; cursor:pointer;'></i>"),
                          tags$script(
                            HTML("
          $(document).on('click', '#info_threshold_level', function(e) {
            e.stopPropagation();
            Shiny.setInputValue('show_threshold_level', Math.random());
          });
          ")
                          )
                        )
                      )
                    )
                  ),
                  # ----------------
                  box(
                    width = 4,
                    title = styled_title("File Inputs", bgcolor = "#4CAF50"),
                    # style = "border: 1px solid #ccc; box-shadow: 2px 2px 8px #aaa;",  # Styling Box
                    solidHeader = TRUE,
                    status = "primary",
                    collapsible = TRUE,
                    # ----------------
                    fluidRow(
                      column(
                        9, #
                        uiOutput("dynamicFileInput")
                      ),
                      column(
                        1,
                        div(
                          HTML("<i id='info_dataFile' class='fas fa-question-circle' style='color:blue; cursor:pointer;'></i>"),
                          tags$script(
                            HTML("
          $(document).on('click', '#info_dataFile', function(e) {
            e.stopPropagation(); // Stop the event from propagating up to the fileInput
            Shiny.setInputValue('show_dataFile', Math.random());
          });
          ")
                          )
                        )
                      )
                    ),
                    # ----------------
                    fluidRow(
                      column(
                        9, #
                        fileInput(
                          inputId = "BlackListFile",
                          label = "BlackList",
                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                        )
                      ),
                      column(
                        1,
                        div(
                          HTML("<i id='info_BlackListFile' class='fas fa-question-circle' style='color:blue; cursor:pointer;'></i>"),
                          tags$script(
                            HTML("
          $(document).on('click', '#info_BlackListFile', function(e) {
            e.stopPropagation(); // Stop the event from propagating up to the fileInput
            Shiny.setInputValue('show_BlackListFile', Math.random());
          });
          ")
                          )
                        )
                      )
                    )
                    # ----------------
                  ),
                  # -----------------------------
                  box(
                    width = 4,
                    title = styled_title("Algorithm selection", bgcolor = "#4CAF50"),
                    solidHeader = TRUE,
                    status = "primary",
                    collapsible = TRUE,
                    # -----------------------------
                    fluidRow(
                      column(
                        5,  #
                        selectInput(
                          inputId = "algorithm_directed",
                          label = "Directed Algorithms",
                          choices = c("iamb", "iamb.fdr", "pc.stable", "hc", "tabu", "mmhc", "rsmax2", "gs"),
                          # choices = c("iamb", "iamb.fdr", "pc.stable", "hc", "tabu", "mmhc", "rsmax2", "gs", "DAGMA"),

                          selected = NULL,
                          multiple = TRUE
                        )
                      ),
                      column(
                        1,
                        div(HTML("<i id='info_algorithm_directed' class='fas fa-question-circle' style='color:blue; cursor:pointer;'></i>")),
                        tags$script(
                          HTML("
        $(document).on('click', '#info_algorithm_directed', function(e) {
        e.stopPropagation(); // Stop the event from propagating up to the selectInput
        Shiny.setInputValue('show_Dir_AlgoDescriptions', Math.random());
        });
        ")
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        5,  #
                        selectInput(
                          inputId = "algorithm_undirected",
                          label = "Undirected Algorithms",
                          choices = c("mmpc", "si.hiton.pc"), selected = NULL, multiple = TRUE
                        )
                      ),
                      column(
                        1,
                        div(HTML("<i id='info_algorithm_undirected' class='fas fa-question-circle' style='color:blue; cursor:pointer;'></i>")),
                        tags$script(
                          HTML("
        $(document).on('click', '#info_algorithm_undirected', function(e) {
        e.stopPropagation(); // Stop the event from propagating up to the selectInput
        Shiny.setInputValue('show_UnDir_AlgoDescriptions', Math.random());
        });
        ")
                        )
                      )
                    )
                  )
                ),
                br(),
                # -----------------------------
                fluidRow(
                  div(
                    style = "text-align: center;",
                    actionButton(inputId = "runButton",
                                 label = tags$span(icon("fas fa-hand-pointer", style="margin-right: 4px;"), "Run Discovery"),
                                 style = "background-color: #3498db; color: white; padding: 10px 20px; font-size: 18px; border-radius: 5px; cursor: pointer;")
                  )
                )
        ),
        # ------------------------------------------------------------------------
        tabItem(tabName = "data_characteristics_Distribution_Correlation_BlackList",
                h3("Data Structure"),
                tags$p(
                  style = "font-family: 'Arial'; font-size: 14px; color: #333;",
                  HTML(
                    "<ul>
      <li>This <code>Data Distribution</code> tab provides a visual analysis of the distributions of variables in your dataset. Each variable is plotted as a histogram with a superimposed density plot, allowing for an initial assessment of each variable's distribution characteristics. These plots help in identifying how closely the variables approximate a normal distribution, offering insights into potential skewness and kurtosis. The tab is especially useful during the exploratory data analysis phase, aiding in decisions regarding data transformation and normalization for subsequent statistical analysis.</li>
      <li>This <code>Correlation Structure</code> tab visually represents the correlation matrix of the dataset using a heatmap, which is crucial for understanding the relationships among variables. The heatmap aids in identifying how variables in your dataset are interrelated, which can inform the structure of a causal Bayesian network. The color palette, ranging from blue to red, effectively highlights negative to positive correlations, respectively. Features like checking for NA or Inf values ensure data integrity before visualization. This tool is indispensable during the exploratory data analysis phase, aiding in decisions regarding model structure and further statistical analysis.</li>
      <li>The <code>BlackList</code> plays a pivotal role in the <code>structural learning</code> and network inference processes within Bayesian networks, which are illustrated as Directed Acyclic Graphs (DAGs). This component is instrumental in molding the network structure, relying primarily on <code>prior knowledge </code> regarding the causal relationships amongst the data features. This list encompasses specific arcs, or <code>directed edges, </code> that are deliberately excluded from the proposed network, serving as constraints based on established or acknowledged information. This ensures the resultant structure and its causal interactions or arcs amongst nodes are in harmony with existing knowledge and logical constraints, thereby maintaining the integrity and accuracy of the network's representation of causal relationships.</li>
    </ul>")
                ),
                tabsetPanel(id = "tabset14",
                            tabPanel(
                              title = "Data Distribution",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 1200px; height: 900px; overflow: auto;",
                                    absolutePanel(top = 10, right = 10, style = "z-index: 500;",
                                                  downloadButton("downloadPlot_Data_Distribution", "Download", style = "color: black; background-color: lightblue;")),
                                    # style = "width: 100%; height: 600px; overflow: auto;",
                                    plotOutput("variable_distribution", height = "800px", width= "900px")
                                  )
                                )
                              )
                            ),

                            tabPanel(
                              title = "Significant Correlation",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 1200px; height: 900px; overflow: auto;",
                                    absolutePanel(top = 10, right = 10, style = "z-index: 500;",
                                                  downloadButton("downloadPlot_Significant_Correlation", "Download", style = "color: black; background-color: lightblue;")),
                                    # style = "width: 100%; height: 600px; overflow: auto;",
                                    plotOutput("correlation_structure_pi_sig", height = "800px", width= "900px")
                                  )
                                )
                              )
                            ),

                            tabPanel(
                              title = "Correlation Dendrograms",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 1200px; height: 900px; overflow: auto;",
                                    absolutePanel(top = 10, right = 10, style = "z-index: 500;",
                                                  downloadButton("downloadPlot_Correlation_Dendrograms", "Download", style = "color: black; background-color: lightblue;")),
                                    # style = "width: 100%; height: 600px; overflow: auto;",
                                    plotOutput("correlation_structure", height = "800px", width= "900px")
                                  )
                                )
                              )
                            ),
                            tabPanel(
                              title = "Correlation Significant",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 1200px; height: 900px; overflow: auto;",
                                    plotOutput("correlation_structure_sig", height = "800px", width= "900px")
                                  )
                                )
                              )
                            ),
                            tabPanel(
                              title = "Data characteristics",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 600px; height: 600px; overflow: auto;",
                                    # style = "width: 100%; height: 600px; overflow: auto;",
                                    verbatimTextOutput("viewDataStructure")
                                  )
                                )
                              )
                            ),
                            tabPanel(
                              title = "BlackList",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 600px; height: 600px; overflow: auto;",
                                    DTOutput("Black_List")
                                  )
                                )
                              )
                            )
                )
        ),
        # ------------------------------------------------------------------------
        tabItem(tabName = "directed_Undirected_algorithm_messages",
                h3("Learning process of directed & Undirected algorithms:"),
                tags$p(
                  style = "font-family: 'Arial'; font-size: 14px; color: #333;",
                  HTML(
                    "<ul>
      <li>The <code>Directed Algorithm</code> in <code>Bayesian network inference</code> is crucial for deducing the structure and causality in a network, represented as a Directed Acyclic Graph (<code>DAG</code>). Users employ an ensemble of different structural learning algorithms to accurately identify potential causal interactions or arcs among nodes and to estimate the <code>conditional probability distributions</code> from datasets. These algorithms contribute to constructing <code>blacklists</code> and/ or <code>whitelists</code> of arcs, serving as a basis for exclusion and inclusion based on <code>prior knowledge</code> and <code>arc strength</code>.</li>
      <li>Some algorithms have been utilized for the local discovery of undirected graphs to furnish additional evidence substantiating the existence of an edge between two nodes, in conjunction with the <code>Directed Algorithms</code>.</li>
      <li>The table displayed on the righ panel enumerates the arcs learnt through using various structural learning algorithms. The column labeled <code>Edge_No</code> corresponds to the number of the arcs as listed in the table.</li>
                  </ul>")
                ),

                tabsetPanel(id = "tabset3",
                            tabPanel(
                              title = "Algorithms Progression",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 600px; height: 600px; overflow: auto;",
                                    # style = "width: 100%; height: 600px; overflow: auto;",

                                    verbatimTextOutput("directed_Undirected_algorithm_messages")
                                  )
                                )
                              )
                            ),
                            tabPanel(
                              title = "List of Arcs",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 600px; height: 600px; overflow: auto;",
                                    # style = "width: 100%; height: 600px; overflow: auto;",

                                    DTOutput("all_edge_list")
                                  )
                                )
                              )
                            )
                )
        ),
        # ------------------------------------------------------------------------
        tabItem(tabName = "augmented_edge_list_thresh_cols_possible_seed_arcs_table",
                h3("Arcs and thier corresponding info in different algorithm "),
                tags$p(
                  style = "font-family: 'Arial'; font-size: 14px; color: #333;",
                  HTML(
                    "<ul>
                  <li>List of Arcs and thier corresponding arc strength and thier inhibitive or promotive impact/ correlation sign in different algorithm.</li>
                  <li>possible seed arcs filtered by the corresponding arc strength threshold</li>
                  </ul>"
                  )
                ),
                tabsetPanel(id = "tabset12",
                            tabPanel(
                              title = "Augmented Arcs",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 100%; height: 600px; overflow: auto;",
                                    DTOutput("augmented_edge_list")
                                  )
                                )
                              )
                            ),
                            tabPanel(
                              title = "Threshold & Arcs",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 100%; height: 600px; overflow: auto;",
                                    DTOutput("augmneted.thresh.cols")
                                  )
                                )
                              )
                            ),
                            tabPanel(
                              title = "Possible seed Arcs",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 100%; height: 600px; overflow: auto;",
                                    DTOutput("possible_seed_arcs_filter")
                                  )
                                )
                              )
                            )
                )
        ),
        # ------------------------------------------------------------------------
        tabItem(tabName = "L1_Parent_bic_table",
                h3("Model selection via threshold "),
                tags$p(
                  style = "font-family: 'Arial'; font-size: 14px; color: #333;",
                  HTML(
                    "<ul>
      <li>This step facilitates the inclusion of specific arcs in the network based on the strength of evidence supporting the existence of an arc or, alternatively, the arc strength. Arcs with strength below a predetermined threshold are incorporated in the consensus seed network or whitelist in a DAG. The threshold aids in balancing model complexity and regression accuracy, quantified using model selection criteriaa, the Bayesian Information Criterion (BIC).</li>
    </ul>"
                  )
                ),
                tabsetPanel(id = "tabset10",
                            tabPanel(
                              title = "Loss function table",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 100%; height: 600px; overflow: auto;",
                                    DTOutput("L1_merged")
                                  )
                                )
                              )
                            ),
                            tabPanel(
                              title = "Number parent table",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 100%; height: 600px; overflow: auto;",
                                    DTOutput("npar_merged")
                                  )
                                )
                              )
                            ),
                            tabPanel(
                              title = "BIC table",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 100%; height: 600px; overflow: auto;",
                                    DTOutput("BIC_merged_table")
                                  )
                                )
                              )
                            ),
                            tabPanel(
                              title = "Min BIC table",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 600px; height: 600px; overflow: auto;",
                                    DTOutput("bic_min_table")
                                  )
                                )
                              )
                            ),
                            tabPanel(
                              title = "Possible WhiteList",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 600px; height: 600px; overflow: auto;",
                                    DTOutput("possible.white.list")
                                  )
                                )
                              )
                            )
                )
        ),
        # ------------------------------------------------------------------------
        tabItem(tabName = "WhiteList_Check_acyclicity",
                h3("WhiteListed arcs"),
                tags$p(
                  style = "font-family: 'Arial'; font-size: 14px; color: #333;",
                  HTML(
                    "<ul>
      <li>Given the intrinsic nature of Directed Acyclic Graphs (DAGs), it is crucial to prevent the formation of cycles. To maintain the acyclic structure, it is necessary to carefully select whitelisted arcs. If a cycle is detected, the arcs responsible for creating the cycle must be removed. This can be done based on the user's domain knowledge or, if unspecified, through a random selection process. This ensures the integrity and functionality of the DAG within the application.</li>
    </ul>"
                  )
                ),
                tabsetPanel(id = "tabset81",
                            tabPanel(
                              title = "WhiteList Check Acyclicity",
                              box(
                                #title = "--",
                                #title = "WhiteList Check Acyclicity",
                                status = "primary",
                                # solidHeader = TRUE,
                                fluidRow(
                                  column(3,
                                         tags$div(
                                           uiOutput('cycleMessage'),
                                           style = "
                           width: 200px;  /* This line sets the width */
                           background: linear-gradient(to right, #3498db, #2980b9);
                           box-shadow: 0 1px 1px 0 rgba(0, 0, 0, 0.2);
                           color: white;
                           padding: 1px 1px;
                           font-size: 15px;
                           border-radius: 4px;
                           transition: all 0.3s ease-in-out;
                           font-family: 'Arial', sans-serif;
                           border: 1px solid #3498db;  /* This line adds a thicker border */
                           &:hover {
                           background: linear-gradient(to right, #2980b9, #3498db);
                           }  "
                                         ),

                                         uiOutput("checkboxUI"),

                                         fluidRow(
                                           uiOutput("removeButtonUI")
                                         )

                                  ),
                                  column(9,
                                         uiOutput("dynamicContent")
                                  )
                                ),
                                width = 12,
                                collapsible = TRUE
                              )
                            ),

                            tabPanel(
                              title = "Final Whitelist",
                              fluidRow(
                                column(
                                  6,
                                  div(
                                    style = "width: 600px; height: 600px; overflow: auto;",
                                    # style = "width: 100%; height: 600px; overflow: auto;",
                                    DTOutput("final_white_list")
                                  )
                                )
                              )
                            )
                )
        ),
        # ------------------------------------------------------------------------
        tabItem(tabName = "contour_plot",
                h3(HTML("Comparative Analysis of <span style='color: red;'>Pathological</span> and <span style='color: blue;'>Healthy</span> Data")),
                tags$p(
                  style = "font-family: 'Arial'; font-size: 14px; color: #333;",
                  HTML(
                    "<ul>
        <li>This visualization facilitates exploration of the <code>conditional probability queries</code> of
          Directed Acyclic Graphs (<code>DAG</code>) in relation to empirical data from both healthy and pathological
          conditions. Users can navigate through the dashboard, opting for a feature from an extensive list;
          the application then generates a visual portrayal of the chosen feature (illustrated on the <code>y-axis</code>)
          against a normalized significant feature (<code>x-axis</code>). In the resultant plot, samples derived from healthy
          tissues are denoted by  <span style='color: red;'><strong>red dots</strong></span>, whereas those from pathological
          tissues are designated by <span style='color: blue;'><strong>blue dots</strong></span>. Given that the pathological
          feature is a binary variable, the dashboard delineates different features with <span style='color: blue;'><strong>blue contours</strong></span>
          for instances where <span style='color: blue;'>condition &lt; 0.95</span> and <span style='color: red;'><strong>red contours</strong></span> for <span style='color: red;'>condition &gt; 0.05</strong></span>.</ul>"
                  )
                ),
                tabsetPanel(id = "tabset4",
                            fluidRow(
                              column(
                                12,
                                fluidRow(
                                  column(
                                    width = 3,
                                    selectInput("userSelected_Status", "Choose Status Feature:", choices = NULL),
                                    selectInput("userSelected_key_feature", "Choose Key Feature:", choices = NULL),
                                    selectInput("selectedCellType", "Select a secondary feature:", choices = NULL),
                                    fluidRow(
                                      column(
                                        width = 12,
                                        style = "display: flex; justify-content: center;", # Use flexbox to center the div
                                        actionButton("updateButton", "Update", icon = icon("sync"), class = "btn-primary", style = "color: white; background-color: #3498db; margin-top: 10px;")
                                      )
                                    )
                                  ),
                                  column(
                                    width = 6,
                                    absolutePanel(top = 10, right = 10, style = "z-index: 500;",  #
                                                  # absolutePanel(top = 10, right = 10, style = "z-index: 500; position: relative;",
                                                  downloadButton("downloadPlot_contour", "Download", style = "color: black; background-color: lightblue;")
                                    ),
                                    plotOutput("contour_plot", height = "600px")
                                    # plotOutput("contour_plot", height = "600px", width = "100%") #
                                  )
                                )
                              )
                            )
                )
        ),
        # ------------------------------------------------------------------------
        tabItem(tabName = "run_algorithms_plot",
                h3(HTML("Single Algorithm Network")),
                tags$p(
                  style = "font-family: 'Arial'; font-size: 14px; color: #333;",
                  HTML(
                    "<ul>
                <li>Plotting DAG network for each single algorithm such as
                Incremental association with false discovery rate control - <code>IAMB.FDR</code>,
                Practical constraint - <code>PC.STABLE</code>,
                Grow-shrink Markov Blanket - <code>GS</code>,
                Incremental association Markov Blanket - <code>IAMB</code>,
                Hill climbing - <code>HC</code>,
                Tabu search - <code>Tabu</code>,
                Max-min hill-climbing - <code>MMHC</code>,
                Restricted maximization - <code>RSMAX2</code>
                in comparison to what we learned by the BaMANI algorithm as an ensemble approach.
                </li>
            </ul>"
                  )
                ),
                tags$head(
                  tags$style(HTML("
            /* Matching the tab headers to box colors in singleAlg.DAG */
            #singleAlg.DAG a[data-value='Single Algorithm DAG network'] {
                background-color: #f39c12; /* Bootstrap warning color for 'warning' */
                color: white !important;
            }
            #singleAlg.DAG a[data-value='Single Algorithm DAG Summary'] {
                background-color: #d9534f; /* Bootstrap danger color for 'danger' */
                color: white !important;
            }
        "))
                ),
                tabsetPanel(id = "singleAlg.DAG",
                            tabPanel(
                              title = "Single Algorithm Run",
                              tabsetPanel(
                                tabPanel(
                                  title = "DAG Network",
                                  fluidRow(
                                    column(
                                      12,
                                      fluidRow(
                                        column(2,
                                               tags$div(
                                                 style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; margin-bottom: 20px;",
                                                 fluidRow(
                                                   column(12,
                                                          selectInput("alg_directed", "Select Directed Algorithm:", choices = c("iamb", "iamb.fdr", "pc.stable", "hc", "tabu", "mmhc", "rsmax2", "gs"))
                                                   )
                                                 ),
                                                 fluidRow(
                                                   column(12,
                                                          style = "display: flex; justify-content: center;",
                                                          actionButton("run", "Run Algorithm", style = "color: white; background-color: #3498db; margin-top: 10px;")
                                                   )
                                                 )
                                               )
                                        ),
                                        column(10,
                                               div(style = "position: relative;",
                                                   div(id = "plot_output_container", plotOutput("plot_output"), style = "width: 100%; height: 600px;"),  #
                                                   div(style = "position: absolute; top: 10px; right: 10px;",  # top-right
                                                       actionButton("downloadPlot_run", "Download", style = "color: #333; background-color: lightblue;")  #
                                                   )
                                               )
                                        )
                                      )
                                    )
                                  )
                                ),
                                tabPanel(
                                  title = "Network Summary",
                                  fluidRow(
                                    column(
                                      12,
                                      div(
                                        style = "width: 900px; height: 600px; overflow: auto;",
                                        dataTableOutput("DAG_detail")
                                      )
                                    )
                                  )
                                )
                              )
                            ),
                            tabPanel(
                              title = "BaMANI vs. Single Algorithm",
                              tabsetPanel(
                                tabPanel(
                                  title = "Comparison of Arcs",
                                  fluidRow(
                                    column(
                                      12,
                                      div(
                                        style = "position: relative; display: flex; justify-content: center; align-items: center; width: 900px; height: 600px; overflow: auto; padding-top: 20px;",
                                        plotOutput("bar_plot"),
                                        actionButton("downloadBarPlot", "Download", style = "position: absolute; top: 10px; right: 10px; z-index: 1000; color: #333; background-color: lightblue;")
                                      )
                                    )
                                  )
                                ),
                                tabPanel(
                                  title = "Analyzing Arc Strength",
                                  fluidRow(
                                    column(
                                      12,
                                      div(
                                        style = "position: relative; display: flex; justify-content: center; align-items: center; width: 1200px; height: auto; padding-top: 10px; padding-bottom: 10px;",
                                        plotOutput("scatter_plot"),
                                        actionButton("downloadScatterPlot", "Download", style = "position: absolute; top: 10px; right: 10px; z-index: 1000; color: #333; background-color: lightblue;")
                                      )
                                    )
                                  )
                                )
                              )
                            )
                )
        ),
        # ------------------------------------------------------------------------
        tabItem(tabName = "Algorithm_Count__Arc_Strength_diagnostic_plot",
                h3("Diagnostic Plot"),
                tags$p(
                  style = "font-family: 'Arial'; font-size: 14px; color: #333;",
                  HTML(
                    "<ul>
      <li>The plots below illustrate how the overall network connectivity (top) is influenced by
      the number of arcs included in the whitelist. A threshold value for edge strength (<code>x-axis</code>)
      is utilized to select arcs for inclusion in the whitelist (indicated by the <span style='color: blue;'><strong>blue</strong></span> curve),
      thereby determining the inferred DAG connectivity (represented by <span style='color: brown;'><strong>brown</strong></span> circles).
      Additionally, values for the Bayesian Information Criterion (<code>BIC</code>) were calculated for the entire <code>DAG</code>
      (bottom - <span style='color: green;'><strong>green</strong></span> circles) and for each node, given the inferred parents, as depicted by the <code>min BIC table</code>.</li>
      <li>Edges are organized based on the number of algorithms that identified that an edge was enriched (as depicted by the bar graph on the top)
      and the strength of the enrichment (bottom). The <code>arc Strength</code> representing the strength of enrichment, corresponds to the
      likelihood of a partial correlation between the two nodes of an arc being attributable to random chance, given the remainder
      of the network. The lines symbolizing the strength of enrichment represent the minimum (bottom: <span style='color: red;'><strong>red line</strong></span>) and maximum (bottom: <span style='color: blue;'><strong>blue line</strong></span>)
      values obtained by the different algorithms for each edge. The color of the bar graph signifies whether an edge was significantly
      enriched with a clear direction and included within the set of arcs identified at the minimum BIC (depicted in <span style='color: green;'><strong>green</strong></span>), significantly
      enriched without a clear direction but included within the set of arcs identified at the minimum BIC (depicted in <span style='color: brown;'><strong>brown</strong></span>),
      significantly enriched but without a clear direction (depicted in <span style='color: red;'><strong>red</strong></span>), or excluded from the consensus seed network list (depicted in <span style='color: blue;'><strong>blue</strong></span>).</li>
    </ul>"
                  )
                ),
                tabsetPanel(id = "tabset6",
                            tabPanel(
                              title = "Whitelist and BIC per threshold",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 900px; height: 500px; overflow: auto;",
                                    # style = "width: 100%; height: 500px; overflow: auto;",
                                    absolutePanel(top = 10, right = 10, style = "z-index: 500;",
                                                  downloadButton("downloadPlot_Diagnostic", "Download", style = "color: black; background-color: lightblue;")
                                    ),
                                    plotOutput("Diagnostic_plot")
                                  )
                                )
                              )
                            ),
                            tabPanel(
                              title = "Algorithm count",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 1200px; height: 600px; overflow: auto;",
                                    # style = "width: 100%; height: 500px; overflow: auto;",
                                    absolutePanel(top = 10, right = 10, style = "z-index: 500;",
                                                  downloadButton("downloadPlot_Algorithm.Count", "Download", style = "color: black; background-color: lightblue;")
                                    ),
                                    plotOutput("Plot.Algorithm.Count_arcs.strength")
                                  )
                                )
                              )
                            )
                )
        ),
        # ------------------------------------------------------------------------
        tabItem(tabName = "DAG_network_plot",
                h3("Causal DAG network"),
                tags$p(
                  style = "font-family: 'Arial'; font-size: 14px; color: #333;",
                  HTML(
                    "<ul>
      <li>The visualization presents a Directed Acyclic Graph (DAG) that represents the conditional probability distribution inferred
    by analyzing clinical data and features associated with the samples. Each node in the graph symbolizes a feature, sample attribute,
    or the prevalence of a specific feature or state, while the edges signify the inferred causal relationships between the nodes.
    A <span style='color: black;'><strong>black</strong></span> arrow denotes a <span style='color: black;'><strong>positive</strong></span> causal relationship, and a <span style='color: red;'><strong>red</strong></span> arrow indicates
    a <span style='color: red;'><strong>negative</strong></span> or inhibitory causal relation. The extent of influence of the parental node (Effect_Size) is annotated numerically adjacent to the edge,
    with the thickness of an edge (Arc_strength) being proportional to the posterior probability of its inclusion in the DAG.</li>
    </ul>"
                  )
                ),
                tabsetPanel(id = "tabset33",
                            tabPanel(
                              title = "BaMANI Dynamics Network",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 1400px; height: 700px; overflow: auto;",
                                    absolutePanel(top = 10, right = 10, style = "z-index: 500;",
                                                  downloadButton("downloadPlot_DAG_flow", "Download", style = "color: black; background-color: lightblue;")),
                                    visNetworkOutput("DAGNetworkPlot", width = "100%", height = "100%")
                                  )
                                )
                              )
                            ),
                            # --------------------------
                            tabPanel(
                              title = "BaMANI Network (parameters)",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 1400px; height: 700px; overflow: auto;",
                                    absolutePanel(top = 10, right = 10, style = "z-index: 500;",
                                                  downloadButton("downloadPlot_DAG_flow_intercept", "Download", style = "color: black; background-color: lightblue;")),
                                    visNetworkOutput("DAGNetworkPlot_intercept", width = "100%", height = "100%")
                                  )
                                )
                              )
                            ),
                            # --------------------------
                            tabPanel(
                              title = "BaMANI DAG Network",
                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 1100px; height: 700px; overflow: auto;",
                                    # style = "width: 1400px; height: 700px; overflow: auto;",
                                    absolutePanel(top = 10, right = 10, style = "z-index: 500;",
                                                  downloadButton("downloadPlot_DAG", "Download", style = "color: black; background-color: lightblue;")),
                                    plotOutput("DAG.Plot", width = "100%", height = "100%")
                                  )
                                )
                              )
                            ),
                            tabPanel(
                              title = "BaMANI Network Summary",
                              # title = "DAG summary Arc Slopes Strength Table",

                              fluidRow(
                                column(
                                  12,
                                  div(
                                    style = "width: 900px; height: 600px; overflow: auto;",
                                    DTOutput("arc_slopes_strength")
                                  )
                                )
                              )
                            )
                )
        )
        # ------------------------------------------------------------------------
      )
    )
  )
}
