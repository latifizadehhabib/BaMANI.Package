#' Run the Shiny App
#'
#' @export
run_app <- function() {
  library(shiny)
  shiny::shinyApp(
    ui = BaMANI.Package::ui,
    server = BaMANI.Package::server
  )
}



