renderStyledTable <- function(table_name, rownames = TRUE, download_version = c()) {
  # Render a styled DataTable with enhanced features and customization options

  renderDT({
    datatable(
      table_name,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        dom = 'Bfrtip',  # Define elements in the table control layout
        buttons = download_version,  # Customizable download buttons
        pageLength = 10,  # Number of rows per page
        autoWidth = TRUE,  # Automatic column width
        scrollY = '400px',  # Vertical scrolling
        scrollCollapse = TRUE,  # Allow row height to shrink to fit data
        scroller = list(loadingIndicator = TRUE)  # Enable loading indicator for large datasets
      ),
      rownames = rownames,
      class = 'cell-border stripe hover'  # Table styling classes
    ) %>%
      formatStyle(
        columns = names(table_name),
        backgroundColor = styleEqual(c(NA, 1), c("white", "#f7f9f9")),
        color = 'black',
        fontSize = '14px',
        fontWeight = styleEqual(c(NA, 1), c("normal", "bold")),
        lineHeight = '20px',  # Improved line height for better readability
        textAlign = 'center'
      )
  }, server = FALSE)  # Disable server-side processing for responsive interaction
}
