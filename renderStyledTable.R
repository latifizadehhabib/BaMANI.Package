renderStyledTable <- function(table_name, rownames = TRUE, download_version = c(), scrollY = '800px') {
  set.seed(2023)
  
  renderDT({
    datatable(
      table_name,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        dom = 'Bfrtip',
        buttons = download_version,
        pageLength = 25,
        lengthMenu = list(c(10, 25, 50, 100, -1), c('10 rows', '25 rows', '50 rows', '100 rows', 'All')),
        autoWidth = TRUE,
        scrollY = scrollY,
        # scrollY = '700px',
        scrollX = TRUE,
        scrollCollapse = TRUE,
        scroller = list(loadingIndicator = TRUE),
        fixedHeader = TRUE,
        # ------------- 
        headerCallback = JS(
          "function(thead, data, start, end, display) {",
          "  $(thead).find('th').css('text-align', 'center');",
          "}"
        )
        # ------------- 
      ),
      rownames = rownames,
      # class = 'cell-border stripe hover'
      class = 'display nowrap order-column'
    ) %>%
      formatStyle(
        columns = names(table_name),
        backgroundColor = styleEqual(c(NA, 1), c("white", "#f7f9f9")),
        color = 'black',
        fontSize = '14px',
        fontWeight = styleEqual(c(NA, 1), c("normal", "bold")),
        lineHeight = '20px',
        textAlign = 'center'
      )
  }, server = FALSE)
}
