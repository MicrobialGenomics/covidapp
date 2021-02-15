#' Title
#'
#' @param id identification
#'
#' @export
data_dw_module_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::pageWithSidebar(
        headerPanel = shiny::headerPanel(""),

        # sidebar
        sidebarPanel = shiny::sidebarPanel(
            width = 2,

            # Select Columns
            shiny::downloadButton(
              outputId = ns("download"),
              label = "Download data file",
              icon = shiny::icon("table")
            )
        ),

        # main
        shiny::mainPanel(
          shiny::fixedRow(
            shiny::column(
              width = 12,
              DT::dataTableOutput(shiny::NS(id, "table_1"))
            )
          )
        )
    )
}

#' Title
#'
#' @param id identification
#'
#' @export
data_dw_module_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    df <- readr::read_rds("data/MergedData_spain.rds")

    # Print data.table with order and n samples
    output$table_1 <- DT::renderDataTable({
      df %>%
        DT::datatable(
          escape = FALSE,
          rownames = FALSE,
          extensions = c("Scroller"),
          options = list(
            deferRender = FALSE,
            scrollY = "70vh",
            scroller = TRUE
          )
        )
    })

    # Export designed run in .csv
    output$download <- shiny::downloadHandler(
      filename = glue::glue("covidapp_data.csv"),
      content = function(file) { readr::write_delim(df, file = file, delim = ";") }
    )

  })
}



