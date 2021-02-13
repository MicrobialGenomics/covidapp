#' Title
#'
#' @param id identification
#'
#' @import ggplot2
#'
#' @export
per_variant_module_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::pageWithSidebar(
        headerPanel = shiny::headerPanel(""),

        # sidebar
        sidebarPanel = shiny::sidebarPanel(
            width = 2,
            shiny::uiOutput(outputId = ns("option_clades")),
        ),

        # main
        mainPanel = shiny::mainPanel(
            width = 10,
            shiny::uiOutput(outputId = ns("plots"))
        )
    )
}

#' Title
#'
#' @param id identification
#'
#' @export
per_variant_module_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {

        df <- readr::read_rds("data/MergedData_spain.rds")
        clades <- df %>% dplyr::pull(NCClade) %>% unique() %>% sort()

        output$option_clades <- shiny::renderUI({
            shinyWidgets::pickerInput(
                inputId = session$ns("variant"),
                label = shiny::h5("Variants:"),
                choices = df$NCClade %>% forcats::fct_infreq() %>% levels(),
                selected = df$NCClade %>% forcats::fct_infreq() %>% levels(),
                multiple = TRUE
            )
        })

        all_plots <- clades %>%
            purrr::set_names() %>%
            purrr::map(function(var) {
                df %>% plot_variant_by_com(var)
            })

        output$plots <- shiny::renderUI({
            lapply(input$variant, function(pp) {
                shiny::tagList(
                    shiny::column(
                        width = 6,
                        shiny::h4(stringr::str_c("Variant: ", pp)),
                        plotly::renderPlotly({ all_plots[[pp]] })
                    )
                )
            })
        })
    })
}

