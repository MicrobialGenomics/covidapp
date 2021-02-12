#' Title
#'
#' @param id identification
#'
#' @import ggplot2
#'
#' @export
per_ca_module_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::pageWithSidebar(
        headerPanel = shiny::headerPanel(""),

        # sidebar
        sidebarPanel = shiny::sidebarPanel(
            width = 2,
            shiny::uiOutput(outputId = ns("option_ca")),

            shinyWidgets::radioGroupButtons(
                inputId = ns("stack_p1"),
                label = shiny::h5("Pick y-axis transfomation:"),
                choices = c("stack" = "counts", "fill" = "freq"),
                checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square"),
                    no = tags$i(class = "fa fa-square-o")
                ),
                status = "default",
                selected = "counts",
                justified = TRUE
            ),
            shinyWidgets::radioGroupButtons(
                inputId = ns("bar_p1"),
                label = shiny::h5("Pick plot type:"),
                choices = c("bar", "density"),
                checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square"),
                    no = tags$i(class = "fa fa-square-o")
                ),
                status = "default",
                selected = "bar",
                justified = TRUE
            ),
            shiny::h5("Pick palette:"),
            shinyWidgets::sliderTextInput(
                inputId = ns("pal_p1"),
                label = NULL,
                selected = "mg",
                choices = c(
                    "mg",
                    RColorBrewer::brewer.pal.info %>%
                        dplyr::filter(category == "qual") %>%
                        rownames()
                )
            )
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
per_ca_module_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {

        df <- readr::read_rds("data/MergedData_spain.rds")
        com_aut <- df %>% dplyr::pull(acom_name) %>% unique() %>% sort() %>% c("all", .)

        output$option_ca <- shiny::renderUI({
            shinyWidgets::pickerInput(
                inputId = session$ns("option_ca"),
                label = shiny::h5("Autonumus Community:"),
                choices = c("all", df$acom_name %>% forcats::fct_infreq() %>% levels()),
                selected = c("all", df$acom_name %>% forcats::fct_infreq() %>% levels()),
                multiple = TRUE
            )
        })

        all_plots <- shiny::reactive({
            shiny::req(input$bar_p1)
            shiny::req(input$stack_p1)

            all_plots <- com_aut %>%
                purrr::set_names() %>%
                purrr::map(function(com) {
                    df %>% prepro_variants(ca = com) %>%
                        plot_vairants(
                            type = input$bar_p1,
                            var = input$stack_p1,
                            pal_dir = -1,
                            pal = input$pal_p1
                        )
                })
        })

        output$plots <- shiny::renderUI({
            lapply(input$option_ca, function(pp) {
                shiny::tagList(
                    shiny::column(
                        width = 6,
                        shiny::h4(stringr::str_c("C.A: ", pp)),
                        plotly::renderPlotly({ all_plots()[[pp]] })
                    )
                )
            })
        })

    })
}

