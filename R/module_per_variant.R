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
            width = 3,
            shinyWidgets::radioGroupButtons(
                inputId = ns("var_annot"),
                label = shiny::h5("Pick Variant Annotation:"),
                choices = c("NCClade" = "NCClade", "Pangolin" = "pangolin_lineage"),
                checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square"),
                    no = tags$i(class = "fa fa-square-o")
                ),
                status = "default",
                selected = "NCClade",
                justified = TRUE
            ),
            shiny::uiOutput(outputId = ns("option_clades")),
        ),

        # main
        mainPanel = shiny::mainPanel(
            width = 9,
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

        df <- readr::read_rds("data/MergedData_spain.rds") %>%
            dplyr::mutate(acom_name = stringr::str_replace_all(acom_name, "Cataluña", "Catalunya"))

        clades <- shiny::reactive({
            if (input$var_annot == "NCClade") {
                clades <- dplyr::pull(df, NCClade) %>% forcats::fct_infreq() %>% levels()
            } else {
                clades <- dplyr::pull(df, pangolin_lineage) %>%
                    forcats::fct_infreq() %>%
                    levels() %>%
                    .[1:14]
            }
        }) %>%
            shiny::bindCache(input$var_annot)


        output$option_clades <- shiny::renderUI({
            shinyWidgets::pickerInput(
                inputId = session$ns("variant"),
                label = shiny::h5("Variants:"),
                choices = clades(),
                selected = clades(),
                multiple = TRUE,
            )
        })

        all_plots <- shiny::reactive({
            clades() %>%
                purrr::set_names() %>%
                purrr::map(function(var) {
                    df %>% plot_variant_by_com(var, input$var_annot)
                })
        }) %>%
            shiny::bindCache(input$var_annot, clades())

        output$plots <- shiny::renderUI({
            lapply(input$variant, function(pp) {
                shiny::tagList(
                    shiny::column(
                        width = 6,
                        shiny::h4(stringr::str_c(pp), align = "center"),
                        shiny::hr(),
                        plotly::renderPlotly({ all_plots()[[pp]] }),
                        shiny::br()
                    )
                )
            })
        }) %>%
            shiny::bindCache(input$variant)
    })
}

