#' Title
#'
#' @param id identification
#'
#' @import ggplot2
#'
#' @export
plot_results_module_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::pageWithSidebar(
        headerPanel = shiny::headerPanel(""),

        # sidebar
        sidebarPanel = shiny::sidebarPanel(
            width = 2,

            ## Upload metadata

        ),

        # main
        mainPanel = shiny::mainPanel(
            width = 10,
            shiny::fluidRow(
                ## Plot 1
                shiny::column(
                    width = 6,
                    shiny::fixedRow(
                        shiny::column(
                            width = 5,
                            shiny::h4(glue::glue("SARS-CoV-2 Variants by Week")),
                            shiny::h6(glue::glue("{stringr::str_to_title(months(Sys.Date()))} {strftime(Sys.Date(), '%d')}th"))
                        ),
                        shiny::column(width = 5),
                        shiny::column(
                            width = 1,
                            shiny::br(),
                            shinyWidgets::dropdown(
                                shiny::h5("Pick y-axis transfomation:"),
                                shinyWidgets::switchInput(
                                    inputId = ns("stack_p1"),
                                    label = "type",
                                    onLabel = "stack",
                                    offLabel = "fill",
                                    size = "small",
                                    value = TRUE
                                ),
                                shiny::h5("Pick plot type:"),
                                shinyWidgets::switchInput(
                                    inputId = ns("bar_p1"),
                                    label = "geom_",
                                    onLabel = "bar",
                                    offLabel = "density",
                                    size = "small",
                                    value = TRUE
                                ),
                                shiny::h5("Pick palette:"),
                                shinyWidgets::sliderTextInput(
                                    inputId = ns("pal_p1"),
                                    label = NULL,
                                    choices = c(
                                        "mg",
                                        RColorBrewer::brewer.pal.info %>%
                                            dplyr::filter(category == "qual") %>%
                                            rownames
                                    ),
                                    selected = "mg"
                                ),
                                style = "stretch", icon = icon("gear"),
                                status = "primary", width = "300px",
                                animate = shinyWidgets::animateOptions(
                                    enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                                    exit = shinyWidgets::animations$fading_exits$fadeOutRight
                                )
                            )
                        )
                    ),
                    plotly::plotlyOutput(shiny::NS(id, "plot_1"), height = 400)
                ),

                ## Plot 2
                shiny::column(
                    width = 6,
                    shiny::fixedRow(
                        shiny::column(
                            width = 5,
                            shiny::h5(glue::glue("SARS-CoV-2 Variants by Week")),
                            shiny::h6(glue::glue("{stringr::str_to_title(months(Sys.Date()))} {strftime(Sys.Date(), '%d')}th"))
                        ),
                        shiny::column(width = 5),
                        shiny::column(
                            width = 1,
                            shiny::br(),
                            shinyWidgets::dropdown(
                                shiny::h5("Pick y-axis transfomation:"),
                                shinyWidgets::switchInput(
                                    inputId = ns("stack_p2"),
                                    label = "type",
                                    onLabel = "stack",
                                    offLabel = "fill",
                                    size = "small",
                                    value = TRUE
                                ),
                                style = "stretch", icon = icon("gear"),
                                status = "primary", width = "300px",right = T,
                                animate = shinyWidgets::animateOptions(
                                    enter = shinyWidgets::animations$fading_entrances$fadeInLeft,
                                    exit = shinyWidgets::animations$fading_exits$fadeOutRight
                                )
                            )
                        )
                    ),
                    plotly::plotlyOutput(shiny::NS(id, "plot_2"), height = 348)
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
plot_results_module_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {

        df <- readr::read_rds("data/MergedData_spain.rds")

        output$title_1 <- shiny::renderText({
            shiny::req(input$stack_p1)
            dplyr::if_else(isTRUE(input$stack_p1), "Counts", "Frequency")
        })


        ## Plot 1
        output$plot_1 <- plotly::renderPlotly({
            shiny::req(exists("df"))
            df %>%
                prepro_variants() %>%
                plot_vairants(
                    type = ifelse(isTRUE(input$bar_p1), "bar", "density"),
                    var = ifelse(isTRUE(input$stack_p1), "counts", "freq"),
                    pal_dir = -1,
                    pal = input$pal_p1
                )
        })

        ## Plot 1
        output$plot_2 <- plotly::renderPlotly({
            shiny::req(exists("df"))
            df %>% efforts_by_center(
                pos = ifelse(isTRUE(input$stack_p2), "stack", "fill")
            )
        })
    })
}

