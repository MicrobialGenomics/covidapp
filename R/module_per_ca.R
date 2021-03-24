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
            width = 3,
            shiny::div(
                shinyWidgets::dropdownButton(
                    popup_help_text,
                    label = NULL,
                    size = "xs",
                    width = "1000px",
                    status = "warning",
                    icon = shiny::icon("question")
                ), style = "float:right"
            ),
            shiny::br(),
            shiny::uiOutput(outputId = ns("option_ca")),
            shinyWidgets::radioGroupButtons(
                inputId = ns("stack_p1"),
                label = shiny::h5("Pick y-axis transfomation:"),
                choices = c("stack" = "counts", "fill" = "freq"),
                status = "default",
                selected = "counts",
                justified = TRUE,
                checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square"),
                    no = tags$i(class = "fa fa-square-o")
                )
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
            ),
            shinyWidgets::radioGroupButtons(
                inputId = ns("var_annot"),
                label = shiny::h5("Pick Variant Annotation:"),
                choices = c("NCClade" = "NCClade", "Pangolin" = "pangolin_lineage"),
                status = "default",
                selected = "NCClade",
                justified = TRUE,
                checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square"),
                    no = tags$i(class = "fa fa-square-o")
                )
            )
        ),

        # main
        mainPanel = shiny::mainPanel(
            width = 9,
            shiny::uiOutput(outputId = ns("plots")) %>%
                shinycssloaders::withSpinner(type = 7, color = "#ABD9E9")
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

        # df <- readr::read_rds("data/MergedData_spain.rds") %>%
        #     dplyr::mutate(acom_name = stringr::str_replace_all(acom_name, "Cataluña", "Catalunya")) %>%
        #     dplyr::filter(!acom_name == "Spain")

        df <- df_over
        com_aut <- df %>% dplyr::pull(acom_name) %>% unique() %>% sort() %>% c("Spain", .)

        output$option_ca <- shiny::renderUI({
            shinyWidgets::pickerInput(
                inputId = session$ns("option_ca"),
                label = shiny::h5("Autonomous Community:"),
                choices = c("Spain", df$acom_name %>% forcats::fct_infreq() %>% levels()),
                selected = c("Spain", df$acom_name %>% forcats::fct_infreq() %>% levels()),
                multiple = TRUE
            )
        })

        all_plots <- shiny::reactive({
            # shiny::req(input$bar_p1)
            shiny::req(input$stack_p1, input$var_annot)
            all_plots <- com_aut %>%
                purrr::set_names() %>%
                purrr::map(function(com) {
                    df %>%
                        prepro_variants(ca = com, var_anno = input$var_annot) %>%
                        plot_vairants(
                            type = "bar", #input$bar_p1,
                            var = input$stack_p1,
                            pal_dir = -1,
                            pal = input$pal_p1
                        )
                })
        }) %>%
            shiny::bindCache(input$stack_p1, input$var_annot) #, input$bar_p1)

        output$plots <- shiny::renderUI({
            lapply(input$option_ca, function(pp) {
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
            shiny::bindCache(input$option_ca)

    })
}

