#' UI module per C.A.
#'
#' @param id module id
#' @export
per_ca_module_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::pageWithSidebar(
        headerPanel = shiny::headerPanel(""),
        sidebarPanel = shiny::sidebarPanel(
            width = 3,
            shiny::h5(
                "Autonomous Community:",
                shiny::div(
                    shinyWidgets::dropdownButton(
                        popup_help_text,
                        label = NULL,
                        size = "xs",
                        width = "1000px",
                        status = "warning",
                        icon = shiny::icon("question")
                    ), style = "float:right"
                )
            ),
            shiny::uiOutput(outputId = ns("option_ca")),
            
            shiny::h5(
                "Pick Variant Annotation:  ",
                shiny::div(
                    shinyWidgets::dropdownButton(
                        popup_variant_description,
                        label = NULL,
                        size = "xs",
                        width = "1000px",
                        status = "warning",
                        icon = shiny::icon("question")
                    ),
                    style = "align: right; float: right"
                )
            ),
            shinyWidgets::pickerInput(
                inputId = ns("var_annot"),
                label = NULL,
                choices = c(
                    "Pango lineages" = "pangolin_lineage",
                    "GISAID clades" = "GISAID_clade",
                    "Nextclade" = "NCClade"
                ), 
                selected = "pangolin_lineage"
            ), 
            
            shiny::h5("Pick y-axis transfomation: "),
            shinyWidgets::prettyRadioButtons(
                inputId = ns("stack_p1"),
                label = NULL,
                choices = c("Counts" = "counts", "Percentages" = "freq"),
                icon = icon("check"), 
                selected = "counts",
                inline = TRUE, 
                status = "default",
                animation = "jelly"
            )
        ),

        mainPanel = shiny::mainPanel(
            width = 9,
            shiny::uiOutput(outputId = ns("plots")) %>%
                shinycssloaders::withSpinner(type = 7, color = "#ABD9E9")
        )
    )
}

#' Server module per C.A.
#'
#' @param id identification
#'
#' @export
per_ca_module_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {

        output$option_ca <- shiny::renderUI({
            shinyWidgets::pickerInput(
                inputId = session$ns("option_ca"),
                label = NULL,
                choices = c(
                    "Spain",
                    df_ca$acom_name %>% forcats::fct_infreq() %>% levels()
                ),
                selected = c(
                    "Spain",
                    df_ca$acom_name %>% forcats::fct_infreq() %>% levels()
                ),
                multiple = TRUE
            )
        })

        plots_to_plot <- shiny::reactive({
            shiny::req(input$stack_p1, input$var_annot)
            all_plots[[paste0(input$stack_p1)]][[paste0(input$var_annot)]]
        })
        
        output$plots <- shiny::renderUI({
            lapply(input$option_ca, function(pp) {
                shiny::tagList(
                    shiny::column(
                        width = 6,
                        shiny::h4(stringr::str_c(pp), align = "center"),
                        shiny::hr(),
                        plotly::renderPlotly({ plots_to_plot()[[pp]] }),
                        shiny::br()
                    )
                )
            })
        })
    })
}

