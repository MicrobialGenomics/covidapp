#' Title
#'
#' @param id identification
#'
#' @import ggplot2
#'
#' @export
overview_module_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::pageWithSidebar(
        headerPanel = shiny::headerPanel(""),

        # sidebar
        sidebarPanel = shiny::sidebarPanel(
            width = 3,

            shiny::uiOutput(outputId = ns("region")),
            shinyWidgets::radioGroupButtons(
                inputId = ns("var_annot"),
                label = shiny::h5("Pick Variant Annotation:"),
                choices = c("NCClade" = "NCClade", "Pangolin" = "pangolin_lineage"),
                checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square"),
                    no = tags$i(class = "fa fa-square-o")
                ),
                status = "default",
                selected = "pangolin_lineage",
                justified = TRUE
            ),
            shiny::uiOutput(outputId = ns("option_clades"))
        ),

        # main
        mainPanel = shiny::mainPanel(
            width = 9,
            shiny::fluidRow(
                ## Plot 1
                shiny::column(
                    width = 6,
                    shiny::h4(shiny::textOutput(outputId = ns("title_1_1")), align = "center"),
                    shiny::h6(shiny::textOutput(outputId = ns("title_1_2")), align = "center"),
                    plotly::plotlyOutput(shiny::NS(id, "plot_1"), height = 400)
                ),

                ## Plot 2
                shiny::column(
                    width = 6,
                    shiny::h4(shiny::textOutput(outputId = ns("title_2_1")), align = "center"),
                    shiny::h6(shiny::textOutput(outputId = ns("title_2_2")), align = "center"),
                    plotly::plotlyOutput(shiny::NS(id, "plot_2"), height = 400)
                )
            ),
            shiny::br(),
            shiny::br(),
            shiny::fluidRow(
                ## Plot 3
                shiny::column(
                    width = 6,
                    shiny::h4(shiny::textOutput(outputId = ns("title_3_1")), align = "center"),
                    shiny::h6(shiny::textOutput(outputId = ns("title_3_2")), align = "center"),
                    plotly::plotlyOutput(shiny::NS(id, "plot_3"), height = 400)
                ),

                ## Plot 4
                shiny::column(
                    width = 6,
                    shiny::h4(shiny::textOutput(outputId = ns("title_4_1")), align = "center"),
                    shiny::h6(shiny::textOutput(outputId = ns("title_4_2")), align = "center"),
                    plotly::plotlyOutput(shiny::NS(id, "plot_4"), height = 400)
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
overview_module_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {

        df <- readr::read_rds("data/MergedData_spain.rds") %>%
            dplyr::mutate(acom_name = stringr::str_replace_all(acom_name, "Cataluña", "Catalunya"))

        clades <- shiny::reactive({
            if (input$var_annot == "NCClade") {
                clades <- dplyr::pull(df, NCClade) %>% forcats::fct_infreq() %>% levels()
            } else if (input$var_annot == "pangolin_lineage") {
                clades <- dplyr::pull(df, pangolin_lineage) %>%
                    forcats::fct_infreq() %>%
                    levels() %>%
                    .[1:14]
            } else {

            }

        }) %>%
            shiny::bindCache(input$var_annot)


        output$option_clades <- shiny::renderUI({
            shinyWidgets::pickerInput(
                inputId = session$ns("variant"),
                label = shiny::h5("Pick Variant:"),
                choices = clades(),
                selected = "B.1.1.7",
                multiple = FALSE,
                options = list(`live-search` = TRUE)
            )
        })

        output$title_1 <- shiny::renderText({
            shiny::req(input$stack_p1)
            dplyr::if_else(isTRUE(input$stack_p1), "Counts", "Frequency")
        })

        ## Render Region options
        output$region <- shiny::renderUI({
            shiny::req(exists("df"))
            shinyWidgets::pickerInput(
                inputId = session$ns("region"),
                label = shiny::h5("Region for left plots"),
                choices = list(
                    "Left Plot" = c("Spain", df$acom_name %>% unique()),
                    "Right Plot" = stringr::str_c(c("Spain", df$acom_name %>% unique()), " ")
                ),
                multiple = TRUE,
                selected = c("Spain", "Catalunya "),
                options =  list("max-options-group" = 1, `live-search` = TRUE)
            )
        })

        # Titles ------------------------------------------------------------------
        output$title_1_1 <- shiny::renderText({
            annot <- ifelse(input$var_annot == "NCClade", "Nextclade", "Pangolin")
            glue::glue("Average weekly {annot} variants counts in {input$region[[1]]}")
        })
        output$title_1_2 <- shiny::renderText({
            glue::glue("Based on reported sample collection date")
        })
        output$title_2_1 <- shiny::renderText({
            annot <- ifelse(input$var_annot == "NCClade", "Nextclade", "Pangolin")
            glue::glue("Average weekly {annot} variants counts in {stringr::str_remove_all(input$region[[2]], ' $' )}")
        })
        output$title_2_2 <- shiny::renderText({
            glue::glue("Based on reported sample collection date")
        })
        output$title_3_1 <- shiny::renderText({
            glue::glue("Average weekly {input$variant} prevalece in {input$region[[1]]}")
        })
        output$title_3_2 <- shiny::renderText({
            glue::glue("Based on reported sample collection date")
        })
        output$title_4_1 <- shiny::renderText({
            glue::glue("Average weekly {input$variant} prevalece in {stringr::str_remove_all(input$region[[2]], ' $' )}")
        })
        output$title_4_2 <- shiny::renderText({
            glue::glue("Based on reported sample collection date")
        })


        # Plots -------------------------------------------------------------------
        ## Plot 1
        output$plot_1 <- plotly::renderPlotly({
            shiny::req(exists("df"), input$region, input$var_annot,)
            df %>%
                prepro_variants(ca = input$region[[1]], var_anno = input$var_annot) %>%
                plot_vairants(type = "bar", var = "counts", pal = "mg")
        }) %>%
            shiny::bindCache(input$region, input$var_annot)

        ## Plot 2
        output$plot_2 <- plotly::renderPlotly({
            shiny::req(exists("df"), stringr::str_remove_all(input$region[[2]], ' $' ), input$var_annot)
            df %>%
                prepro_variants(ca = stringr::str_remove_all(input$region[[2]], ' $' ), var_anno = input$var_annot) %>%
                plot_vairants(type = "bar", var = "counts", pal = "mg")
        }) %>%
            shiny::bindCache(input$region, input$var_annot)

        ## Plot 3
        output$plot_3 <- plotly::renderPlotly({
            shiny::req(exists("df"),
                       input$var_annot,
                       input$region,
                       input$variant)

            if (input$region[[1]] == "Spain") {
                pp <- df %>%
                    plot_variant_line(variant = input$variant,
                                      var_col = input$var_annot)
            } else {
                pp <- df %>%
                    dplyr::filter(acom_name == input$region[[1]]) %>%
                    plot_variant_line(variant = input$variant,
                                      var_col = input$var_annot)
            }

            pp
        }) %>%
            shiny::bindCache(input$var_annot, input$region, input$variant)


        ## Plot 4
        output$plot_4 <- plotly::renderPlotly({
            shiny::req(exists("df"),
                       input$var_annot,
                       input$region,
                       input$variant)

            if (stringr::str_remove_all(input$region[[2]], ' $' ) == "Spain") {
                pp <- df %>%
                    plot_variant_line(variant = input$variant,
                                      var_col = input$var_annot)
            } else {
                pp <- df %>%
                    dplyr::filter(acom_name == stringr::str_remove_all(input$region[[2]], ' $' )) %>%
                    plot_variant_line(variant = input$variant,
                                      var_col = input$var_annot)
            }

            pp
        }) %>%
            shiny::bindCache(input$var_annot, input$region, input$variant)
    })

}

