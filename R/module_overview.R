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
        headerPanel = NULL,
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
            shiny::uiOutput(outputId = ns("region")),
            shinyWidgets::radioGroupButtons(
                inputId = ns("stack_p1"),
                label = shiny::h5("Pick y-axis transfomation: "),
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
                inputId = ns("var_annot"),
                label = shiny::h5("Pick Variant Annotation:  "),
                choices = c(
                    "NCClade" = "NCClade",
                    "Pangolin" = "pangolin_lineage",
                    "Mutation" = "mutation"
                ),
                checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square"),
                    no = tags$i(class = "fa fa-square-o")
                ),
                status = "default",
                selected = "pangolin_lineage",
                justified = TRUE
            ),
            shiny::uiOutput(outputId = ns("mutation_positions")),
            shiny::uiOutput(outputId = ns("option_clades")),
            shiny::uiOutput(outputId = ns("info"))
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
                    plotly::plotlyOutput(shiny::NS(id, "plot_1"), height = 400) %>%
                        shinycssloaders::withSpinner(type = 7, color = "#ABD9E9")
                ),

                ## Plot 2
                shiny::column(
                    width = 6,
                    shiny::h4(shiny::textOutput(outputId = ns("title_2_1")), align = "center"),
                    shiny::h6(shiny::textOutput(outputId = ns("title_2_2")), align = "center"),
                    plotly::plotlyOutput(shiny::NS(id, "plot_2"), height = 400) %>%
                        shinycssloaders::withSpinner(type = 7, color = "#ABD9E9")
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
                    plotly::plotlyOutput(shiny::NS(id, "plot_3"), height = 400) %>%
                        shinycssloaders::withSpinner(type = 7, color = "#ABD9E9")
                ),

                ## Plot 4
                shiny::column(
                    width = 6,
                    shiny::h4(shiny::textOutput(outputId = ns("title_4_1")), align = "center"),
                    shiny::h6(shiny::textOutput(outputId = ns("title_4_2")), align = "center"),
                    plotly::plotlyOutput(shiny::NS(id, "plot_4"), height = 400) %>%
                        shinycssloaders::withSpinner(type = 7, color = "#ABD9E9")
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

        output$mutation_positions <- shiny::renderUI({
            shiny::req(input$var_annot == "mutation")
            shinyWidgets::pickerInput(
                inputId = session$ns("mutation_positions"),
                label = shiny::h5("Pick Mutation Position:"),
                choices = mt_pos,
                selected = "E:71",
                multiple = FALSE,
                options = list(`live-search` = TRUE)
            )
        })

        ## Clades/Variants/Mutations UI
        clades <- shiny::reactive({
            if (input$var_annot == "NCClade") {
                clades <- dplyr::pull(df_over, NCClade) %>% forcats::fct_infreq() %>% levels()
            } else if (input$var_annot == "pangolin_lineage") {
                clades <- dplyr::pull(df_over, pangolin_lineage) %>%
                    forcats::fct_infreq() %>% levels() %>% .[1:11]
            } else if (input$var_annot == "mutation") {
                shiny::req(input$mutation_positions)
                clades <- mt %>%
                    option_mutation(input$mutation_positions)
            }
        }) %>%
            shiny::bindCache(input$var_annot, input$mutation_positions)

        output$option_clades <- shiny::renderUI({
            shinyWidgets::pickerInput(
                inputId = session$ns("variant"),
                label = shiny::h5("Pick Variant / Mutation:"),
                choices = clades(),
                selected = "B.1.1.7",
                multiple = FALSE,
                options = list(`live-search` = TRUE)
            )
        })

        ## Render Region options
        output$region <- shiny::renderUI({
            shiny::req(exists("df_over"))
            shinyWidgets::pickerInput(
                inputId = session$ns("region"),
                label = shiny::h5("Region for left and right plots"),
                choices = list(
                    "Left Plot" = c("Spain", df_over$acom_name %>% unique()),
                    "Right Plot" = stringr::str_c(c("Spain", df_over$acom_name %>% unique()), " ")
                ),
                multiple = TRUE,
                selected = c("Spain", "Catalunya "),
                options =  list("max-options-group" = 1, `live-search` = TRUE)
            )
        })

        # Titles ------------------------------------------------------------------
        output$title_1_1 <- shiny::renderText({
            annot <- dplyr::case_when(
                input$var_annot == "NCClade" ~ "Nextclade variants",
                input$var_annot == "pangolin_lineage" ~ "Pangolin variants",
                TRUE ~ "Mutations"
            )
            glue::glue("Average weekly {annot} {input$stack_p1} in {input$region[[1]]}")
        })

        output$title_1_2 <- shiny::renderText({
            glue::glue("Based on reported sample collection date")
        })

        output$title_2_1 <- shiny::renderText({
            annot <- dplyr::case_when(
                input$var_annot == "NCClade" ~ "Nextclade variants",
                input$var_annot == "pangolin_lineage" ~ "Pangolin variants",
                TRUE ~ "Mutations"
            )
            glue::glue("Average weekly {annot} {input$stack_p1} in {stringr::str_remove_all(input$region[[2]], ' $' )}")
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

        file <- "https://raw.github.com/cov-lineages/pango-designation/master/lineage_notes.txt"
        annot <- readr::read_tsv(file, col_types = readr::cols()) %>%
            dplyr::mutate(Description = stringr::str_remove_all(Description, "More information .*"))

        output$info <- shiny::renderUI({
            shiny::req(input$variant, input$var_annot == "pangolin_lineage")

            var_description <- annot %>%
                dplyr::filter(Lineage == input$variant) %>%
                dplyr::pull(Description)

            shiny::div(
                shiny::h5(var_description, style = "color: darkgray; text-align: justify; text-justify: inter-word;"),
                shiny::tags$a(
                    href = stringr::str_c("https://outbreak.info/situation-reports?pango=", input$variant),
                    shiny::h5("Click for more information!", style = "color: darkgray")
                )
            )
        })

        # Plots -------------------------------------------------------------------
        ## Plot 1
        output$plot_1 <- plotly::renderPlotly({
            shiny::req(exists("df_over"), input$region, input$var_annot, input$stack_p1, exists("mt"))
            if (input$var_annot == "mutation") {
                shiny::req(input$mutation_positions)
                pp <- mt %>%
                    plot_mutations_2(
                        region = input$region[[1]],
                        mut_pos = input$mutation_positions,
                        var = input$stack_p1
                    )
            } else {
                pp <- df_over %>%
                    prepro_variants(ca = input$region[[1]], var_anno = input$var_annot) %>%
                    plot_vairants(type = "bar", var = input$stack_p1)
            }
            pp
        }) %>%
            shiny::bindCache(input$region, input$var_annot, input$stack_p1, input$mutation_positions)

        ## Plot 2
        output$plot_2 <- plotly::renderPlotly({
            shiny::req(exists("df_over"), input$var_annot, input$stack_p1, input$region)

            if (input$var_annot == "mutation") {
                shiny::req(input$mutation_positions)
                pp <- mt %>%
                    plot_mutations_2(
                        region = stringr::str_remove_all(input$region[[2]], ' $'),
                        mut_pos = input$mutation_positions,
                        var = input$stack_p1
                    )
            } else {
                pp <- df_over %>%
                    prepro_variants(
                        ca = stringr::str_remove_all(input$region[[2]], ' $'),
                        var_anno = input$var_annot
                    ) %>%
                    plot_vairants(type = "bar", var = input$stack_p1)
            }
            pp
        }) %>%
            shiny::bindCache(input$region, input$var_annot, input$stack_p1, input$mutation_positions)

        ## Plot 3
        output$plot_3 <- plotly::renderPlotly({
            shiny::req(exists("df_over"),
                       input$var_annot,
                       input$region,
                       input$variant)

            if (input$var_annot == "mutation") {
                shiny::req(input$mutation_positions)
                pp <- plot_mutation_line(inp_list = mt,
                                         region = input$region[[1]],
                                         mut_pos = input$mutation_positions,
                                         mut = input$variant)
            } else {
                if (input$region[[1]] == "Spain") {
                    pp <- df_over %>%
                        plot_variant_line(variant = input$variant,
                                          var_col = input$var_annot)
                } else {
                    pp <- df_over %>%
                        dplyr::filter(acom_name == input$region[[1]]) %>%
                        plot_variant_line(variant = input$variant,
                                          var_col = input$var_annot)
                }
            }
            pp
        }) %>%
            shiny::bindCache(input$var_annot, input$region, input$variant)


        ## Plot 4
        output$plot_4 <- plotly::renderPlotly({
            shiny::req(exists("df_over"),
                       input$var_annot,
                       input$region,
                       input$variant)

            if (input$var_annot == "mutation") {
                shiny::req(input$mutation_positions)
                pp <- plot_mutation_line(
                    inp_list = mt,
                    region = stringr::str_remove_all(input$region[[2]], ' $'),
                    mut_pos = input$mutation_positions,
                    mut = input$variant
                )
            } else {
                if (stringr::str_remove_all(input$region[[2]], ' $' ) == "Spain") {
                    pp <- df_over %>%
                        plot_variant_line(variant = input$variant,
                                          var_col = input$var_annot)
                } else {
                    pp <- df_over %>%
                        dplyr::filter(acom_name == stringr::str_remove_all(input$region[[2]], ' $' )) %>%
                        plot_variant_line(variant = input$variant,
                                          var_col = input$var_annot)
                }
            }
            pp
        }) %>%
            shiny::bindCache(input$var_annot,
                             input$region,
                             input$variant,
                             input$mutation_positions)
    })
}

popup_help_text = shiny::fluidPage(
    shiny::fixedRow(
        shiny::h3("Overview",
                  align = "center",
                  style = "font-weight: bold; font-style: italic;"),
        shiny::fixedRow(
            shiny::h5(
                "This section allows a comparative exploration of the
                        evolution of the different variants of SARS-CoV-2 (CoV-19)
                        between autonomous communities. Specifically, the graphs
                        at the top give a more general view showing the total
                        number of sequences or the frequency (stack or fill
                        option respectively) of each variant/mutation over time
                        in intervals of weeks. On the other hand, the bottom
                        graphs show the evolution of the frequency of a single
                        variant/mutation selected using the “Pick
                        Variant/Mutation” panel.",
                style = "margin-left: 50px; margin-right: 50px; line-height: 25px; text-align: justify;"
            )
        ),
        shiny::fixedRow(
            shiny::h5(
                "For a correct interpretation it is necessary to take into account:",
                style = "margin-left: 50px; margin-right: 50px; line-height: 25px; text-align: justify;"
            )
        ),
        shiny::fixedRow(
            shiny::h5(
                "•	Sampling may not be uniform within the different
                        autonomous communities. Therefore, it is important not
                        to assume that the counts/frequencies shown are
                        necessarily representative of the region.",
                style = "margin-left: 100px; margin-right: 50px; line-height: 25px; text-align: justify;"
            ),
            shiny::h5(
                "• The charts used are based on the sample collection date,
                        and sequencing or GISAID submission date. As a result,
                        this date may differ from when the sample was processed
                        and sequenced, and when the data is released to the public.",
                style = "margin-left: 100px; margin-right: 50px; line-height: 25px; text-align: justify;"
            ),
            shiny::h5(
                "• In relation with the last consideration, last data
                        pints often has incomplete data and may change as more
                        sequences come in.",
                style = "margin-left: 100px; margin-right: 50px; line-height: 25px; text-align: justify;"
            ),
            shiny::h5(
                "•	The mutations that define SARS-CoV-2 (CoV-19) lineages
                        are updated every few days. Therefore, displayed data is
                        susceptible to change over time.",
                style = "margin-left: 100px; margin-right: 50px; line-height: 25px; text-align: justify;"
            )
        ),
        shiny::br(),
    )
)




