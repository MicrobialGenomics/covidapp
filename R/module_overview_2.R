#' UI module overview
#'
#' @param id module id
#' @export
overview2_module_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::pageWithSidebar(
        headerPanel = NULL,
        
        sidebarPanel = shiny::sidebarPanel(
            width = 3,
            
            ## Upload data
            shiny::h5(
                "Choose csv    (",
                shiny::a(
                    shiny::downloadLink(
                        outputId = ns("downloadData"), 
                        label = 'Download example file', 
                        style = "color: darkgray;"
                    )
                ), 
                ")",
                shiny::div(
                    shinyWidgets::dropdownButton(
                        modaldiag,
                        label = NULL,
                        size = "xs",
                        width = "1000px",
                        status = "warning",
                        icon = shiny::icon("question")
                    ),
                    style = "align: right; float: right"
                )
            ),
            shiny::fileInput(
                inputId = ns("target_load"),
                label = NULL,
                accept = c(".csv", ".txt", ".zip", ".tar.gz", ".tar", ".xls", ".xlsx"),
                placeholder = "No file selected",
                buttonLabel = "Browse...",
                width = 400
            ),
            shiny::br(),
            shiny::h5(
                "Region for left and right plots",
                shiny::div(
                    shinyWidgets::dropdownButton(
                        popup_help_text_2,
                        label = NULL,
                        size = "xs",
                        width = "1000px",
                        status = "warning",
                        icon = shiny::icon("question")
                    ),
                    style = "align: right; float: right"
                )
            ),
            shiny::uiOutput(outputId = ns("region")),
            tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: lightgray; border-color: lightgray}")),
            shiny::uiOutput(outputId = ns("plot_date")),
            shiny::br(),
            
            shiny::h5(
                "Pick Variant Annotation:  ",
                shiny::div(
                    shinyWidgets::dropdownButton(
                        popup_variant_description_2,
                        label = NULL,
                        size = "xs",
                        width = "1000px",
                        status = "warning",
                        icon = shiny::icon("question")
                    ),
                    style = "align: right; float: right"
                )
            ),
            shiny::uiOutput(outputId = ns("var_annot")),
            shiny::uiOutput(outputId = ns("mutation_positions")),
            shiny::uiOutput(outputId = ns("option_clades")),
            shiny::uiOutput(outputId = ns("info")), 
            
            shiny::br(), 
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
            
            ## Plots first row
            shiny::fluidRow(
                shiny::column(
                    width = 6,
                    shiny::h4(shiny::textOutput(outputId = ns("title_1_1")), align = "center"),
                    shiny::h6(shiny::textOutput(outputId = ns("title_1_2")), align = "center"),
                    plotly::plotlyOutput(ns("plot_1"), height = 400) %>%
                        shinycssloaders::withSpinner(type = 7, color = "#ABD9E9")
                ),
                shiny::column(
                    width = 6,
                    shiny::h4(shiny::textOutput(outputId = ns("title_2_1")), align = "center"),
                    shiny::h6(shiny::textOutput(outputId = ns("title_2_2")), align = "center"),
                    plotly::plotlyOutput(ns("plot_2"), height = 400) %>%
                        shinycssloaders::withSpinner(type = 7, color = "#ABD9E9")
                )
            ),
            shiny::br(),
            shiny::br(),
            
            ## Plots second row
            shiny::fluidRow(
                shiny::column(
                    width = 6,
                    shiny::h4(shiny::textOutput(outputId = ns("title_3_1")), align = "center"),
                    shiny::h6(shiny::textOutput(outputId = ns("title_3_2")), align = "center"),
                    plotly::plotlyOutput(ns("plot_3"), height = 400) %>%
                        shinycssloaders::withSpinner(type = 7, color = "#ABD9E9")
                ),
                shiny::column(
                    width = 6,
                    shiny::h4(shiny::textOutput(outputId = ns("title_4_1")), align = "center"),
                    shiny::h6(shiny::textOutput(outputId = ns("title_4_2")), align = "center"),
                    plotly::plotlyOutput(ns("plot_4"), height = 400) %>%
                        shinycssloaders::withSpinner(type = 7, color = "#ABD9E9")
                )
            )
        )
    )
}

#' Sever module overview
#'
#' @param id module id
#' @export
overview2_module_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        
        shiny::showModal(
            shiny::modalDialog(
                modaldiag,
                title = NULL,
                footer = NULL, 
                easyClose = TRUE,
                size = "l"
            )
        )
        
        output$downloadData <- shiny::downloadHandler(
            filename = function() { "test_data.csv" },
            content = function(file) {
                "data/example_data.csv.zip" %>% 
                    readr::read_delim(delim = ";") %>% 
                    readr::write_delim(file = file, delim = ";")
            }
        )
        
        df_over <- shiny::reactive({
            shiny::req(inFile <- input$target_load)
            if (is.null(inFile)) { return(NULL) }
            if (stringr::str_detect(inFile$datapath, "[.]csv|[.]txt")) {
                dat <- readr::read_delim(inFile$datapath, col_names = TRUE, delim = ";") %>% 
                    dplyr::mutate(
                        collection_date = as.Date(collection_date, format = "%Y-%m-%d"),
                        date = format(collection_date, "%y-%W"),
                        week = as.numeric(stringr::str_remove(date, ".*-")),
                        week_num = week,
                        year = as.numeric(stringr::str_remove(date, "-.*")),
                        date = lubridate::parse_date_time(paste0(year, "/", week, "/", 1), 'y/W/w')
                    )
            }
            
            if (stringr::str_detect(inFile$datapath, "[.]xls|[.]xlsx")) {
                dat <- readxl::read_excel(inFile$datapath) %>% 
                    dplyr::mutate(
                        collection_date = as.Date(collection_date, format = "%Y-%m-%d"),
                        date = format(collection_date, "%y-%W"),
                        week = as.numeric(stringr::str_remove(date, ".*-")),
                        week_num = week,
                        year = as.numeric(stringr::str_remove(date, "-.*")),
                        date = lubridate::parse_date_time(paste0(year, "/", week, "/", 1), 'y/W/w')
                    )
            }
            dat
        })
        
        # Slider UI rendering --------------------------------------------------
        ## Slider date
        output$plot_date <- shiny::renderUI({
            shiny::req(df_over())
            dates <- format(as.Date(sort(unique(df_over()$date))), "%d %b %y")
            shinyWidgets::sliderTextInput(
                inputId = session$ns("plot_date"),
                label = shiny::h5("Select date range"),
                choices = dates,
                selected = c(dates[1], dates[length(dates)]),
                grid = FALSE
            )
        })
        
        f_df <- shiny::reactive({
            shiny::req(input$plot_date, length(input$plot_date) == 2)
            format_date_min <- input$plot_date[[1]] %>% as.Date("%d %b %y")
            format_date_max <- input$plot_date[[2]] %>% as.Date("%d %b %y")
            
            if (format_date_min == format_date_max) {
                format_date_max <- format_date_max + 7
            }
            
            df_over() %>%
                dplyr::filter(date <= format_date_max & date >= format_date_min)
        })
        
        ## Region picker
        output$region <- shiny::renderUI({
            shiny::req(df_over())
            shinyWidgets::pickerInput(
                inputId = session$ns("region"),
                label = NULL,
                choices = list(
                    "Left Plot" = c("All", df_over()$acom_name %>% unique()),
                    "Right Plot" = stringr::str_c(c(
                        "All", df_over()$acom_name %>% unique()
                    ), " ")
                ),
                multiple = TRUE,
                selected = c(
                    "All", 
                    stringr::str_c(c(
                        "All", df_over()$acom_name %>% unique()
                    ), " ")[2]
                ),
                options =  list(
                    "max-options-group" = 1,
                    `live-search` = TRUE
                )
            )
        })
        
        output$var_annot <- shiny::renderUI({
            shiny::req(df_over())
            if (exists("mt")) {
                otp <- c(
                    "Pango lineages" = "pangolin_lineage",
                    "GISAID clades" = "GISAID_clade",
                    "Nextclade" = "NCClade",
                    "World Health Organization" = "who",
                    "Mutation" = "mutation"
                )
            } else {
                otp <- c(
                    "Pango lineages" = "pangolin_lineage",
                    "GISAID clades" = "GISAID_clade",
                    "Nextclade" = "NCClade",
                    "World Health Organization" = "who"
                )
            }
            
            shinyWidgets::pickerInput(
                inputId = session$ns("var_annot"),
                label = NULL,
                choices = otp[otp %in% names(df_over())], 
                selected = "pangolin_lineage"
            )
        })
        
        ## Annotation options
        clades <- shiny::reactive({
            if (input$var_annot == "NCClade") {
                clades <- f_df() %>%
                    dplyr::pull(NCClade) %>%
                    forcats::fct_infreq() %>%
                    levels()
            } else if (input$var_annot == "pangolin_lineage") {
                clades <- f_df() %>%
                    dplyr::pull(pangolin_lineage) %>%
                    forcats::fct_infreq() %>%
                    levels()
            } else if (input$var_annot == "mutation") {
                shiny::req(input$mutation_positions)
                clades <- mt %>% option_mutation(input$mutation_positions)
            } else if (input$var_annot == "GISAID_clade") {
                clades <- f_df() %>% 
                    dplyr::pull(GISAID_clade) %>% 
                    forcats::fct_infreq() %>% 
                    levels()
            } else if (input$var_annot == "who") {
                clades <- f_df() %>% 
                    dplyr::pull(who) %>% 
                    forcats::fct_infreq() %>% 
                    levels()
            }
        }) 
        
        ## Annotation picker
        output$option_clades <- shiny::renderUI({
            shiny::req(df_over())
            shinyWidgets::pickerInput(
                inputId = session$ns("variant"),
                label = shiny::h5("Pick Variant:"),
                choices = clades(),
                selected = "B.1.1.7",
                multiple = FALSE,
                options = list(`live-search` = TRUE)
            )
        })
        
        ## Mutation position picker
        output$mutation_positions <- shiny::renderUI({
            shiny::req(df_over())
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
        
        ## Pangolin variant information
        file <- "https://raw.github.com/cov-lineages/pango-designation/master/lineage_notes.txt"
        annot <- file %>%
            readr::read_tsv(col_types = readr::cols()) %>%
            dplyr::mutate(
                Description = stringr::str_remove_all(Description, " More information .*"),
                Description = dplyr::if_else(
                    condition = stringr::str_detect(Description, "[.]$"),
                    true = Description,
                    false = stringr::str_c(Description, ".")
                ),
                Description = stringr::str_c(Description, " *")
            )
        
        output$info <- shiny::renderUI({
            shiny::req(input$variant, input$var_annot == "pangolin_lineage")
            
            var_description <- dplyr::filter(annot, Lineage == input$variant) %>%
                dplyr::pull(Description)
            
            shiny::div(
                shiny::h5(
                    var_description,
                    style = "color: gray; text-align: justify; text-justify: inter-word;"
                ),
                shiny::tags$a(
                    href = stringr::str_c(
                        "https://outbreak.info/situation-reports?pango=",
                        input$variant
                    ),
                    shiny::h5("Click for more information!", style = "color: gray")
                ),
                shiny::br(),
                shiny::tags$a(
                    href = "https://cov-lineages.org/index.html",
                    shiny::h6(
                        "* Variant description from PANGO lineages",
                        style = "color: darkgray; text-align: justify; text-justify: inter-word;"
                    )
                )
                
            )
        })
        
        # Dynamic titles  ------------------------------------------------------
        output$title_1_1 <- shiny::renderText({
            annot <- dplyr::case_when(
                input$var_annot == "NCClade" ~ "Nextclade variants",
                input$var_annot == "pangolin_lineage" ~ "Pango variants",
                input$var_annot == "GISAID_clade" ~ "GISAID clades",
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
                input$var_annot == "GISAID_clade" ~ "GISAID clades",
                TRUE ~ "Mutations"
            )
            glue::glue("Average weekly {annot} {input$stack_p1} in {stringr::str_remove_all(input$region[[2]], ' $' )}")
        })
        
        output$title_2_2 <- shiny::renderText({
            glue::glue("Based on reported sample collection date")
        })
        
        output$title_3_1 <- shiny::renderText({
            glue::glue("Average weekly {input$variant} prevalence in {input$region[[1]]}")
        })
        
        output$title_3_2 <- shiny::renderText({
            glue::glue("Based on reported sample collection date")
        })
        
        output$title_4_1 <- shiny::renderText({
            glue::glue("Average weekly {input$variant} prevalence in {stringr::str_remove_all(input$region[[2]], ' $' )}")
        })
        
        output$title_4_2 <- shiny::renderText({
            glue::glue("Based on reported sample collection date")
        })
        
        # Plots ----------------------------------------------------------------
        ## Plot 1
        output$plot_1 <- plotly::renderPlotly({
            shiny::req(f_df(), input$region, input$var_annot, input$stack_p1)
            if (input$var_annot == "mutation") {
                shiny::req(input$mutation_positions)
                pp <- mt %>%
                    plot_mutations_2(
                        region = input$region[[1]],
                        mut_pos = input$mutation_positions,
                        var = input$stack_p1
                    )
            } else {
                pp <- f_df() %>%
                    prepro_variants(ca = input$region[[1]], var_anno = input$var_annot) %>%
                    plot_vairants(type = "bar", var = input$stack_p1)
            }
            pp
        }) 
        
        ## Plot 2
        output$plot_2 <- plotly::renderPlotly({
            shiny::req(f_df(), input$var_annot, input$stack_p1, input$region)
            if (input$var_annot == "mutation") {
                shiny::req(input$mutation_positions)
                pp <- mt %>%
                    plot_mutations_2(
                        region = stringr::str_remove_all(input$region[[2]], ' $'),
                        mut_pos = input$mutation_positions,
                        var = input$stack_p1
                    )
            } else {
                pp <- f_df() %>%
                    prepro_variants(
                        ca = stringr::str_remove_all(input$region[[2]], ' $'),
                        var_anno = input$var_annot
                    ) %>%
                    plot_vairants(type = "bar", var = input$stack_p1)
            }
            pp
        }) 
        
        ## Plot 3
        output$plot_3 <- plotly::renderPlotly({
            shiny::req(f_df(), input$var_annot, input$region, input$variant)
            if (input$var_annot == "mutation") {
                shiny::req(input$mutation_positions)
                pp <- mt %>%
                    plot_mutation_line(
                        region = input$region[[1]],
                        mut_pos = input$mutation_positions,
                        mut = input$variant
                    )
            } else {
                if (input$region[[1]] == "All") {
                    pp <- f_df() %>%
                        plot_variant_line(
                            variant = input$variant,
                            var_col = input$var_annot
                        )
                } else {
                    pp <- f_df() %>%
                        dplyr::filter(acom_name == input$region[[1]]) %>%
                        plot_variant_line(
                            variant = input$variant,
                            var_col = input$var_annot
                        )
                }
            }
            pp
        })
        
        ## Plot 4
        output$plot_4 <- plotly::renderPlotly({
            shiny::req(f_df(), input$var_annot, input$region, input$variant)
            
            if (input$var_annot == "mutation") {
                shiny::req(input$mutation_positions)
                pp <- mt %>%
                    plot_mutation_line(
                        region = stringr::str_remove_all(input$region[[2]], ' $'),
                        mut_pos = input$mutation_positions,
                        mut = input$variant
                    )
            } else {
                if (stringr::str_remove_all(input$region[[2]], ' $' ) == "All") {
                    pp <- f_df() %>%
                        plot_variant_line(
                            variant = input$variant,
                            var_col = input$var_annot
                        )
                } else {
                    pp <- f_df() %>%
                        dplyr::filter(acom_name == stringr::str_remove_all(input$region[[2]], ' $' )) %>%
                        plot_variant_line(
                            variant = input$variant,
                            var_col = input$var_annot
                        )
                }
            }
            pp
        }) 
    })
}

popup_help_text_2 <- shiny::fluidPage(
    shiny::fixedRow(
        shiny::h3("Overview",
                  align = "center",
                  style = "font-weight: bold; font-style: italic;"),
        shiny::fixedRow(
            shiny::h5(
                "This section allows a comparative exploration of the
                        evolution of the different variants of SARS-CoV-2 (CoV-19)
                        between regions. Specifically, the graphs
                        at the top give a more general view showing the total
                        number of sequences or the frequency (stack or fill
                        option respectively) of each variant over time
                        in intervals of weeks. On the other hand, the bottom
                        graphs show the evolution of the frequency of a single
                        variant selected using the “Pick Variant” panel.",
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
                "• In relation with the last consideration, last data
                        pints often has incomplete data and may change as more
                        sequences come in.",
                style = "margin-left: 100px; margin-right: 50px; line-height: 25px; text-align: justify;"
            )
        ),
        shiny::br(),
    )
)

popup_variant_description_2 <- shiny::fluidPage(
    shiny::fixedRow(
        shiny::h3("Variant Classification",
                  align = "center",
                  style = "font-weight: bold; font-style: italic;"),
        shiny::fixedRow(
            shiny::h5(
                "SARS-CoV-2 (hCoV-19) genome sequences obtained from samples are
                classified into groups according their similarity in terms of
                mutations or groups of mutations. In myData CovidTag you can use 
                four classification systems including the clades designed by 
                GISAID, NextClade, Pangolin and the World Health Organization 
                (WHO). For instance, the variant of interest 'VUI202012/01'
                first identified in the United Kindom in December 2020, is
                designated by GISAID as clade 'GRY', as 501Y.V1 by NextClade
                , as B.1.1.7 by Pango lineages and as 'Alpha' by WHO annotation.",
                style = "margin-left: 50px; margin-right: 50px; line-height: 25px; text-align: justify;"
            ),
            shiny::h5(
                "You can select any of the variants using the drop-down menu for
                a brief description and a link to access complete information.",
                style = "margin-left: 50px; margin-right: 50px; line-height: 25px; text-align: justify;"
            )
        )
    )
)

modaldiag <-  shiny::fluidPage(
    shiny::fluidRow(
        shiny::h3("MyData Overview",
                  align = "center",
                  style = "font-weight: bold; font-style: italic;"),
        shiny::hr(),
        shiny::h5(
            "Please upload your data through the Browse button. 
                            The file must be separated by semicolons and weigh 
                            less than 1 Mb, in case of greater weight please 
                            upload a compressed version of the file (.zip, .tar 
                            or .tar.gz). This file should contain the following 
                            columns:",
            style = "margin-left: 10px; line-height: 25px; text-align: justify;"
        )
    ),
    shiny::fluidRow(
        shiny::h5(
            " 1.-  acom_name: Name of the region from which the 
                            samples are collected",
            style = "margin-left: 100px; line-height: 25px; text-align: justify;"
        )
    ),
    shiny::fluidRow(
        shiny::h5(
            " 2.-  collection_date: Date of collection of the 
                            sample with the format year/month/day (2021/04/25)",
            style = "margin-left: 100px; line-height: 25px; text-align: justify;"
        )
    ),
    shiny::fluidRow(
        shiny::h5(
            "And at least one of the following columns:",
            style = "margin-left: 10px; line-height: 25px; text-align: justify;"
        )
    ),
    
    shiny::fluidRow(
        shiny::h5(
            " 3.-  NCClade: Nextclade variant annotation",
            style = "margin-left: 100px; line-height: 25px; text-align: justify;"
        )
    ),
    shiny::fluidRow(
        shiny::h5(
            " 4.-  pangolin_lineage: Pango annotation",
            style = "margin-left: 100px; line-height: 25px; text-align: justify;"
        )
    ),
    shiny::fluidRow(
        shiny::h5(
            " 5.-  GISAID_clade: Annotation in GISAID format",
            style = "margin-left: 100px; line-height: 25px; text-align: justify;"
        )
    ),
    shiny::fluidRow(
        shiny::h5(
            " 6.-  who: World Healt Organization variant annotation",
            style = "margin-left: 100px; line-height: 25px; text-align: justify;"
        )
    )
)

