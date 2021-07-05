#' Title
#'
#' @export
ui <- function() {
    shiny::shinyUI(
        shiny::navbarPage(
            title = shiny::div(
                shiny::img(
                    src = "images/covidTag.png",
                    height = 80,
                    width = 80,
                    style = "margin:-50px 5px"
                ),

                shiny::a(
                    "Enabled by data from",
                    style = "font-size: 14px; color: white",
                    href = "https://www.gisaid.org",
                    target = "_blank",
                    shiny::img(
                        src = "images/gisaid_2.png",
                        height = 20,
                        width = 50
                    )
                ),
                
                shiny::fixedRow(
                    shiny::a(
                        "Plot my data",
                        style = "font-size: 14px; color: white",
                        href = "http://covidtag.paseq.org:1989"
                    ),
                    shiny::a(
                        href = "https://github.com/MicrobialGenomics/covidapp",
                        target = "_blank",
                        shiny::img(
                            src = "images/github_2.png",
                            height = 20,
                            width = 20,
                            style = "filter: invert(1)"
                        )
                    ),
                    shiny::a(
                        href = "https://twitter.com/MicrobialGene",
                        target = "_blank",
                        shiny::img(
                            src = "images/twitter2.png",
                            height = 20,
                            width = 20,
                            style = "border-radius: 50%;"
                        )
                    ),
                    style = "right: 50px; top: 20px; position: absolute;"
                )
            ),
            theme = shinythemes::shinytheme("flatly"),
            fluid = TRUE,
            collapsible = TRUE,

            # TAB1: Overview
            shiny::tabPanel(
                title = "Overview",
                value = "tab_1",
                overview_module_ui("tab1"),
                shiny::br(),
                shiny::br(),
                footer
            ),

            # TAB2: Cat map
            shiny::tabPanel(
                title = "Sequencing efforts",
                value = "tab_2",
                map_module_ui("tab2")
            ),

            # TAB3: Per Autonomous Com
            shiny::tabPanel(
                title = "Per C.A.",
                value = "tab_3",
                per_ca_module_ui("tab3"),
                shiny::br(),
                shiny::br(),
                footer
            ),

            # TAB4: Acknowledgements
            shiny::tabPanel(
                title = "Acknowledgements",
                value = "tab_4",
                acknowledgements_module_ui("tab4"),
                shiny::br(),
                shiny::br(),
                footer
            ),
            shiny::tags$head(shiny::tags$style('
                       nav .container:first-child {
                           margin-left:100px; width: 100%;
                       }')),
            shinyjs::useShinyjs()
        )
    )
}

#' Title
#'
#' @export
server <- function() {
    function(input, output, session) {

        shinyjs::runjs('document.title = "CovidTag"')
        shinylogs::track_usage(storage_mode = shinylogs::store_rds("logs/"))

        ## Module tab 1
        overview_module_server("tab1")

        ## Module tab 2
        map_module_server("tab2")

        ## Module tab 3
        per_ca_module_server("tab3")
        
        ## Module tab 4
        acknowledgements_module_server("tab4")
        
        
    }
}

#' Title
#'
#' @export
ui_2 <- function() {
    shiny::shinyUI(
        shiny::navbarPage(
            title = shiny::div(
                shiny::img(
                    src = "images/covidTag.png",
                    height = 80,
                    width = 80,
                    style = "margin:-50px 5px"
                ),
                
                shiny::fixedRow(
                    shiny::a(
                        href = "https://github.com/MicrobialGenomics/covidapp",
                        target = "_blank",
                        shiny::img(
                            src = "images/github_2.png",
                            height = 20,
                            width = 20,
                            style = "filter: invert(1)"
                        )
                    ),
                    shiny::a(
                        href = "https://twitter.com/MicrobialGene",
                        target = "_blank",
                        shiny::img(
                            src = "images/twitter2.png",
                            height = 20,
                            width = 20,
                            style = "border-radius: 50%;"
                        )
                    ),
                    style = "right: 50px; top: 20px; position: absolute;"
                )
            ),
            theme = shinythemes::shinytheme("flatly"),
            fluid = TRUE,
            collapsible = TRUE,
            
            # TAB2: Overview
            shiny::tabPanel(
                title = "Overview",
                value = "tab_1",
                overview2_module_ui("tab1"),
                shiny::br(),
                shiny::br(),
                footer
            ),
            
            shiny::tags$head(shiny::tags$style('
                       nav .container:first-child {
                           margin-left:100px; width: 100%;
                       }')),
            shinyjs::useShinyjs()
        )
    )
}

#' Title
#'
#' @export
server_2 <- function() {
    function(input, output, session) {
        
        shinyjs::runjs('document.title = "CovidTag"')
        shinylogs::track_usage(storage_mode = shinylogs::store_rds("logs/"))
        
        ## Module tab 1
        overview2_module_server("tab1")
    }
}
