#' Title
#'
#' @export
ui <- function() {
    shiny::shinyUI(
        shiny::navbarPage(
            title = "covidApp",
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
            )
            # ,

            # # TAB4: Per Variant
            # shiny::tabPanel(
            #     title = "Per Variant",
            #     value = "tab_4",
            #     per_variant_module_ui("tab4"),
            #     shiny::br(),
            #     shiny::br(),
            #     footer
            # ),
            #
            # # TAB5: Data download
            # shiny::tabPanel(
            #     title = "Data",
            #     value = "tab_5",
            #     data_dw_module_ui("tab5"),
            #     shiny::br(),
            #     shiny::br(),
            #     footer
            # )
        )
    )
}

#' Title
#'
#' @export
server <- function() {
    function(input, output, session) {

        ## Module tab 1
        overview_module_server("tab1")

        ## Module tab 2
        map_module_server("tab2")

        ## Module tab 3
        per_ca_module_server("tab3")

        # ## Module tab 4
        # per_variant_module_server("tab4")
        #
        # ## Module tab 5
        # data_dw_module_server("tab5")
    }
}

#' Title
#'
#' @export
run_shiny <- function() {
    shiny::runApp(list(ui = ui(), server = server()))
}
