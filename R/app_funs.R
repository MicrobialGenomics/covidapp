#' Title
#'
#' @export
ui <- function() {
    shiny::shinyUI(
        shiny::navbarPage(
            title = shiny::div(shiny::div(
                id = "img-id",
                shiny::img(
                    src = "images/logo_irsicaixa.png",
                    height = '30',
                    width = '90'
                )
            ), "covidApp"),
            theme = shinythemes::shinytheme("flatly"),
            #footer = footer(),
            fluid = TRUE,
            collapsible = TRUE,

            # TAB1: Results
            shiny::tabPanel(
                title = "Results",
                value = "tab_1",
                plot_results_module_ui("tab1")
            ),

            # TAB2: Cat map
            shiny::tabPanel(
                title = "Sequencing efforts",
                value = "tab_2",
                cat_map_module_ui("tab2")
            ),

            # TAB3: Per Autonomous Com
            shiny::tabPanel(
                title = "Per C.A.",
                value = "tab_3",
                per_ca_module_ui("tab3")
            ),

            # TAB4: Per Variant
            shiny::tabPanel(
                title = "Per Variant",
                value = "tab_4",
                per_variant_module_ui("tab4")
            ),

            # TAB5: Data download
            shiny::tabPanel(
                title = "Data",
                value = "tab_5",
                data_dw_module_ui("tab5")
            )
        )
    )
}

#' Title
#'
#' @export
server <- function(){
    function(input, output, session) {

        ## Module tab 1
        plot_results_module_server("tab1")

        ## Module tab 2
        cat_map_module_server("tab2")

        ## Module tab 3
        per_ca_module_server("tab3")

        ## Module tab 4
        per_variant_module_server("tab4")

        ## Module tab 5
        data_dw_module_server("tab5")
    }
}

#' Title
#'
#' @export
run_shiny <- function() {
    shiny::runApp(list(ui = ui(), server = server()))
}
