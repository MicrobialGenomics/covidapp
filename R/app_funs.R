#' Title
#'
#' @export
ui <- function() {
    shiny::shinyUI(
        shiny::fluidPage(
            shinyjs::useShinyjs(),
            theme = shinythemes::shinytheme("slate"),
            shiny::navbarPage(
                title = shiny::div(shiny::div(
                    id = "img-id",
                    shiny::img(
                        src = "logo_irsicaixa.png",
                        height = '30',
                        width = '90'
                    )
                ), "covidApp"),
                id = "tabs",

                # TAB1: Results
                shiny::tabPanel(
                    title = "Results",
                    value = "tab_1",
                    plot_results_module_ui("tab1")
                ),

                # TAB2: Cat map
                shiny::tabPanel(
                    title = "Cat map",
                    value = "tab_2",
                    cat_map_module_ui("tab2")
                )
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
    }
}

#' Title
#'
#' @export
run_shiny <- function() {
    shiny::runApp(list(ui = ui(), server = server()))
}
