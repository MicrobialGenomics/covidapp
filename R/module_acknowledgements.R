#' Title
#'
#' @param id identification
#'
#' @import ggplot2
#'
#' @export
acknowledgements_module_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::pageWithSidebar(
        headerPanel = shiny::headerPanel(title = shiny::fluidPage(
            shiny::fixedRow(
                shiny::h1("Acknowledgements",
                          align = "center",
                          style = "font-family: 'Brush Script MT';"),
            ),
            shiny::fixedRow(
                shiny::h5(
                    "We gratefully acknowledge all data contributors, i.e.
                    the Authors and their Originating laboratories responsible
                    for obtaining the specimens, and their Submitting laboratories
                    for generating the genetic sequence and metadata and sharing
                    via the GISAID Initiative (1), on which this research is based.",
                    align = "center",
                    style = "margin-left: 150px; margin-right: 150px; line-height: 25px;"
                )
            ),
            shiny::fixedRow(
                shiny::h5(
                    "(1) Elbe, S., and Buckland-Merrett, G. (2017) Data, disease
                    and diplomacy: GISAID’s innovative contribution to global
                    health. Global Challenges, 1:33-46.
                    DOI: 10.1002/gch2.1018PMCID: 31565258",
                    align = "center",
                    style = "margin-left: 150px; margin-right: 150px; line-height: 25px;"
                )
            )
        )),



        # "),

        # sidebar
        sidebarPanel = NULL,

        # main
        mainPanel = NULL
    )
}

#' Title
#'
#' @param id identification
#'
#' @export
acknowledgements_module_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {


    })

}

