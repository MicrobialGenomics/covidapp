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
        headerPanel = shiny::headerPanel(
            title = shiny::fluidPage(
                shiny::fixedRow(
                    shiny::h1("Acknowledgements",
                              align = "center",
                              style = "font-family: 'Brush Script MT';"),
                ),
                shiny::fixedRow(
                    shiny::h5(
                        "We are grateful to the data contributors who shared the data
                        used in this Web Application via the GISAID Initia-tive*: the
                        Authors, the Originating Laboratories responsible for
                        obtaining the specimens, and the Submitting Laboratories that
                        generated the genetic sequences and metadata.",
                        align = "center",
                        style = "margin-left: 150px; margin-right: 150px; line-height: 25px;"
                    )
                ),
                shiny::fixedRow(
                    shiny::h5(
                        "* (a) Elbe, S., and Buckland-Merrett, G. (2017) Data, disease
                        and diplomacy: GISAID’s innovative contribution to global health.
                        Global Challenges, 1:33-46. DOI: 10.1002/gch2.1018 and / or
                        (b) Shu, Y., McCauley, J. (2017) GISAID: From vision to reality.
                        EuroSurveillance, 22(13) DOI: 10.2807/1560-7917.ES.2017.22.13.30494.",
                        align = "center",
                        style = "margin-left: 150px; margin-right: 150px; line-height: 25px;"
                    )
                )
            )
        ),

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
