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
                    shiny::h3(
                        "Notes for a correct interpretation",
                        align = "center",
                        style = "font-weight: bold; font-style: italic;"
                    )
                ),
                shiny::fixedRow(
                    shiny::h5(
                        "•	Sampling may not be uniform within the different
                        autonomous communities. Therefore, it is important not
                        to assume that the counts/frequencies shown are
                        necessarily representative of the region.",
                        align = "center",
                        style = "margin-left: 70px; margin-right: 70px; line-height: 25px;"
                    ),
                    shiny::h5(
                        "• The charts used are based on the sample collection date,
                        and sequencing or GISAID submission date. As a result,
                        this date may differ from when the sample was processed
                        and sequenced, and when the data is released to the public.",
                        align = "center",
                        style = "margin-left: 70px; margin-right: 70px; line-height: 25px;"
                    ),
                    shiny::h5(
                        "• In relation with the last consideration, last data
                        pints often has incomplete data and may change as more
                        sequences come in.",
                        align = "center",
                        style = "margin-left: 70px; margin-right: 70px; line-height: 25px;"
                    ),
                    shiny::h5(
                        "•	The mutations that define SARS-CoV-2 (CoV-19) lineages
                        are updated every few days. Therefore, displayed data is
                        susceptible to change over time.",
                        align = "center",
                        style = "margin-left: 70px; margin-right: 70px; line-height: 25px;"
                    )
                ),
                shiny::br(),
                shiny::fixedRow(
                    shiny::h3("Acknowledgements",
                              align = "center",
                              style = "font-weight: bold; font-style: italic;"),
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

