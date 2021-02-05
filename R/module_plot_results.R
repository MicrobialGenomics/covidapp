#' Title
#'
#' @param id identification
#'
#' @import ggplot2
#'
#' @export
plot_results_module_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::pageWithSidebar(
        headerPanel = shiny::headerPanel(""),

        # sidebar
        sidebarPanel = shiny::sidebarPanel(
            width = 3,

            shiny::h4("Please click to update the data")
        ),

        # main
        mainPanel = shiny::mainPanel(
            width = 9,
            shiny::fixedRow(
                shiny::column(
                    width = 7,
                    shiny::h4("Genomic Sequencing Efforts of SARS-CoV-2"),
                    shiny::h6("On date, $date, a total of XXX Samples have been processed for genome sequencing. These efforts are distributed along different weeks, starting 2021."),
                    shiny::plotOutput(shiny::NS(id, "plot_1"), height = 300)
                ),
                shiny::column(
                    width = 5,
                    shiny::h4("Longitudinal evolution of variants"),
                    shiny::h6("Globally, variant composition of the studied sample population consists of"),
                    shiny::plotOutput(shiny::NS(id, "plot_2"), height = 300)
                ),
            ),
            shiny::br(),
            shiny::br(),
            shiny::fixedRow(
                shiny::column(
                    width = 6,
                    shiny::h4("Longitudinal evolution of variants"),
                    shiny::h6("Longitudinally, variant composition shows the following trends"),
                    shiny::plotOutput(shiny::NS(id, "plot_3"), height = 300)
                ),
                shiny::column(
                    width = 6,
                    shiny::h4("Longitudinal evolution of variants"),
                    shiny::h6("Longitudinally, variant composition shows the following trends"),
                    shiny::plotOutput(shiny::NS(id, "plot_4"), height = 300)
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
plot_results_module_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {

        #shiny::observe({
        NCCladeSpace <- c("20A", "20E", "19A", "20B", "20D", "501Y")
        xDim <- 100
        seqEffortDF <- tibble::tibble(
            collection_date = sample(seq(
                as.Date('2021/01/01'), as.Date('2021/03/31'), by = "day"
            ), xDim, replace = T),
            sample_id = stringi::stri_rand_strings(xDim, 10),
            library_id = stringi::stri_rand_strings(xDim, 10),
            QPass = wakefield::answer(xDim),
            RawReads = runif(xDim, 10000, 140000),
            PercCov = runif(xDim),
            DepthOfCoverage = runif(xDim, 0, 1000),
            NCClade = sample(NCCladeSpace, xDim, replace = T),
        ) %>%
            dplyr::mutate(
                PassReads = RawReads - runif(nrow(.), 1000, 4000),
                WeekNumber = strftime(collection_date, format = "%V")
            )
        #})

        output$plot_1 <- shiny::renderPlot({
            if (!is.null(seqEffortDF)) {
                seqEffortDF %>%
                    ggplot(aes(x = WeekNumber)) +
                    geom_bar(stat = "count") +
                    theme_light()
            }
        })

        output$plot_2 <- shiny::renderPlot({
            if (!is.null(seqEffortDF)) {
                seqEffortDF %>%
                    ggplot(aes(NCClade, fill = NCClade)) +
                    geom_bar() +
                    coord_polar() +
                    theme_light()
            }
        })

        output$plot_3 <- shiny::renderPlot({
            if (!is.null(seqEffortDF)) {
                seqEffortDF %>%
                    ggplot(aes(WeekNumber, fill = NCClade)) +
                    geom_bar(position = "fill", stat = "count") +
                    theme_light()
            }
        })

        output$plot_4 <- shiny::renderPlot({
            if (!is.null(seqEffortDF)) {
                seqEffortDF %>%
                    ggplot(aes(as.numeric(WeekNumber), fill = NCClade)) +
                    geom_area(position = "fill", stat = "count") +
                    theme_light()
            }
        })

    })
}

