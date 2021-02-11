#' Title
#'
#' @param id identification
#'
#' @export
cat_map_module_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::div(class = "outer",
               shiny::tags$head(shiny::includeCSS("www/style/map_style.css")),
               leaflet::leafletOutput(outputId = ns("mymap"), width = "100%",height = "100%"),
               shiny::absolutePanel(
                   id = "controls", class = "panel panel-default",
                   top = 75, left = 55, width = 250, fixed = TRUE,
                   draggable = TRUE, height = "auto",
                   shiny::span(shiny::tags$i(shiny::h6("Reported cases are ...")), style="color:#045a8d"),
                   shiny::h3(shiny::textOutput(outputId = ns("reactive_case_count")), align = "right"),
                   shiny::h4(shiny::textOutput(outputId = ns("reactive_death_count")), align = "right"),
                   shiny::h6(shiny::textOutput(outputId = ns("clean_date_reactive")), align = "right"),
                   shiny::h6(shiny::textOutput(outputId = ns("reactive_country_count")), align = "right"),
                   shiny::plotOutput(outputId = ns("epi_curve"), height = "130px", width = "100%"),
                   shiny::plotOutput(outputId = ns("cumulative_plot"), height = "130px", width = "100%"),

                   shinyWidgets::sliderTextInput(
                       inputId = ns("plot_date"),
                       label = shiny::h5("Select mapping date"),
                       choices = format(sample(seq(as.Date('2020/04/01'), as.Date('2021/01/01'), by="day"), 12), "%d %b %y"),
                       grid = FALSE,
                       animate = shiny::animationOptions(interval = 3000, loop = FALSE)
                   ),

                   shiny::absolutePanel(
                       id = "logo",
                       class = "card",
                       bottom = 20,
                       right = 60,
                       width = 80,
                       fixed = TRUE,
                       draggable = FALSE,
                       height = "auto",
                       shiny::tags$a(
                           href = 'https://www.irsicaixa.es',
                           shiny::tags$img(
                               src =  "logo_irsicaixa.png",
                               height = '50',
                               width = '120'
                           )
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
cat_map_module_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        catalunya_map$values <- sample(100:5000, length(catalunya_map$comarca))
        bins = c(100, 500, 1000, 2500, 5000, Inf)
        cv_pal <- leaflet::colorBin("Oranges", domain = catalunya_map$values, bins = bins)

        plot_map <- leaflet::leaflet(catalunya_map) %>%
            leaflet::addTiles() %>%
            leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
            leaflet::setView(lng = 2, lat = 41.75, zoom = 8) %>%
            leaflet::addLayersControl(
                position = "topright",
                overlayGroups = c("COVID (new)", "COVID (cumulative)"),
                options = leaflet::layersControlOptions(collapsed = FALSE)
            ) %>%
            leaflet::addPolygons(
                stroke = FALSE,
                smoothFactor = 0.3,
                fillOpacity = 0.6,
                fillColor = ~ cv_pal(values)
            ) %>%
            leaflet::addLegend(
                position = "topright",
                pal = cv_pal,
                values = ~ values,
                title = "<small>Deaths per million</small>"
            )

        output$mymap <- leaflet::renderLeaflet({
            plot_map
        })
    })
}

