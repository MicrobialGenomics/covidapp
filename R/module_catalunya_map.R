#' Title
#'
#' @param id identification
#'
#' @export
cat_map_module_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::div(
        class = "outer",
        shiny::tags$head(shiny::includeCSS("www/style/map_style.css")),

        ## Map
        leaflet::leafletOutput(outputId = ns("mymap"), width = "100%",height = "100%"),

        ## Absolute panel
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
                        src =  "images/logo_irsicaixa.png",
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

        map <- ca_spain_gj

        df <- readr::read_rds("data/MergedData_spain.rds") %>%
            dplyr::mutate(acom_name = factor(
                acom_name,
                c(unique(acom_name), "Territorio no asociado a ninguna autonomía")
            ))

        map$cases <- df %>%
            dplyr::count(acom_name, .drop = FALSE) %>%
            dplyr::left_join(
                x = tibble::tibble(acom_name = map$acom_name),
                y = .,
                by = "acom_name"
            ) %>%
            dplyr::pull(n)


        p <- map$acom_name %>%
            purrr::set_names() %>%
            purrr::map(function(x) { plot_pie(x, df) })


        bins = c(seq(0, 2000, by = 250))
        cv_pal <- leaflet::colorBin("Oranges", domain = map$cases, bins = bins)

        plot_map <- leaflet::leaflet(map) %>%
            leaflet::addTiles() %>%
            leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
            leaflet::setView(lng = -4, lat = 40, zoom = 6) %>%
            leaflet::addLayersControl(
                position = "topright",
                overlayGroups = c("COVID (new)", "COVID (cumulative)"),
                options = leaflet::layersControlOptions(collapsed = FALSE)
            ) %>%
            leaflet::addPolygons(
                stroke = FALSE,
                smoothFactor = 0.3,
                fillOpacity = 0.6,
                fillColor = ~ cv_pal(cases)
            ) %>%
            leaflet::addLegend(
                position = "bottomleft",
                pal = cv_pal,
                values = ~ cases,
                title = "<small>Sequenced cases</small>"
            ) %>%
            leaflet::addPolygons(
                stroke = FALSE,
                fillOpacity = 0,
                fillColor = "transparent",
                popup = leafpop::popupGraph(p)
            )

        output$mymap <- leaflet::renderLeaflet({
            plot_map
        })


    })
}


plot_pie <- function(com, df) {
    p <- df %>%
        dplyr::filter(acom_name == com) %>%
        dplyr::mutate(NCClade = forcats::fct_infreq(NCClade)) %>%
        ggplot(aes(NCClade, fill = NCClade)) +
        geom_bar(stat = "count") +
        #coord_polar() +
        theme_bw() +
        labs(x = "", title = com) +
        theme(axis.text.x = element_blank())

   # p <- plotly::ggplotly(p)
}





