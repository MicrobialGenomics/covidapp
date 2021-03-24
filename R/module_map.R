#' Title
#'
#' @param id identification
#'
#' @export
map_module_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::div(
        class = "outer",
        shiny::tags$head(shiny::includeCSS("www/style/map_style.css")),

        ## Map
        leaflet::leafletOutput(outputId = ns("mymap"), width = "100%", height = "100%"),

        ## Absolute panel
        shiny::absolutePanel(
            id = "controls", class = "panel panel-default",
            top = 180, left = 55, width = 300, fixed = TRUE,
            draggable = TRUE, height = "auto",
            shiny::span(shiny::tags$i(
                shiny::h6(
                    "Data from GISAID Initiative. Note, that it is important
                        not to assume that shown data are necessarily
                        representative of the all region due to a potential non
                        uniform sampling"
                )
            ), style = "color:#045a8d; text-align: justify;"),
            shiny::br(),
            shiny::h4(shiny::textOutput(outputId = ns("acum_seq")), align = "right"),
            shiny::h4(shiny::textOutput(outputId = ns("week_seq")), align = "right"),
            shiny::h6(shiny::textOutput(outputId = ns("sel_week")), align = "right"),
            shiny::h6(shiny::textOutput(outputId = ns("count_ca")), align = "right"),
            shiny::plotOutput(outputId = ns("plot_counts"), height = "150px", width = "100%"),
            shiny::plotOutput(outputId = ns("plot_cumsum"), height = "150px", width = "100%"),

            shiny::uiOutput(outputId = ns("plot_date")),
            shinyWidgets::materialSwitch(
                inputId = ns("norm"),
                label = "per 1e5 inhabitant correction",
                value = TRUE,
                right = TRUE,
                width = "100%",
                status = "info"
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
map_module_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {

        # df_map <- readr::read_rds("data/map_data.rds")

        ## Slider date
        output$plot_date <- shiny::renderUI({
            shinyWidgets::sliderTextInput(
                inputId = session$ns("plot_date"),
                label = shiny::h5("Select mapping date"),
                choices = format(as.Date(sort(names(df_map$maps))), "%d %b %y"),
                selected = format(max(as.Date(sort(names(df_map$maps)))), "%d %b %y"),
                grid = FALSE
            )
        }) %>%
            shiny::bindCache(input$date)

        ## Filter by date
        f_df <- shiny::reactive({
            shiny::req(input$plot_date)
            format_date <- as.Date(input$plot_date, "%d %b %y")
            df_map$dat %>%
                dplyr::filter(date <= format_date)
        })

        ## Base map
        base_map <- leaflet::leaflet(df_map$bs_map$base_map) %>%
            leaflet::addTiles() %>%
            leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
            leaflet::setView(lng = -4, lat = 40, zoom = 6) %>%
            leaflet::addLegend(
                position = "topright",
                pal = df_map$bs_map$cv_pal_norm,
                values = ~ norm_cases,
                title = "<small>Seq. cases per 1e5 inhab.</small>"
            ) %>%
            leaflet::addLegend(
                position = "topright",
                pal = df_map$bs_map$cv_pal,
                values = ~ cases,
                title = "<small>Total Sequenced cases</small>"
            )

        output$mymap <- leaflet::renderLeaflet({ base_map })

        ## Map reactivity
        shiny::observeEvent(c(input$plot_date, input$norm), {
            shiny::req(input$plot_date, f_df())
            format_date <- input$plot_date %>% as.Date("%d %b %y") %>% paste()
            map <- df_map$bs_map$base_map

            p <- map$acom_name %>%
                purrr::set_names() %>%
                purrr::map(function(x) {
                    if (x == "Territorio no asociado a ninguna autonomía") {
                        popup <- ggplot()
                    } else {
                        popup <- f_df() %>%
                            dplyr::filter(acom_name == x) %>%
                            efforts_all() %>% .[["dual"]]
                    }
                    popup
                })

            if (isTRUE(input$norm)) {
                map[["norm_cases"]] <- df_map$maps[[format_date]]$map_data$norm_cases
                leaflet::leafletProxy("mymap") %>%
                    leaflet::clearMarkers() %>%
                    leaflet::clearShapes() %>%
                    leaflet::addPolygons(
                        data = map,
                        stroke = FALSE,
                        smoothFactor = 0.3,
                        fillOpacity = 0.6,
                        fillColor = ~ df_map$bs_map$cv_pal_norm(norm_cases)
                    ) %>%
                    leaflet::addPolygons(
                        data = map,
                        stroke = FALSE,
                        fillOpacity = 0,
                        fillColor = "transparent",
                        popup = leafpop::popupGraph(p, width = 500, height = 300)
                    )
            } else {
                map[["cases"]] <- df_map$maps[[format_date]]$map_data$cases
                leaflet::leafletProxy("mymap") %>%
                    leaflet::clearMarkers() %>%
                    leaflet::clearShapes() %>%
                    leaflet::addPolygons(
                        data = map,
                        stroke = FALSE,
                        smoothFactor = 0.3,
                        fillOpacity = 0.6,
                        fillColor = ~ df_map$bs_map$cv_pal(cases)
                    ) %>%
                    leaflet::addPolygons(
                        data = map,
                        stroke = FALSE,
                        fillOpacity = 0,
                        fillColor = "transparent",
                        popup = leafpop::popupGraph(p, width = 500, height = 300)
                    )
            }
        })

        ## Absolute panel reactivity
        # Total sequences
        output$acum_seq <- shiny::renderText({
            acum_seq <- prettyNum(nrow(f_df()), big.mark = ",")
            glue::glue("{acum_seq} Total Sequences")
        }) %>%
            shiny::bindCache(f_df())

        # New sequences selected date
        output$week_seq <- shiny::renderText({
            week_seq <- f_df() %>%
                dplyr::filter(date == max(date)) %>%
                nrow() %>%
                prettyNum(., big.mark = ",")

            glue::glue("{week_seq} New Sequences")
        }) %>%
            shiny::bindCache(f_df())

        # Selected date
        output$sel_week <- shiny::renderText({
            sel_week <- input$plot_date
            sel_week
        }) %>%
            shiny::bindCache(input$plot_date)

        # Affected C.A. counts
        output$count_ca <- shiny::renderText({
            count_ca <- f_df() %>%
                dplyr::count(acom_name) %>%
                dplyr::filter(n > 0) %>%
                nrow()

            glue::glue("{count_ca} of 19 C. A. with reported sequences")
        }) %>%
            shiny::bindCache(f_df())

        # Plot counts
        output$plot_counts <- shiny::renderPlot({
            shiny::req(f_df())
            f_df() %>% efforts_all() %>% .[["pp_counts"]]
        }) %>%
            shiny::bindCache(f_df())

        # Plot commutative counts
        output$plot_cumsum <- shiny::renderPlot({
            shiny::req(f_df())
            f_df() %>% efforts_all() %>% .[["pp_cumsum"]]
        }) %>%
            shiny::bindCache(f_df())
    })
}
