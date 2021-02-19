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
            shiny::span(shiny::tags$i(shiny::h6("Data from GSAID Initiative.")), style="color:#045a8d"),
            shiny::br(),
            shiny::h4(shiny::textOutput(outputId = ns("acum_seq")), align = "right"),
            shiny::h4(shiny::textOutput(outputId = ns("week_seq")), align = "right"),
            shiny::h6(shiny::textOutput(outputId = ns("sel_week")), align = "right"),
            shiny::h6(shiny::textOutput(outputId = ns("count_ca")), align = "right"),
            shiny::plotOutput(outputId = ns("plot_counts"), height = "150px", width = "100%"),
            shiny::plotOutput(outputId = ns("plot_cumsum"), height = "150px", width = "100%"),

            shiny::uiOutput(outputId = ns("plot_date")),

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

        ## Load data
        df <- readr::read_rds("data/MergedData_spain.rds") %>%
            tidyr::drop_na(week_num) %>%
            dplyr::mutate(
                acom_name = factor(
                    acom_name,
                    c(unique(acom_name), "Territorio no asociado a ninguna autonomía")
                ),
                date = format(collection_date, "%y-%W"),
                week = as.numeric(stringr::str_remove(date, ".*-")),
                year = as.numeric(stringr::str_remove(date, "-.*")),
                date = lubridate::parse_date_time(paste0(year, "/", week, "/", 1), 'y/W/w')
            )

        ## Slider date
        output$plot_date <- shiny::renderUI({
            shinyWidgets::sliderTextInput(
                inputId = session$ns("plot_date"),
                label = shiny::h5("Select mapping date"),
                choices = format(sort(unique(df$date)), "%d %b %y"),
                selected = format(max(df$date), "%d %b %y"),
                grid = FALSE
            )
        })

        # Filter by date
        f_df <- shiny::reactive({
            shiny::req(input$plot_date)
            format_date <- lubridate::parse_date_time(input$plot_date, 'd b y')
            df %>%
                dplyr::filter(date <= format_date)
        })

        ## Map plot -------
        map <- ca_spain_gj
        map$cases <- df %>%
            dplyr::count(acom_name, .drop = FALSE) %>%
            dplyr::left_join(
                x = tibble::tibble(acom_name = map$acom_name),
                y = .,
                by = "acom_name"
            ) %>%
            dplyr::pull(n)

        bins = c(seq(0, max(map$cases) + 250 , by = 250))
        cv_pal <- leaflet::colorBin("Oranges", domain = map$cases, bins = bins)

        base_map <- leaflet::leaflet(map) %>%
            leaflet::addTiles() %>%
            leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
            leaflet::setView(lng = -4, lat = 40, zoom = 6) %>%
            leaflet::addLegend(
                position = "topright",
                pal = cv_pal,
                values = ~ cases,
                title = "<small>Sequenced cases</small>"
            )

        output$mymap <- leaflet::renderLeaflet({ base_map })

        observeEvent(input$plot_date, {

            map$cases <- f_df() %>%
                dplyr::count(acom_name, .drop = FALSE) %>%
                dplyr::left_join(
                    x = tibble::tibble(acom_name = map$acom_name),
                    y = .,
                    by = "acom_name"
                ) %>%
                dplyr::pull(n)

            p <- map$acom_name %>%
                purrr::set_names() %>%
                purrr::map(function(x) {
                    if (x == "Territorio no asociado a ninguna autonomía") {
                        popup <- ggplot()
                    } else {
                        popup <- df %>%
                            prepro_variants(ca = x) %>%
                            plot_vairants(type = "bar",
                                          var = "counts",
                                          pal = "mg",
                                          plotly = FALSE)
                    }
                    popup
                })


            leaflet::leafletProxy("mymap") %>%
                leaflet::clearMarkers() %>%
                leaflet::clearShapes() %>%
                leaflet::addPolygons(
                    data = map,
                    stroke = FALSE,
                    smoothFactor = 0.3,
                    fillOpacity = 0.6,
                    fillColor = ~ cv_pal(cases)
                ) %>%
                leaflet::addPolygons(
                    data = map,
                    stroke = FALSE,
                    fillOpacity = 0,
                    fillColor = "transparent",
                    popup = leafpop::popupGraph(p, width = 400, height = 300)
                )
        })


        ## Absolute panel ------------------------------------------------------
        output$acum_seq <- shiny::renderText({
            acum_seq <- prettyNum(nrow(f_df()), big.mark = ",")
            glue::glue("{acum_seq} Total Sequences")
        })

        output$week_seq <- shiny::renderText({
            week_seq <- f_df() %>%
                dplyr::filter(date == max(date)) %>%
                nrow() %>%
                prettyNum(., big.mark = ",")

            glue::glue("{week_seq} New Sequences")
        })

        output$sel_week <- shiny::renderText({
            sel_week <- input$plot_date
            sel_week
        })

        output$count_ca <- shiny::renderText({
            count_ca <- f_df() %>%
                dplyr::count(acom_name) %>%
                dplyr::filter(n > 0) %>%
                nrow()

            glue::glue("{count_ca} of 19 C. A. with reported sequences")
        })

        output$plot_counts <- shiny::renderPlot({ efforts_all(f_df())$pp_counts })
        output$plot_cumsum <- shiny::renderPlot({ efforts_all(f_df())$pp_cumsum })
    })
}


# plot_pie <- function(com, df) {
#     df %>%
#         dplyr::mutate(NCClade = forcats::fct_infreq(NCClade)) %>%
#         dplyr::filter(acom_name == com) %>%
#         dplyr::count(NCClade, .drop = FALSE) %>%
#         ggplot(aes(NCClade, n, fill = NCClade)) +
#         geom_bar(stat = "identity", alpha = 0.7, position = "dodge") +
#         theme_minimal() +
#         scale_fill_brewer(palette = "Spectral", guide = F) +
#         labs(x = "", title = com) +
#         theme(axis.text.x = element_text(angle = 30, hjust = 1))
# }





