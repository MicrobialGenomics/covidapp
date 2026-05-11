#' SEO metadata shared by CovidTag entry points.
covidtag_seo_head <- function() {
    title <- "CovidTag | SARS-CoV-2 variant surveillance in Spain"
    description <- paste(
        "CovidTag is an interactive dashboard for SARS-CoV-2 genomic",
        "surveillance in Spain, tracking variants, mutations and sequencing",
        "efforts using GISAID data."
    )
    url <- "https://covidtag.paseq.org/"
    image <- "https://covidtag.paseq.org/images/covidTag.png"

    shiny::tags$head(
        shiny::includeHTML("google-analytics.html"),
        shiny::tags$meta(name = "description", content = description),
        shiny::tags$link(rel = "canonical", href = url),
        shiny::tags$meta(name = "robots", content = "index, follow"),
        shiny::tags$meta(property = "og:type", content = "website"),
        shiny::tags$meta(property = "og:title", content = title),
        shiny::tags$meta(property = "og:description", content = description),
        shiny::tags$meta(property = "og:url", content = url),
        shiny::tags$meta(property = "og:image", content = image),
        shiny::tags$meta(name = "twitter:card", content = "summary"),
        shiny::tags$meta(name = "twitter:title", content = title),
        shiny::tags$meta(name = "twitter:description", content = description),
        shiny::tags$meta(name = "twitter:image", content = image),
        shiny::tags$script(
            type = "application/ld+json",
            shiny::HTML('{
  "@context": "https://schema.org",
  "@type": "WebApplication",
  "name": "CovidTag",
  "url": "https://covidtag.paseq.org/",
  "description": "CovidTag is an interactive dashboard for SARS-CoV-2 genomic surveillance in Spain, tracking variants, mutations and sequencing efforts using GISAID data.",
  "applicationCategory": "HealthApplication",
  "operatingSystem": "Web",
  "image": "https://covidtag.paseq.org/images/covidTag.png",
  "publisher": {
    "@type": "Organization",
    "name": "PASEQ"
  }
}')
        )
    )
}

#' Title
#'
#' @export
ui <- function() {
    shiny::shinyUI(
        shiny::navbarPage(
            title = shiny::div(
                shiny::tags$a(
                    href = "http://covidtag.paseq.org",
                    target = "_blank",
                    shiny::img(
                        src = "images/covidTag.png",
                        height = 80,
                        width = 80,
                        style = "margin:-50px 5px"
                    )
                ),
                shiny::a(
                    "Enabled by data from",
                    style = "font-size: 14px; color: white",
                    href = "https://www.gisaid.org",
                    target = "_blank",
                    shiny::img(
                        src = "images/gisaid_2.png",
                        height = 20,
                        width = 50
                    )
                ),
                
                shiny::fixedRow(
                    shiny::a(
                        "Plot my data",
                        style = "font-size: 14px; color: white",
                        href = "http://covidtag.paseq.org:1989"
                    ),
                    shiny::a(
                        href = "https://github.com/MicrobialGenomics/covidapp",
                        target = "_blank",
                        shiny::img(
                            src = "images/github_2.png",
                            height = 20,
                            width = 20,
                            style = "filter: invert(1)"
                        )
                    ),
                    shiny::a(
                        href = "https://twitter.com/MicrobialGene",
                        target = "_blank",
                        shiny::img(
                            src = "images/twitter2.png",
                            height = 20,
                            width = 20,
                            style = "border-radius: 50%;"
                        )
                    ),
                    style = "right: 50px; top: 20px; position: absolute;"
                )
            ),
            theme = shinythemes::shinytheme("flatly"),
            fluid = TRUE,
            collapsible = TRUE,
            windowTitle = "CovidTag | SARS-CoV-2 variant surveillance in Spain",
            header = shiny::tagList(
                covidtag_seo_head(),
                shiny::tags$head(shiny::tags$style('
                       nav .container:first-child {
                           margin-left:100px; width: 100%;
                       }')),
                shinyjs::useShinyjs()
            ),

            # TAB1: Overview
            shiny::tabPanel(
                title = "Overview",
                value = "tab_1",
                overview_module_ui("tab1"),
                shiny::br(),
                shiny::br(),
                footer
            ),

            # TAB2: Cat map
            shiny::tabPanel(
                title = "Sequencing efforts",
                value = "tab_2",
                map_module_ui("tab2")
            ),

            # TAB3: Per Autonomous Com
            shiny::tabPanel(
                title = "Per C.A.",
                value = "tab_3",
                per_ca_module_ui("tab3"),
                shiny::br(),
                shiny::br(),
                footer
            ),

            # TAB4: Acknowledgements
            shiny::tabPanel(
                title = "Acknowledgements",
                value = "tab_4",
                acknowledgements_module_ui("tab4"),
                shiny::br(),
                shiny::br(),
                footer
            )
        )
    )
}

#' Title
#'
#' @export
server <- function() {
    function(input, output, session) {

        shinyjs::runjs('document.title = "CovidTag | SARS-CoV-2 variant surveillance in Spain"')
        shinylogs::track_usage(storage_mode = shinylogs::store_rds("logs/"))

        ## Module tab 1
        overview_module_server("tab1")

        ## Module tab 2
        map_module_server("tab2")

        ## Module tab 3
        per_ca_module_server("tab3")
        
        ## Module tab 4
        acknowledgements_module_server("tab4")
        
        
    }
}

#' Title
#'
#' @export
ui_2 <- function() {
    shiny::shinyUI(
        shiny::navbarPage(
            title = shiny::div(
                shiny::tags$a(
                    href = "http://covidtag.paseq.org",
                    target = "_blank",
                    shiny::img(
                        src = "images/covidTag.png",
                        height = 80,
                        width = 80,
                        style = "margin:-50px 5px"
                    )
                ),
                shiny::fixedRow(
                    shiny::a(
                        href = "https://github.com/MicrobialGenomics/covidapp",
                        target = "_blank",
                        shiny::img(
                            src = "images/github_2.png",
                            height = 20,
                            width = 20,
                            style = "filter: invert(1)"
                        )
                    ),
                    shiny::a(
                        href = "https://twitter.com/MicrobialGene",
                        target = "_blank",
                        shiny::img(
                            src = "images/twitter2.png",
                            height = 20,
                            width = 20,
                            style = "border-radius: 50%;"
                        )
                    ),
                    style = "right: 50px; top: 20px; position: absolute;"
                )
            ),
            theme = shinythemes::shinytheme("flatly"),
            fluid = TRUE,
            collapsible = TRUE,
            windowTitle = "CovidTag | SARS-CoV-2 variant surveillance in Spain",
            header = shiny::tagList(
                covidtag_seo_head(),
                shiny::tags$head(shiny::tags$style('
                       nav .container:first-child {
                           margin-left:100px; width: 100%;
                       }')),
                shinyjs::useShinyjs()
            ),
            
            # TAB2: Overview
            shiny::tabPanel(
                title = "Overview",
                value = "tab_1",
                overview2_module_ui("tab1"),
                shiny::br(),
                shiny::br(),
                footer
            )
        )
    )
}

#' Title
#'
#' @export
server_2 <- function() {
    function(input, output, session) {
        
        shinyjs::runjs('document.title = "CovidTag | SARS-CoV-2 variant surveillance in Spain"')
        shinylogs::track_usage(storage_mode = shinylogs::store_rds("logs/"))
        
        ## Module tab 1
        overview2_module_server("tab1")
    }
}
