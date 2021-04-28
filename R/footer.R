footer <- shiny::div(
    class = "footer",
    shiny::fluidPage(
        ## Spaces and div line
        shiny::hr(),
        shiny::fluidRow(
            shiny::column(
                width = 6,
                align = "center",
                shiny::h4("Powered by", style = "font-weight: bold; font-style: italic;"),
                shiny::br(),
                shiny::fixedRow(
                    shiny::column(
                        width = 2,
                        align = "right",
                        shiny::tags$a(
                            href = 'https://www.gisaid.org',
                            shiny::img(
                                src =  "images/gisaid.png",
                                height = '40px',
                                width = '80px'
                            )
                        )
                    ),
                    shiny::column(
                        width = 2,
                        align = "left",
                        shiny::tags$a(
                            href = 'https://shiny.rstudio.com',
                            shiny::img(
                                src =  "images/shiny.png",
                                height = '40px',
                                width = '110px'
                            )
                        )
                    ),
                    shiny::column(
                        width = 2,
                        align = "center",
                        shiny::tags$a(
                            href = 'https://leafletjs.com',
                            shiny::img(
                                src =  "images/leaflet.png",
                                height = '40px',
                                width = '110px'
                            )
                        )
                    ),
                    shiny::column(
                        width = 2,
                        align = "center",
                        shiny::tags$a(
                            href = 'https://plotly.com',
                            shiny::img(
                                src =  "images/plotly.png",
                                height = '40px',
                                width = '130px'
                            )
                        )
                    ),
                    shiny::column(
                        width = 2,
                        align = "center",
                        shiny::tags$a(
                            href = 'https://docs.docker.com/compose/',
                            shiny::img(
                                src =  "images/compose.png",
                                height = '60px',
                                width = '130px'
                            )
                        )
                    ),
                    shiny::column(
                        width = 2,
                        align = "center",
                        shiny::tags$a(
                            href = 'https://www.docker.com',
                            shiny::img(
                                src =  "images/gh_actions.png",
                                height = '60px',
                                width = '100px'
                            )
                        )
                    ),
                )
            ),
            shiny::column(
                width = 3,
                align = "center",
                shiny::h4("Share", style = "font-weight: bold; font-style: italic;"),
                shiny::br(),
                shiny::fixedRow(
                    shiny::tags$a(
                        href = "",
                        shiny::img(
                            src = "images/mail.png",
                            height = '30px',
                            width = '30px',
                            style = "border-radius: 50%;"
                        )
                    ),
                    shiny::tags$a(
                        href = "https://twitter.com/intent/tweet?text=Hello%20world&url=http://ec2-34-254-223-195.eu-west-1.compute.amazonaws.com/covidapp/",
                        shiny::img(
                            src = "images/twitter2.png",
                            height = '30px',
                            width = '30px',
                            style = "border-radius: 50%;"
                        )
                    ),
                    shiny::tags$a(
                        href = "https://www.linkedin.com/sharing/share-offsite/?url=http://ec2-34-254-223-195.eu-west-1.compute.amazonaws.com/covidapp/",
                        shiny::img(
                            src = "images/linkedin.svg",
                            height = '30px',
                            width = '30px',
                            style = "border-radius: 50%;"
                        )
                    )
                ),
            ),
            ## Affiliation
            shiny::column(
                width = 3,
                align = "center",
                shiny::h4("Affiliations", style = "font-weight: bold; font-style: italic;"),
                shiny::br(),
                shiny::fluidRow(shiny::column(
                    width = 12,
                    align = "center",
                    shiny::tags$a(
                        href = "https://www.irsicaixa.es/",
                        shiny::img(
                            src = "images/logo_irsicaixa.png",
                            height = '60px',
                            width = '150px',
                            style = "opacity: 0.6;"
                        )
                    )
                ))
            ),
        ),
        shiny::br(),
        shiny::tags$a(
            # href = "",
            shiny::h6(
                "Developed by Francesc Català Moll and Marc Noguera i Julian",
                align = "center",
                style = "color: darkgray;"
            )
        ),
        shiny::tags$a(
            href = "mailto:covidapp@irsicaixa.es?",
            shiny::h6(
                "Contact us at covidseq@irsicaixa.es",
                align = "center",
                style = "color: darkgray;"
            )
        ),
        shiny::tags$a(
            href = "https://www.gisaid.org/registration/terms-of-use/",
            shiny::h6(
                "GISAID data provided on this website are a subject to GISAID Terms and Conditions",
                align = "center",
                style = "color: darkgray;"
            )
        )
    )
)
