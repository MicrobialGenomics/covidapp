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
                            href = 'https://containrrr.dev/watchtower',
                            target="_blank",
                            shiny::img(
                                src =  "images/whatchtower.png",
                                height = '40px',
                                width = '40px'
                            )
                        )
                    ),
                    shiny::column(
                        width = 2,
                        align = "left",
                        shiny::tags$a(
                            href = 'https://shiny.rstudio.com',
                            target="_blank",
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
                            target="_blank",
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
                            target="_blank",
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
                            target="_blank",
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
                            target="_blank",
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
                width = 2,
                align = "center",
                shiny::h4("Share", style = "font-weight: bold; font-style: italic;"),
                shiny::fixedRow(
                    shiny::br(),
                    shiny::tags$a(
                        href = "",
                        target="_blank",
                        shiny::img(
                            src = "images/mail.png",
                            target="_blank",
                            height = '30px',
                            width = '30px',
                            style = "border-radius: 50%;"
                        )
                    ),
                    # https://twitter.com/intent/tweet?url=https%3A%2F%2Fcovariants.org%2F&text=CoVariants&via=firefoxx66&hashtags=CoVariants
                    shiny::tags$a(
                        href = "https://twitter.com/intent/tweet?url=http%3A%2F%2Fcovidtag.paseq.org%2F&text=CovidTag&hashtags=CovidTag",
                        target="_blank",
                        shiny::img(
                            src = "images/twitter2.png",
                            height = '30px',
                            width = '30px',
                            style = "border-radius: 50%;"
                        )
                    ),
                    shiny::tags$a(
                        href = "https://www.linkedin.com/sharing/share-offsite/?url=http://ec2-34-254-223-195.eu-west-1.compute.amazonaws.com/covidapp/",
                        target="_blank",
                        shiny::img(
                            src = "images/linkedin.svg",
                            height = '30px',
                            width = '30px',
                            style = "border-radius: 50%;"
                        )
                    )
                ),
            ),
            shiny::column(
                width = 2,
                align = "center",
                shiny::h4("Supported by", style = "font-weight: bold; font-style: italic;"),
                shiny::br(),
                shiny::fluidRow(shiny::column(
                    width = 12,
                    align = "center",
                    shiny::tags$a(
                        href = "https://www.ccma.cat/tv3/marato/",
                        target="_blank",
                        shiny::img(
                            src = "images/marato.svg",
                            height = '40px',
                            width = '200px',
                            style = "opacity: 0.6;"
                        )
                    )
                ))
            ),
            ## Affiliation
            shiny::column(
                width = 2,
                align = "center",
                shiny::h4("Affiliations", style = "font-weight: bold; font-style: italic;"),
                shiny::br(),
                shiny::fluidRow(shiny::column(
                    width = 12,
                    align = "center",
                    shiny::tags$a(
                        href = "https://www.irsicaixa.es/",
                        target="_blank",
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
        shiny::h6(
            "Developed",
            shiny::a(
                " Francesc Català Moll",
                href = "https://www.irsicaixa.es/es/investigadores/francesc-catala-moll",
                target = "_blank"
            ),
            "and",
            shiny::a(
                "  Marc Noguera i Julian",
                href = "https://www.irsicaixa.es/es/node/2300",
                target = "_blank"
            ),
            align = "center",
            style = "color: darkgray;"
        ),
        shiny::h6(
            "Contact us at",
            shiny::a(
                " covidseq@irsicaixa.es",
                href = "mailto:covidseq@irsicaixa.es?",
                target = "_blank"
            ),
            align = "center",
            style = "color: darkgray;"
        ),
        shiny::h6(
            "Amb el suport de la Fundació La Marató de TV3 (Projecte 202126-30-21)",
            align = "center",
            style = "color: darkgray;"
        ),
        shiny::h6(
            "GISAID data provided on this website are a subject to GISAID's ",
            shiny::a(
                "Terms and Conditions",
                href = "https://www.gisaid.org/DAA/",
                target = "_blank"
            ),
            align = "center",
            style = "color: darkgray;"
        )
    )
)


