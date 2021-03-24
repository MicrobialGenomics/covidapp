footer_1 <- shiny::div(
    class = "footer",
    shiny::fluidPage(
        ## Spaces and div line
        shiny::hr(),
        shiny::br(),

        ## First row
        shiny::fluidRow(
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
                    ),
                    shiny::tags$a(
                        href = "",
                        shiny::img(
                            src = "images/whats.png",
                            height = '30px',
                            width = '30px',
                            style = "border-radius: 50%;"
                        )
                    ),
                    shiny::tags$a(
                        href = "",
                        shiny::img(
                            src = "images/facebook.png",
                            height = '30px',
                            width = '30px'
                        )
                    )
                )
            ),
            shiny::column(
                width = 9,
                align = "center",
                shiny::h4("Powered by", style = "font-weight: bold; font-style: italic;"),
                shiny::br(),
                shiny::fixedRow(
                    shiny::column(
                        width = 2,
                        align = "center",
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
                        align = "center",
                        shiny::tags$a(
                            href = 'https://nextstrain.org/?utm_source=CoVariants',
                            shiny::img(
                                src =  "images/nextstrain.svg",
                                height = '40px',
                                width = '110px'
                            )
                        )
                    ),
                    shiny::column(
                        width = 2,
                        align = "center",
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
                            href = 'https://www.tidyverse.org',
                            shiny::img(
                                src =  "images/tidyverse.png",
                                height = '40px',
                                width = '90px'
                            )
                        )
                    )
                )
            )
        ),
        shiny::br(),
        shiny::br(),

        ## Second row
        shiny::fixedRow(
            shiny::column(
                width = 7,
                align = "center",
                shiny::h4("Affilations", style = "font-weight: bold; font-style: italic;"),
                shiny::br(),
                shiny::br(),
                shiny::fluidRow(
                    shiny::column(
                        width = 6,
                        align = "right",
                        shiny::tags$a(
                            href = "http://www.hospitalgermanstrias.cat/es",
                            shiny::img(
                                src = "images/gtp.png",
                                height = '75px',
                                width = '200px'
                            )
                        )
                    ),
                    shiny::column(
                        width = 6,
                        align = "left",
                        shiny::tags$a(
                            href = "http://www.germanstrias.org/es-index/",
                            shiny::img(
                                src = "images/igtp.png",
                                height = '75px',
                                width = '200px'
                            )
                        )
                    )
                ),
                shiny::br(),
                shiny::br(),
                shiny::fluidRow(
                    shiny::column(
                        width = 12,
                        align = "center",
                        shiny::tags$a(
                            href = "https://www.irsicaixa.es/",
                            shiny::img(
                                src = "images/logo_irsicaixa.png",
                                height = '75px',
                                width = '200px'
                            )
                        )
                    )
                )
            ),
            shiny::column(
                width = 5,
                align = "center",
                shiny::h4("Developed by", style = "font-weight: bold; font-style: italic;"),
                shiny::br(),
                shiny::fixedRow(
                    # MNJ
                    shiny::column(
                        width = 6,
                        align = "center",
                        shiny::tags$a(
                            href = 'https://www.irsicaixa.es/ca/node/2300',
                            shiny::img(
                                src =  "images/mnoguera.png",
                                height = '100px',
                                width = '100px',
                                style = "border-radius: 50%;"
                            )
                        ),
                        shiny::h5("Marc Noguera i Julian, PhD"),
                        shiny::h6("Microbial Genomics Group"),
                        shiny::h6("Institut de Recerca de la Sida, Barcelona"),
                        shiny::br(),
                        shiny::fixedRow(
                            shiny::column(
                                width = 6,
                                align = "right",
                                shiny::tags$a(
                                    href = "https://www.linkedin.com/in/marcnoguerajulian/",
                                    shiny::img(
                                        src = "images/linkedin.svg",
                                        height = '25px',
                                        width = '25px'
                                    )
                                )
                            ),
                            shiny::column(
                                width = 6,
                                align = "left",
                                shiny::tags$a(
                                    href = "https://twitter.com/marc_noguera",
                                    shiny::img(
                                        src = "images/twitter.png",
                                        height = '25px',
                                        width = '40px'
                                    )
                                )
                            )
                        )
                    ),
                    # FCM
                    shiny::column(
                        width = 6,
                        align = "center",
                        shiny::tags$a(
                            href = 'https://www.irsicaixa.es/ca/investigadors/francesc-catala-moll',
                            shiny::img(
                                src =  "images/fcatala.jpg",
                                height = '100px',
                                width = '100px',
                                style = "border-radius: 50%;"
                            )
                        ),
                        shiny::h5("Francesc Català Moll, PhD"),
                        shiny::h6("Microbial Genomics Group"),
                        shiny::h6("Institut de Recerca de la Sida, Barcelona"),
                        shiny::br(),
                        shiny::fixedRow(
                            shiny::column(
                                width = 6,
                                align = "right",
                                shiny::tags$a(
                                    href = "https://www.linkedin.com/in/francesc-català-moll-11342485/",
                                    shiny::img(
                                        src = "images/linkedin.svg",
                                        height = '25px',
                                        width = '25px'
                                    )
                                )
                            ),
                            shiny::column(
                                width = 6,
                                align = "left",
                                shiny::tags$a(
                                    href = "https://twitter.com/fcatalamoll",
                                    shiny::img(
                                        src = "images/twitter.png",
                                        height = '25px',
                                        width = '40px'
                                    )
                                )
                            )
                        )
                    )
                )
            )
        ),
        shiny::br(),
        shiny::br()
    )
)


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
                shiny::fluidRow(
                    shiny::column(
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
                    )
                )
            ),

            # ## Developed
            # shiny::column(
            #     width = 2,
            #     align = "center",
            #     shiny::h2("Developed by", style = "font-weight: bold; font-style: italic;"),
            #     shiny::fixedRow(
            #         # MNJ
            #         shiny::column(
            #             width = 6,
            #             align = "center",
            #             shiny::tags$a(
            #                 href = 'https://www.irsicaixa.es/ca/node/2300',
            #                 shiny::img(
            #                     src =  "images/mnoguera.png",
            #                     height = '60px',
            #                     width = '60px',
            #                     style = "border-radius: 50%;"
            #                 )
            #             ),
            #             shiny::h5("Marc Noguera i Julian, PhD")
            #         ),
            #         # FCM
            #         shiny::column(
            #             width = 6,
            #             align = "center",
            #             shiny::tags$a(
            #                 href = 'https://www.irsicaixa.es/ca/investigadors/francesc-catala-moll',
            #                 shiny::img(
            #                     src =  "images/fcatala.jpg",
            #                     height = '60px',
            #                     width = '60px',
            #                     style = "border-radius: 50%;"
            #                 )
            #             ),
            #             shiny::h5("Francesc Català Moll, PhD")
            #         )
            #     )
            # )
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
