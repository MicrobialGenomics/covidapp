library(covidapp)
library(ggplot2)

# covidTag initialization -------------------------------------------------
shiny::shinyApp(ui = covidapp::ui_2(), server = covidapp::server_2())
