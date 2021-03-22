library(covidapp)
library(ggplot2)


# Preporcessing map data --------------------------------------------------
df_map <<- readr::read_rds("data/MergedData_spain.rds") %>%
    map_data(my_map_data = ca_spain_gj, ca_inhabitants = ca_inhabitants)

shiny::shinyApp(ui = covidapp::ui(), server = covidapp::server())





