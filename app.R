library(covidapp)

df_over <<- readr::read_rds("data/MergedData_spain.rds")
df_map <<- readr::read_rds("data/map_data.rds")
shiny::shinyApp(ui = covidapp::ui(), server = covidapp::server())





