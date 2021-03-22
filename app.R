library(covidapp)
df_map <<- readr::read_rds("data/map_results_data.rds")
shiny::shinyApp(ui = covidapp::ui(), server = covidapp::server())





