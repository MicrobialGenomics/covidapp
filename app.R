library(covidapp)
library(ggplot2)


# Preprocessing map data --------------------------------------------------
# if (!file.exists("data/map_data.rds")) {
#     df_map <- readr::read_rds("data/MergedData_spain.rds") %>%
#         map_data(my_map_data = ca_spain_gj, ca_inhabitants = ca_inhabitants)
#
#     readr::write_rds(df_map, file = "data/map_data.rds")
# } else {
#     df_map <<-  readr::read_rds("data/map_data.rds")
# }
load("data/map_data.Rdata")
df_map <<- df_map
shiny::shinyApp(ui = covidapp::ui(), server = covidapp::server())





