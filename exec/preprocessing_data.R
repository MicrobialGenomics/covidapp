library(covidapp)

df_map <- readr::read_rds("data/MergedData_spain.rds") %>%
    map_data(my_map_data = ca_spain_gj, ca_inhabitants = ca_inhabitants)

readr::write_rds(df_map, file = "data/map_data.rds")
