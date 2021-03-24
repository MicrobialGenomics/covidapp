library(covidapp)

df_map <<- readr::read_rds("data/map_data.rds")
df_over <<- df_map$dat %>%
    dplyr::mutate(acom_name = stringr::str_replace_all(acom_name, "Cataluña", "Catalunya")) %>%
    dplyr::filter(!acom_name == "Spain")

mt <<- readr::read_rds("data/MutationEmbeddedData.rds")
mt_pos <<- extract_mutations(mt)

shiny::shinyApp(ui = covidapp::ui(), server = covidapp::server())





