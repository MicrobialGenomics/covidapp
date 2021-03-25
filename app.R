library(covidapp)

# Load map module data ----------------------------------------------------
df_map <<- readr::read_rds("data/map_data.rds")

# Load overview module data -----------------------------------------------
df_over <<- df_map$dat %>%
    dplyr::mutate(acom_name = stringr::str_replace_all(acom_name, "Cataluña", "Catalunya")) %>%
    dplyr::filter(!acom_name == "Spain")

mt <<- readr::read_rds("data/MutationEmbeddedData.rds")
mt_pos <<- extract_mutations(mt)

# Load per C.A module data ------------------------------------------------
df_ca <<- df_over

# covidTag initialization -------------------------------------------------
shiny::shinyApp(ui = covidapp::ui(), server = covidapp::server())





