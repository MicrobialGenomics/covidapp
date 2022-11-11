library(covidapp)
library(ggplot2)

# Load map module data ----------------------------------------------------
df_map <<- readr::read_rds("data/map_data.rds")

# Load overview module data -----------------------------------------------
df_over <<- df_map$dat %>%
    dplyr::mutate(acom_name = stringr::str_replace_all(acom_name, "Cataluña", "Catalunya")) 

mt <<- readr::read_rds("data/MutationEmbeddedData.rds")
mt_pos <<- extract_mutations(mt)

# Load per C.A module data ------------------------------------------------
df_ca <<- df_over
all_plots <<-  readr::read_rds("data/all_plots.rds")

# dev.off()
# covidTag initialization -------------------------------------------------
shiny::shinyApp(ui = covidapp::ui(), server = covidapp::server())




