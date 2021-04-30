library(covidapp)
library(ggplot2)

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

com_aut <- df_ca %>%
    dplyr::pull(acom_name) %>%
    unique() %>%
    sort() %>%
    c("Spain", .)


all_plots <- list()
for (com in com_aut) {
    all_plots[["counts"]][["pangolin_lineage"]][[com]] <- df_ca %>%
        prepro_variants(ca = com, var_anno = "pangolin_lineage") %>%
        plot_vairants(
            type = "bar",
            var = "counts",
            pal_dir = -1,
            pal = "mg"
        )

    all_plots[["counts"]][["NCClade"]][[com]] <- df_ca %>%
        prepro_variants(ca = com, var_anno = "NCClade") %>%
        plot_vairants(
            type = "bar",
            var = "counts",
            pal_dir = -1,
            pal = "mg"
        )

    all_plots[["freq"]][["pangolin_lineage"]][[com]] <- df_ca %>%
        prepro_variants(ca = com, var_anno = "pangolin_lineage") %>%
        plot_vairants(
            type = "bar",
            var = "freq",
            pal_dir = -1,
            pal = "mg"
        )

    all_plots[["freq"]][["NCClade"]][[com]] <- df_ca %>%
        prepro_variants(ca = com, var_anno = "NCClade") %>%
        plot_vairants(
            type = "bar",
            var = "freq",
            pal_dir = -1,
            pal = "mg"
        )

    all_plots
}

all_plots <<- all_plots

# covidTag initialization -------------------------------------------------
shiny::shinyApp(ui = covidapp::ui(), server = covidapp::server())





