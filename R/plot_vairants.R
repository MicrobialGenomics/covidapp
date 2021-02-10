#' Pre-processing variants data
#'
#' @param remove_others Boolean indicating if Other variants category must be removed
#'
#' @return tibble
#' @export
prepro_variants <- function(remove_others = FALSE) {

    # Pre-processing data
    dat <- jsonlite::fromJSON("data/EUClusters_data.json")[["countries"]][["Spain"]] %>%
        tibble::as_tibble() %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            sum = sum(dplyr::c_across(dplyr::matches("EU|S:"))),
            Others = total_sequences - sum,
            .after = 1
        ) %>%
        dplyr::select(-total_sequences, -sum) %>%
        tidyr::pivot_longer(cols = 2:8, names_to = "variant", values_to = "counts") %>%
        dplyr::group_by(week) %>%
        dplyr::summarise(freq = round(counts / sum(counts), 2),
                         pct = freq * 100,
                         counts = counts,
                         sum = sum(counts),
                         variant = variant)

    ## Removing others variants
    if (isTRUE(remove_others)) {
        dat <- dat %>% dplyr::filter(!variant == "Others")
    }

    dat
}

#' Plot variants
#'
#' @param df output of function prepro_variants
#' @param var variable to plot. Options: "freq" and "counts"
#' @param pal Used palette
#' @param pal_dir Palete direction order. Options: 1 and -1
#'
#' @return plotly object
#' @export
plot_vairants <- function(df, var = "freq", pal = "Spectral", pal_dir = -1) {

    # define order by median
    ord <- df %>%
        dplyr::group_by(variant) %>%
        dplyr::summarise(mean = mean(!!sym(var))) %>%
        dplyr::pull(mean) %>%
        sort()

    # Prepare ggplot
    pp <- df %>%
        dplyr::mutate(variant = forcats::fct_reorder(variant, rev(ord)),
                      text = stringr::str_c(
                          "variant:", variant,
                          "<br>frequency:", freq,
                          "<br>percentage:", pct,
                          "<br>count:", counts,
                          "<br>total:", sum,
                          sep = " "
                      )) %>%
        ggplot(aes(week, !!sym(var), fill = variant, group = variant, text = text)) +
        geom_density(stat = "identity", position = "stack", alpha = 0.5, colour = NA) +
        theme_minimal(base_line_size = 0) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
        scale_fill_brewer(palette = pal, direction = pal_dir, name = "") +
        labs(x = "", y = "")

    if (var == "freq") {
        pp <- pp + scale_y_continuous(labels = scales::percent, limits = c(0, 1))
    }

    ## Converting to plotly
    plotly::ggplotly(pp, tooltip = "text") %>%
        plotly::layout(hovermode = 'compare') %>%
        plotly::config(displaylogo = FALSE)
}

prepro_variants(remove_others = F) %>%
    plot_vairants()


