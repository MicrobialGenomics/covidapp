#' Pre-processing variants data
#'
#' @param df tibble
#' @param ca all or autonomous community
#' @param var_anno variant annotation column. ex. "NCClade" or "pangolin_lineage"
#'
#' @import ggplot2
#' @importFrom graphics text
#' @return tibble
#' @export
prepro_variants <- function(df, ca = "Spain", var_anno = "NCClade") {
    if (var_anno == "pangolin_lineage") {
        to_retain <- df %>%
            dplyr::mutate(clade = forcats::fct_infreq(!!sym(var_anno))) %>%
            dplyr::pull(clade) %>% 
            levels() %>% 
            .[. %in% c(.[1:11], "B.1.1.7", "P.1", "B.1.351", "B.1.617.2")] %>% 
            unique()

        df <- df %>% dplyr::mutate(!!sym(var_anno) := dplyr::if_else(!!sym(var_anno) %in% to_retain, !!sym(var_anno), "Other"))
    }

    df <- df %>%
        dplyr::mutate(
            clade = forcats::fct_infreq(!!sym(var_anno)) %>% forcats::fct_rev()
        )

    if (ca != "Spain") { df <- df %>% dplyr::filter(acom_name == ca) }

    df %>%
        tidyr::drop_na(week_num) %>%
        dplyr::mutate(
            date = format(collection_date, "%y-%W"),
            week = as.numeric(stringr::str_remove(date, ".*-")),
            year = as.numeric(stringr::str_remove(date, "-.*")),
            week_num = lubridate::parse_date_time(paste0(year, "/", week, "/", 1), 'y/W/w')
        ) %>%
        dplyr::group_by(week_num, .drop = FALSE) %>%
        dplyr::select(week_num, clade) %>%
        dplyr::count(clade, .drop = FALSE) %>%
        dplyr::summarise(
            freq = n / sum(n),
            pct = freq * 100,
            counts = n,
            sum = sum(counts),
            clade = clade
        )
}

#' Plot variants
#'
#' @param df output of function prepro_variants
#' @param type Class of plot. Options: density or bar
#' @param var variable to plot. Options: "freq" and "counts"
#' @param pal Used palette
#' @param pal_dir Palete direction order. Options: 1 and -1
#' @param clade List with clade to plot
#' @param plotly boolean indicating if output plot must be plotly
#'
#' @return plotly or ggplot object
#' @export
plot_vairants <- function(df,
                          type = "density",
                          var = "freq",
                          pal = "mg",
                          pal_dir = -1,
                          clade = NULL,
                          plotly = TRUE) {

    if (!is.null(clade)) {
        df <- df %>% dplyr::filter(clade %in% clade)
    }

    df <- df %>%
        dplyr::mutate(
            text = stringr::str_c(
                "Clade:", clade,
                "<br>Frequency:", round(freq, 2),
                "<br>Counts:", counts,
                "<br>Total counts:", sum,
                "<br>Date:", week_num,
                sep = " "
            )
        )

    if (type == "density") {
        pp <- df %>%
            ggplot(aes(week_num, !!sym(var), fill = clade, group = clade, text = text)) +
            geom_density(stat = "identity", position = "stack", alpha = 0.5, colour = NA)
    } else if (type == "bar") {
        pp <- df %>%
            ggplot(aes(week_num, !!sym(var), fill = clade, group = clade, text = text)) +
            geom_bar(stat = "identity", position = "stack", alpha = 0.5, colour = NA)
    }

    t_1 <- dplyr::if_else(var == "freq", "Frequency", "Counts")
    t_2 <- dplyr::if_else(var == "freq", "Frequency", "Counts by Variant")

    pp <- pp +
        theme_minimal(base_rect_size = 0, base_size = 12) +
        labs(x = "", y = t_2)

    if (var == "freq") {
        pp <- pp + scale_y_continuous(labels = scales::percent, limits = c(0, 1))
    }

    if (!pal == "mg") {
        pp <- pp + scale_fill_brewer(palette = pal, direction = pal_dir, name = "")
    } else {
        pal <- c("#6A33C2", "#CAB2D6", "darkorchid1", "gray", "#000000", "#E69F00", "#56B4E9", "#009E73",
                 "#F0E442", "#0072B2", "#D55E00", "#CC79A7","darkred","darkgreen",
                 "steelblue", "#F51313", "#85660D", "#AA0DFE")

        pp <- pp + scale_fill_manual(values = pal)
    }

    if (isTRUE(plotly)) {
        pp <- plotly::ggplotly(pp, tooltip = "text") %>%
            plotly::layout(
                hovermode = 'closest',
                legend = list(orientation = 'h', y = -0.2)
            ) %>%
            plotly::config(displaylogo = FALSE)
    }
    pp
}


#' Plots sequencing efforts by center
#'
#' @param df df
#' @param pos y transformation
#'
#' @return plotly
#' @export
efforts_by_center <- function(df, pos = "stack") {
    prepro <- df %>%
        tidyr::drop_na(week_num) %>%
        dplyr::mutate(
            sub_center = dplyr::case_when(
                stringr::str_detect(submitting_lab, "d'Hebron") ~ "Vall d'Hebron Institut de Recerca",
                stringr::str_detect(submitting_lab, "FISABIO") ~ "FISABIO-Public Health",
                stringr::str_detect(submitting_lab, "SeqCOVID-SPAIN") ~ "SeqCOVID-SPAIN consortium",
                stringr::str_detect(submitting_lab, "IrsiCaixa") ~ "IrsiCaixa - Can Ruti CovidSeq",
                stringr::str_detect(submitting_lab, "INIA") ~ "Instituto Nacional de Investigación y Tecnología Agraria y Alimentaria (INIA)",
                stringr::str_detect(submitting_lab, "Compostela") ~ "Universidad de Santiago de Compostela",
                stringr::str_detect(submitting_lab, "Universitario de Vigo") ~ "Complexo Hospitalario Universitario de Vigo",
                stringr::str_detect(submitting_lab, "Hospital Universitario Donostia") ~ "Hospital Universitario Donostia",
                stringr::str_detect(submitting_lab, "Hospital Ramón y Cajal") ~ "Hospital Ramón y Cajal",
                stringr::str_detect(submitting_lab, "Central de Asturias") ~ "Hospital Universitario Central de Asturias",
                TRUE ~ submitting_lab
            ),
            sub_center = factor(sub_center)
        )

    t_1 <- dplyr::if_else(pos == "fill", "Frequency", "Counts")
    t_2 <- dplyr::if_else(pos == "fill", "Frequency", "Counts by Center")

    pp <- prepro %>%
        dplyr::mutate(center = forcats::fct_infreq(sub_center)) %>%
        ggplot(aes(x = week_num , fill = center)) +
        geom_bar(stat = "count", position = pos, alpha = 0.6) +
        theme_minimal(base_rect_size = 0) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
              legend.position = "none") +
        labs(x = "", y = t_2) +
        scale_fill_manual(values = pals::polychrome() %>% as.vector())

    if (pos == "fill") {
        pp <- pp + scale_y_continuous(labels = scales::percent, limits = c(0, 1))
    }

    plotly::ggplotly(pp, tooltip = c("fill", "count")) %>%
        plotly::config(displaylogo = FALSE) %>%
        plotly::layout(
            hovermode = 'compare',
                       legend = list(orientation = 'h', y = -10)
        )
}

#' Plot variant by C.A.
#'
#' @param df tibble
#' @param variant NCClade to plot
#' @param var_col column name of plotting variant
#'
#' @return plotly
#' @export
plot_variant_by_com <- function(df, variant, var_col) {
    prepro <- df %>%
        tidyr::drop_na(week_num) %>%
        dplyr::mutate(
            varcol = !!sym(var_col),
            varcol = factor(varcol, levels = unique(varcol)),
            acom_name = factor(acom_name, levels = unique(acom_name)),
            date = format(collection_date, "%y-%W"),
            week = as.numeric(stringr::str_remove(date, ".*-")),
            year = as.numeric(stringr::str_remove(date, "-.*")),
            week_num = lubridate::parse_date_time(paste0(year, "/", week, "/", 1), 'y/W/w')
        ) %>%
        dplyr::group_by(week_num, acom_name) %>%
        dplyr::count(varcol, .drop = FALSE) %>%
        dplyr::summarise(
            freq = n / sum(n),
            pct = freq * 100,
            counts = n,
            sum = sum(counts),
            varcol = varcol,
            acom_name = acom_name
        ) %>%
        tidyr::replace_na(replace = list(freq = 0, pct = 0)) %>%
        dplyr::filter(varcol == variant) %>%
        dplyr::mutate(text = stringr::str_c("C.A:", acom_name, "<br>Frequency:", pct, sep = " "))

    pp <- prepro %>%
        dplyr::rename("C.A" = "acom_name") %>%
        ggplot(aes(week_num, freq, colour = C.A, group = C.A, text = text)) +
        stat_smooth(se = F, method = "gam", formula = y ~ s(x), size = 0.8, alpha = 0.5, geom = "line", lty = 5) +
        geom_point(data = . %>% dplyr::filter(!freq == 0), alpha = 0.3, shape = 21) +
        scale_colour_manual(values = pals::polychrome() %>% as.vector(), name = "") +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1.05), breaks = seq(0, 1, by = 0.25)) +
        labs(x = "", y = "Frequency") +
        theme_minimal(base_rect_size = 0) +
        theme(legend.position = "none")

    plotly::ggplotly(pp, tooltip = "colour") %>%
        plotly::layout(hovermode = 'closest') %>%
        plotly::config(displaylogo = FALSE)
}

#' Plot variant by C.A.
#'
#' @param df tibble
#' @param mt mutation count list
#' @param variant NCClade to plot
#' @param var_col column name of plotting variant
#'
#' @return plotly
#' @export
plot_variant_line <- function(df, mt, variant, var_col) {
    prepro <- df %>%
        tidyr::drop_na(week_num) %>%
        dplyr::mutate(
            varcol = !!sym(var_col),
            varcol = factor(varcol, levels = unique(varcol)),
            acom_name = factor(acom_name, levels = unique(acom_name)),
            date = format(collection_date, "%y-%W"),
            week = as.numeric(stringr::str_remove(date, ".*-")),
            year = as.numeric(stringr::str_remove(date, "-.*")),
            week_num = lubridate::parse_date_time(paste0(year, "/", week, "/", 1), 'y/W/w')
        ) %>%
        dplyr::group_by(week_num) %>%
        dplyr::count(varcol, .drop = FALSE) %>%
        dplyr::summarise(
            freq = n / sum(n),
            pct = freq * 100,
            counts = n,
            sum = sum(counts),
            varcol = varcol
        ) %>%
        tidyr::replace_na(replace = list(freq = 0, pct = 0)) %>%
        dplyr::filter(varcol == variant)

    pp <- prepro %>%
        ggplot(aes(week_num, freq, colour = varcol, fill = varcol)) +
        stat_smooth(method = "gam", formula = y ~ s(x), size = 0.8, alpha = 0.5, lty = 0) +
        geom_point(data = . %>% dplyr::filter(!freq == 0), alpha = 0.8, shape = 21) +
        scale_y_continuous(labels = scales::percent) + # , breaks = seq(0, 1, by = 0.25)
        scale_fill_manual(values = "#FEE08B") +
        scale_colour_manual(values = "#F46D43") +
        labs(x = "", y = "Frequency") +
        theme_minimal(base_rect_size = 0) +
        theme(legend.position = "none")

    plotly::ggplotly(pp) %>%
        plotly::config(displaylogo = FALSE)
}


#' Preprocessing mutations
#'
#' @param df df
#' @param ca CA or spain
#'
#' @return df
#' @export
prepro_mutations <- function(df, ca = "Spain") {

    all_pos <- df %>%
        tidyr::unite(mutation, c(aaSubstitutions, aaDeletions), sep = ",", remove = TRUE) %>%
        dplyr::select(mutation) %>%
        tidyr::separate_rows(mutation, sep = ",") %>%
        dplyr::filter(mutation != "NA") %>%
        dplyr::filter(!stringr::str_detect(mutation, "X$")) %>%
        dplyr::mutate(mutation = stringr::str_remove_all(mutation, "[:alpha:]$|-$")) %>%
        dplyr::pull(mutation) %>%
        unique()

    pre <- df %>%
        dplyr::select(week_num, aaSubstitutions, aaDeletions, collection_date, acom_name) %>%
        tidyr::drop_na(week_num) %>%
        dplyr::mutate(
            date = format(collection_date, "%y-%W"),
            week = as.numeric(stringr::str_remove(date, ".*-")),
            year = as.numeric(stringr::str_remove(date, "-.*")),
            week_num = lubridate::parse_date_time(paste0(year, "/", week, "/", 1), 'y/W/w')
        ) %>%
        tidyr::unite(mutation, c(aaSubstitutions, aaDeletions), sep = ",", remove = TRUE)

    if (ca != "Spain") { pre <- pre %>% dplyr::filter(acom_name == ca) }

    pre %>%
        dplyr::pull(week_num) %>%
        unique() %>%
        purrr::map_dfr(function(x) {
            pre %>%
                dplyr::filter(week_num == as.Date(x)) %>%
                dplyr::rowwise() %>%
                dplyr::mutate(mutation = check_mutations(mutation, all_pos)) %>%
                tidyr::unnest(mutation) %>%
                dplyr::select(week_num, mutation) %>%
                dplyr::count(mutation, .drop = FALSE) %>%
                dplyr::mutate(pos = stringr::str_sub(mutation, end = -2)) %>%
                dplyr::group_by(pos) %>%
                dplyr::summarise(week_num = as.Date(x),
                                 freq = n / sum(n),
                                 pct = freq * 100,
                                 counts = n,
                                 sum = sum(counts),
                                 mutation = mutation) %>%
                dplyr::ungroup()
        }) %>%
        dplyr::mutate(mutation = forcats::fct_infreq(mutation))
}


#' Plot mutations
#'
#' @param df df
#' @param var counts or freq
#' @param pal mg or RColorBrewer palette
#' @param plotly TRUE or FALSE
#' @param mut_pos position to plot
#'
#' @return plot
#' @export
plot_mutations <- function(df,
                          mut_pos = "ORF1b:P314",
                          var = "counts",
                          pal = "mg",
                          plotly = TRUE) {

    df <- df %>% dplyr::filter(stringr::str_detect(mutation, mut_pos))

    df <- df %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            text = stringr::str_c(
                "Mutation:", mutation,
                "<br>frequency:", round(freq, 2),
                "<br>percentage:", round(pct, 2),
                "<br>count:", counts,
                "<br>total:", sum,
                sep = " "
            )
        )

    t_1 <- dplyr::if_else(var == "freq", "Frequency", "Counts")
    t_2 <- dplyr::if_else(var == "freq", "Frequency", "Counts by Variant")

    pp <- df %>%
        ggplot(aes(week_num, !!sym(var), fill = mutation, group = mutation, text = text)) +
        geom_bar(stat = "identity", position = "stack", alpha = 0.5, colour = NA) +
        theme_minimal(base_rect_size = 0, base_size = 12) +
        labs(x = "", y = t_2)

    if (var == "freq") {
        pp <- pp + scale_y_continuous(labels = scales::percent, limits = c(0, 1))
    }

    pal <- c("darkorchid1", "gray", "#000000", "#E69F00", "#56B4E9", "#009E73", 
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7","darkred","darkgreen",
             "steelblue", "#F51313")
    
    pp <- pp + scale_fill_manual(values = rev(pal))

    if (isTRUE(plotly)) {
        pp <- plotly::ggplotly(pp, tooltip = "text") %>%
            plotly::config(displaylogo = FALSE) %>%
            plotly::layout(
                hovermode = 'closest',
                legend = list(orientation = 'h', y = -0.2)
            )
    }
    pp
}

#' Chexk mutations
#'
#' @param mut mutation columns
#' @param all_pos all positions
#'
#' @return list
#' @export
check_mutations <- function(mut, all_pos) {
    mutations <- mut %>%
        stringr::str_split(pattern = ",") %>%
        unlist() %>%
        .[!. == "NA"]

    to_remove <- mutations %>% stringr::str_remove_all("[:alpha:]$|$-")

    filt_wt <- all_pos[!all_pos %in% to_remove]
    final_letter <- filt_wt %>% stringr::str_remove_all(".*:|\\d.*")
    filt_wt <- stringr::str_c(filt_wt, final_letter)

    res <- list(c(mutations, filt_wt))
}

#' Plot mutations 2
#'
#' @param inp_list mutations list
#' @param region C.A or Spain
#' @param var counts or freq
#' @param pal mg or RColorBrewer palette
#' @param plotly TRUE or FALSE
#' @param mut_pos position to plot
#'
#' @return plot
#' @export
plot_mutations_2 <- function(inp_list,
                             region = "Spain",
                             mut_pos = "S:1251",
                             var = "counts",
                             pal = "mg",
                             plotly = TRUE) {

    gene <- stringr::str_remove_all(mut_pos, ":.*")
    pos <- as.numeric(stringr::str_remove_all(mut_pos, ".*:"))

    if (region == "Catalunya") { reg = "Cataluña" } else { reg = region }

    df <- inp_list[[gene]][[pos]][[reg]] %>%
        tidyr::pivot_longer(cols = -1) %>%
        tidyr::replace_na(list(value = 0)) %>%
        dplyr::mutate(
            week = stringr::str_remove(week_num, ".*-"),
            year = stringr::str_remove(week_num, "-.*"),
            week = dplyr::if_else(week == "53", "52", week),
            week_num = lubridate::parse_date_time(paste0(year, "/", week, "/", 1), 'y/W/w')
        ) %>%
        dplyr::group_by(week_num) %>%
        dplyr::summarise(
            mutation = name,
            counts = value,
            total = sum(value),
            freq = value / total
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            text = stringr::str_c(
                "Mutation:", mutation,
                "<br>Frequency:", round(freq, 2),
                "<br>Counts:", counts,
                "<br>Total counts:", total,
                sep = " "
            )
        )

    t_1 <- dplyr::if_else(var == "freq", "Frequency", "Counts")
    t_2 <- dplyr::if_else(var == "freq", "Frequency", "Counts by Variant")

    pp <- df %>%
        ggplot(aes(week_num, !!sym(var), fill = mutation, group = mutation, text = text)) +
        geom_bar(stat = "identity", position = "stack", alpha = 0.5, colour = NA) +
        theme_minimal(base_rect_size = 0, base_size = 12) +
        labs(x = "", y = t_2)

    if (var == "freq") {
        pp <- pp + scale_y_continuous(labels = scales::percent, limits = c(0, 1))
    }

    pal <- c("darkorchid1", "gray", "#000000", "#E69F00", "#56B4E9", "#009E73", 
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7","darkred","darkgreen",
             "steelblue", "#F51313")
    
    pp <- pp + scale_fill_manual(values = rev(pal))

    if (isTRUE(plotly)) {
        pp <- plotly::ggplotly(pp, tooltip = "text") %>%
            plotly::layout(
                hovermode = 'closest',
                legend = list(orientation = 'h', y = -0.2)
            ) %>%
            plotly::config(displaylogo = FALSE)
    }
    pp
}

#' Plot mutation line
#'
#' @param inp_list mutation list
#' @param region region
#' @param mut_pos muatatio pos
#' @param mut variant
#'
#' @return plotly
#' @export
plot_mutation_line <- function(inp_list,
                               region = "Spain",
                               mut_pos = "S:1251",
                               mut = "V") {

    gene <- stringr::str_remove_all(mut_pos, ":.*")
    pos <- as.numeric(stringr::str_remove_all(mut_pos, ".*:"))

    if (region == "Catalunya") { reg = "Cataluña" } else { reg = region }

    na_replace = list()
    na_replace[[mut]] <- 0

    prepro <- inp_list[[gene]][[pos]][[reg]] %>%
        tidyr::pivot_longer(cols = -1) %>%
        tidyr::replace_na(list(value = 0)) %>%
        dplyr::mutate(
            week = stringr::str_remove(week_num, ".*-"),
            year = stringr::str_remove(week_num, "-.*"),
            week = dplyr::if_else(week == "53", "52", week),
            week_num = lubridate::parse_date_time(paste0(year, "/", week, "/", 1), 'y/W/w')
        ) %>%
        dplyr::group_by(week_num) %>%
        dplyr::summarise(
            mutation = name,
            counts = value,
            total = sum(value),
            freq = value / total
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(mutation == mut)

    pp <- prepro %>%
        ggplot(aes(week_num, freq, colour = mutation, fill = mutation)) +
        stat_smooth(
            method = "gam",
            formula = y ~ s(x),
            size = 0.8,
            alpha = 0.5,
            lty = 0
        ) +
        geom_point(data = . %>% dplyr::filter(!freq == 0), alpha = 0.8, shape = 21) +
        scale_y_continuous(labels = scales::percent) + #, breaks = seq(0, 1, by = 0.25)
        scale_fill_manual(values = "#FEE08B") +
        scale_colour_manual(values = "#F46D43") +
        labs(x = "", y = "Frequency") +
        theme_minimal(base_rect_size = 0) +
        theme(legend.position = "none")

    plotly::ggplotly(pp) %>%
        plotly::config(displaylogo = FALSE)
}

#' empty plot
#'
#' @param msg output message
#'
#' @return plotly
#' @export
empty_plot <- function(msg = "No data for this region") {
    pp <- tibble::tibble(x = 1, y = 1, text = msg) %>%
        ggplot(aes(x, y, label = text)) +
        geom_text() +
        theme_void()

    plotly::ggplotly(pp) %>%
        plotly::config(displaylogo = FALSE)
}

#' efforts all
#'
#' @param df df
#'
#' @return ggplot
#' @export
#'
#' @import ggplot2
efforts_all <- function(df) {
    prepro <- df %>%
        dplyr::count(date) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(cum = cumsum(n))

    by_date <- prepro %>%
        ggplot(aes(x = date, y = n)) +
        geom_bar(stat = "identity", colour = NA, fill = "#FDAE61", alpha = 0.7) +
        theme_minimal(base_rect_size = 0, base_size = 15) +
        theme(legend.position = "none") +
        labs(x = "Date", y = "New sequences (weekly)")

    accomulate <- prepro %>%
        ggplot(aes(x = date, y = cum)) +
        geom_line(colour = "#6BAED6") +
        geom_point(colour = "#6BAED6") +
        theme_minimal(base_rect_size = 0, base_size = 15) +
        theme(legend.position = "none") +
        labs(x = "Date", y = "Comulative sequences")

    coeff <- round(max(prepro$cum) / max(prepro$n))

    dual <- prepro %>%
        ggplot(aes(x = date)) +
        geom_bar(aes(y = n), stat = "identity", colour = NA, fill = "#FDAE61", alpha = 0.8) +
        geom_line(aes(y = cum / coeff), colour = "#6BAED6") +
        geom_point(aes(y = cum / coeff), colour = "#6BAED6") +
        scale_y_continuous(
            name = "New sequences (weekly)",
            sec.axis = sec_axis(~.*coeff, name = "Comulative sequences")
        ) +
        theme_minimal(base_rect_size = 0, base_size = 15) +
        theme(
            axis.title.y = element_text(color = "#E16462FF"),
            axis.title.y.right = element_text(color = "#6BAED6")
        ) +
        labs(x = "Date")

    list(pp_counts = by_date, pp_cumsum = accomulate, dual = dual)
}
