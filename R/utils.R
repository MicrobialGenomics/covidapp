#' Extract mutations
#'
#' @param inp_list list
#'
#' @return character vector
#' @export
extract_mutations <- function(inp_list) {
    names(inp_list) %>%
        purrr::map_dfr(function(name) {
            f_res <- inp_list[[name]] %>%
                length() %>%
                seq_len() %>%
                purrr::map_dfr(function(x) {
                    dat <- inp_list[[name]][[x]]
                    if (is.null(dat)) {
                        res = tibble::tibble(mut = NA)
                    } else {
                        res <- tibble::tibble(mut = glue::glue("{name}:{x}"))
                    }
                    res
                })
        }) %>%
        tidyr::drop_na(mut) %>%
        dplyr::pull(mut)
}


#' Option mutations
#' @param inp_list list
#' @param gene_pos gene:pos
#'
#' @return character vector
#' @export
option_mutation <- function(inp_list, gene_pos) {
    gene <- stringr::str_remove_all(gene_pos, ":.*")
    pos <- as.numeric(stringr::str_remove_all(gene_pos, ".*:"))

    inp_list[[gene]][[pos]][["Spain"]] %>%
        dplyr::select(-week_num) %>%
        names()
}

#' Generate base map data
#'
#' @param df mergedData
#' @param map map object
#'
#' @return list
#' @export
base_map <- function(df, map) {
    map$cases <- df %>%
        dplyr::count(acom_name, .drop = FALSE) %>%
        dplyr::left_join(
            x = tibble::tibble(acom_name = map$acom_name),
            y = .,
            by = "acom_name"
        ) %>%
        tidyr::replace_na(list(n = 0)) %>%
        dplyr::pull(n)

    bins = seq(0, max(map$cases) + max(map$cases)*0.2 , by = round(max(map$cases)*0.2))
    cv_pal <- leaflet::colorBin("Oranges", domain = map$cases, bins = bins)

    map$norm_cases <- map$cases / ca_inhabitants * 1e5
    map$norm_cases[is.na(map$norm_cases)] <- 0

    bins_norm = seq(0, max(map$norm_cases) + max(map$norm_cases)*0.2 , by = round(max(map$norm_cases)*0.2))
    cv_pal_norm <- leaflet::colorBin("Reds", domain = map$norm_cases, bins = bins_norm)

    list(base_map = map, cv_pal_norm = cv_pal_norm, cv_pal = cv_pal)
}

#' Map data
#'
#' @param df tibble
#' @param my_map_data map geojson
#' @param ca_inhabitants inhabitant values by C.A.
#'
#' @return list
#' @export
map_data <- function(df, my_map_data, ca_inhabitants) {
    dat <- df %>%
        tidyr::drop_na(week_num) %>%
        dplyr::filter(acom_name != "Spain") %>%
        dplyr::mutate(
            acom_name = factor(
                acom_name,
                c(unique(acom_name), "Territorio no asociado a ninguna autonomía")
            ),
            date = format(collection_date, "%y-%W"),
            week = as.numeric(stringr::str_remove(date, ".*-")),
            year = as.numeric(stringr::str_remove(date, "-.*")),
            date = lubridate::parse_date_time(paste0(year, "/", week, "/", 1), 'y/W/w')
        )

    map <- my_map_data
    bs_map = base_map(df, my_map_data)

    map_plot <- dat$date %>%
        unique() %>%
        purrr::set_names() %>%
        purrr::map(function(filt_date) {
            dat <- dat %>% dplyr::filter(date <= filt_date)
            res <- list()
            res$cases <- dat %>%
                dplyr::count(acom_name, .drop = FALSE) %>%
                dplyr::left_join(
                    x = tibble::tibble(acom_name = map$acom_name),
                    y = .,
                    by = "acom_name"
                ) %>%
                dplyr::pull(n)

            res$norm_cases <- res$cases / ca_inhabitants * 1e5
            res$norm_cases[is.na(res$norm_cases)] <- 0

            list(map_data = res)
        })

    list(dat = dat, bs_map = bs_map, maps = map_plot)
}
