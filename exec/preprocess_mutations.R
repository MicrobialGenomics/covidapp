
#' Title
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

#' Title
#'
#' @param df df
#'
#' @return list
#' @export
preproces_mutations <- function(df) {
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

    df %>%
        dplyr::pull(acom_name) %>%
        unique() %>%
        c("Spain", .) %>%
        purrr::set_names() %>%
        purrr::map(function(ca) {

            if (ca != "Spain") { pre <- pre %>% dplyr::filter(acom_name == ca) }

            res <- pre %>%
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
        })
    res
}







