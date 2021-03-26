#' Title
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


#' Title
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


