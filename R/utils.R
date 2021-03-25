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

#' Function which returns a user's IP address
#' This input field is hidden from the user by default, so it will not appear or be editable.
#' Include in ui.R with the following code: inputIp("ipid")
#' @export
#'
inputIP <- function(inputId, value=' '){
    tagList(
        tags$input(id = inputId, class = "ip", value=as.character(value), type="text", style="display:none;"),
        singleton(tags$head(includeScript(system.file("js/ip.js", package="shinyTestR")))),
        singleton(tags$head(includeScript(system.file("js/javascriptMessages.js", package="shinyTestR"))))
    )
}

#' Function to send a message from shiny to get a user's IP address
#' Include in server.R with the following code: outputIP(session)
#' Note: you must call the shinyServer function with session as an argument.
#' @export
#'
outputIP <- function(session=session){
    observe({
        session$sendCustomMessage(type='javascript', message=list(value="getip();"))
    })
}


