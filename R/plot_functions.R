#' Pre-processing variants data
#'
#' @param df tibble
#' @param ca all or autonomous community
#' @param var_anno variant annotation column. ex. "NCClade" or "pangolin_lineage"
#'
#' @return tibble
#' @export
prepro_variants <- function(df, ca = "Spain", var_anno = "NCClade") {

    if (var_anno == "pangolin_lineage") {
        to_retain <- df %>%
            dplyr::mutate(clade = forcats::fct_infreq(!!sym(var_anno))) %>%
            dplyr::pull(clade) %>% levels() %>% .[1:12]

        df <- df %>% dplyr::filter(!!sym(var_anno) %in% to_retain)
    }

    df <- df %>%
        dplyr::mutate(clade = forcats::fct_infreq(!!sym(var_anno)) %>% forcats::fct_rev())

    if (ca != "Spain") { df <- df %>% dplyr::filter(acom_name == ca) }

    # Pre-processing data
    df %>%
        tidyr::drop_na(week_num) %>%
        dplyr::group_by(week_num, .drop = FALSE) %>%
        dplyr::select(week_num, clade) %>%
        dplyr::count(clade, .drop = FALSE) %>%
        dplyr::summarise(freq = n / sum(n),
                         pct = freq * 100,
                         counts = n,
                         sum = sum(counts),
                         clade = clade)
}

#' Plot variants
#'
#' @param df output of function prepro_variants
#' @param type Class of plot. Options: density or bar
#' @param var variable to plot. Options: "freq" and "counts"
#' @param pal Used palette
#' @param pal_dir Palete direction order. Options: 1 and -1
#' @param clade List with clade to plot
#'
#' @return plotly object
#' @export
plot_vairants <- function(df,
                          type = "density",
                          var = "freq",
                          pal = "mg",
                          pal_dir = -1,
                          clade = NULL) {

    # filter clade
    if (!is.null(clade)) {
        df <- df %>% dplyr::filter(clade %in% clade)
    }

    # define order by median
    # ord <- df %>%
    #     dplyr::group_by(clade) %>%
    #     dplyr::summarise(mean = mean(!!sym(var))) %>%
    #     dplyr::arrange(mean) %>%
    #     dplyr::pull(clade)

    # Text label for plotly
    df <- df %>%
        dplyr::mutate(#clade = factor(clade, levels = ord),
                      text = stringr::str_c(
                          "clade:", clade,
                          "<br>frequency:", freq,
                          "<br>percentage:", pct,
                          "<br>count:", counts,
                          "<br>total:", sum,
                          sep = " "
                      ))

    # Define plot class
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

     # Common plot
    pp <- pp +
        theme_minimal(base_rect_size = 0) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
        labs(x = "", y = t_2)

    if (var == "freq") {
        pp <- pp + scale_y_continuous(labels = scales::percent, limits = c(0, 1))
    }

    if (!pal == "mg") {
        pp <- pp + scale_fill_brewer(palette = pal, direction = pal_dir, name = "")
    } else {
        pal <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                 "#D55E00", "#CC79A7","darkred","darkgreen","steelblue", "#F51313")
        pp <- pp + scale_fill_manual(values = pal)
    }

    ## Converting to plotly
    # title <- list(text = stringr::str_c(
    #     glue::glue("SARS-CoV-2 Variant {t_1} by Week, Full GISAID"),
    #     "<br>", "<sup>", "Feb 10th (Spain)", "</sup>"
    # ))

    plotly::ggplotly(pp, tooltip = "text") %>%
        plotly::layout(
            hovermode = 'closest',
            legend = list(orientation = 'h', y = -0.2)
            #title = title,
            #margin = list(l = 0, r = 0, b = 0, t = 50, pad = 4)
        ) %>%
        plotly::config(displaylogo = FALSE)
}


#' Plots sequencing efforts by center
#'
#' @param df df
#' @param pos y transformation
#'
#' @return plotly
#' @export
efforts_by_center <- function(df, pos = "stack") {
    ## Edit center names
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

    ## Plotting by position type
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

    ## Converting to plotly
    # title <- list(text = stringr::str_c(
    #     glue::glue("Sequencing efforts by Week, Full GISAID"),
    #     "<br>", "<sup>", "Feb 10th (Spain)", "</sup>"
    # ))

    plotly::ggplotly(pp, tooltip = c("fill", "count")) %>%
        plotly::layout(
            hovermode = 'compare',
            legend = list(orientation = 'h', y = -10)
            # title = title,
            # margin = list(l = 20, r = 20, b = 0, t = 50, pad = 4)
        ) %>%
        plotly::config(displaylogo = FALSE)
}


#' Plot variant by C.A.
#'
#' @param df tibble
#' @param variant NCClade to plot
#'
#' @return plotly
#' @export
plot_variant_by_com <- function(df, variant) {
    # Preparing data
    prepro <- df %>%
        tidyr::drop_na(week_num) %>%
        dplyr::mutate(
            NCClade = factor(NCClade, levels = unique(NCClade)),
            acom_name = factor(acom_name, levels = unique(acom_name))
        ) %>%
        dplyr::group_by(week_num, acom_name) %>%
        dplyr::count(NCClade, .drop = FALSE) %>%
        dplyr::summarise(
            freq = n / sum(n),
            pct = freq * 100,
            counts = n,
            sum = sum(counts),
            NCClade = NCClade,
            acom_name = acom_name
        ) %>%
        tidyr::replace_na(replace = list(freq = 0, pct = 0)) %>%
        dplyr::filter(NCClade == variant) %>%
        dplyr::mutate(text = stringr::str_c("C.A:", acom_name, "<br>Frequency:", pct, sep = " "))

    # Plot data
    pp <- prepro %>%
        dplyr::rename("C.A" = "acom_name") %>%
        ggplot(aes(week_num, freq, colour = C.A, group = C.A, text = text)) +
        stat_smooth(se = F, method = "loess", formula = "y ~ x", size = 0.8,
                    alpha = 0.5, geom = "line") +
        scale_colour_manual(values = pals::polychrome() %>% as.vector(), name = "") +
        scale_y_continuous(labels = scales::percent,
                           limits = c(0, 1.05), breaks = seq(0, 1, by = 0.25)) +
        labs(x = "", y = "Frequency") +
        theme_minimal(base_rect_size = 0) +
        theme(legend.position = "none",
              axis.text.x = element_text(
                  angle = 90,
                  hjust = 1,
                  vjust = 0.5
              ))

    # Convert to plotly
    plotly::ggplotly(pp, tooltip = "colour") %>%
        plotly::layout(hovermode = 'closest') %>%
        plotly::config(displaylogo = FALSE)
}


efforts_all <- function(df) {
    ## Edit center names
    prepro <- df %>%
        dplyr::count(date) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(cum = cumsum(n))

    ## Plotting by position type
    by_date <- prepro %>%
        ggplot(aes(x = date, y = n)) +
        geom_line(colour = "#E16462FF") +
        geom_point(colour = "#E16462FF") +
        theme_minimal(base_rect_size = 0) +
        theme(legend.position = "none") +
        labs(x = "Date", y = "New sequences (weekly)")

    accomulate <- prepro %>%
        ggplot(aes(x = date, y = cum)) +
        geom_line(colour = "#E16462FF") +
        geom_point(colour = "#E16462FF") +
        theme_minimal(base_rect_size = 0) +
        theme(legend.position = "none") +
        labs(x = "Date", y = "Comulative sequences")

    list(pp_counts = by_date, pp_cumsum = accomulate)
}


# ### Header Looks like
# ##collection_date;sample_id;library_id;QPass;RawReads;PercCov;DepthOfCoverage;NCClade;PassReads;WeekNumber
# require(dplyr)
#
# provinces<-xlsx::read.xlsx("~/Downloads/16codmun_en.xls",sheetIndex = 1)
# colnames(provinces)<-c("ProvinceCode","MunicipalCode","ComarcaCode","MunicipalName","ProvinceName")
# provinces<-provinces[-1,]
# sum(mergedData$location %in% provinces$MunicipalName) ### All MunicipalNames have a province assigned
# dim(mergedData)
#
# mergedData$week<-as.factor(paste(strftime(mergedData$date, format = "%y"),strftime(mergedData$date, format = "%V"),sep="-"))
# mergedData$month<-as.factor(paste(strftime(mergedData$date, format = "%y"),strftime(mergedData$date, format = "%m"),sep="-"))
# mergedData$year
# mergedData %>%
#     dplyr::filter(country == "Spain") %>%
#     dplyr::select(c(seqName,date,region,country,division,location,sex,qc.overallStatus,aaSubstitutions,clade,lineage))%>%
#     dplyr::rename(collection_date=date,sample_id=seqName,NCClade=clade,WeekNumber=week,)
#
# mergedData$week<-as.factor(paste(strftime(mergedData$date, format = "%y"),strftime(mergedData$date, format = "%V"),sep="-"))
# mergedData$month<-as.factor(paste(strftime(mergedData$date, format = "%y"),strftime(mergedData$date, format = "%m"),sep="-"))
#
# # require(ggplot2)
# # mergedData %>%
# #     dplyr::filter(division=="Catalunya") %>%
# #     dplyr::filter(! is.na(date)) %>%
# # ggplot(aes(x=week,fill=Province))+geom_bar(stat="count")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# #
# # mergedData %>%
# #     dplyr::filter(division=="Catalunya") %>%
# #     dplyr::filter(! is.na(date)) %>%
# #     ggplot(aes(x=month,fill=Province))+geom_bar(stat="count")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# #
# # require(viridis)
# # mergedData %>%
# #     dplyr::filter(country=="Spain") %>%
# #     dplyr::filter(! is.na(date)) %>%
# #     ggplot(aes(x=month,fill=division))+geom_bar(stat="count")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+scale_color_brewer("Set3")
#






