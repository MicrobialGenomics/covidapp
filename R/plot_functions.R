#' Pre-processing variants data
#'
#' @return tibble
#' @export
prepro_variants <- function(df) {
    # Pre-processing data
    df %>%
        tidyr::drop_na(week_num) %>%
        dplyr::mutate(NCClade = factor(NCClade, levels = unique(NCClade))) %>%
        dplyr::group_by(week_num) %>%
        dplyr::select(week_num, NCClade) %>%
        dplyr::count(NCClade, .drop = FALSE) %>%
        dplyr::summarise(freq = n / sum(n),
                         pct = freq * 100,
                         counts = n,
                         sum = sum(counts),
                         NCClade = NCClade)
}

#' Plot variants
#'
#' @param df output of function prepro_variants
#' @param type Class of plot. Options: density or bar
#' @param var variable to plot. Options: "freq" and "counts"
#' @param pal Used palette
#' @param pal_dir Palete direction order. Options: 1 and -1
#' @param NCClade List with NCClade to plot
#'
#' @return plotly object
#' @export
plot_vairants <- function(df,
                          type = "density",
                          var = "freq",
                          pal = "mg",
                          pal_dir = -1,
                          NCClade = NULL) {

    # filter NCClade
    if (!is.null(NCClade)) {
        df <- df %>% dplyr::filter(NCClade %in% NCClade)
    }

    # define order by median
    ord <- df %>%
        dplyr::group_by(NCClade) %>%
        dplyr::summarise(mean = mean(!!sym(var))) %>%
        dplyr::pull(mean) %>%
        sort()

    # Text label for plotly
    df <- df %>%
        dplyr::mutate(NCClade = forcats::fct_reorder(NCClade, rev(ord)),
                      text = stringr::str_c(
                          "NCClade:", NCClade,
                          "<br>frequency:", freq,
                          "<br>percentage:", pct,
                          "<br>count:", counts,
                          "<br>total:", sum,
                          sep = " "
                      ))

    # Define plot class
    if (type == "density") {
        pp <- df %>%
            ggplot(aes(week_num, !!sym(var), fill = NCClade, group = NCClade, text = text)) +
            geom_density(stat = "identity", position = "stack", alpha = 0.5, colour = NA)
    } else if (type == "bar") {
        pp <- df %>%
            ggplot(aes(week_num, !!sym(var), fill = NCClade, group = NCClade, text = text)) +
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
                 "#D55E00", "#CC79A7","darkred","darkgreen","steelblue")
        pp <- pp + scale_fill_manual(values = pal)
    }

    ## Converting to plotly
    # title <- list(text = stringr::str_c(
    #     glue::glue("SARS-CoV-2 Variant {t_1} by Week, Full GISAID"),
    #     "<br>", "<sup>", "Feb 10th (Spain)", "</sup>"
    # ))

    plotly::ggplotly(pp, tooltip = "text") %>%
        plotly::layout(
            hovermode = 'compare',
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






