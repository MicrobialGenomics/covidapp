### This script is intended to run after data has been filtered and analyzed with NextClade and Pangolin

### Trying to do it well!

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(optparse)
library(aws.s3)

# Arguments ---------------------------------------------------------------
option_list <- list(
    optparse::make_option(
        c("-c", "--nextclade"),
        type = "character",
        default = "",
        help = "Semicolon-separated file with nextclade results",
        metavar = "file"
    ),
    optparse::make_option(
        c("-p", "--pangolin"),
        type = "character",
        default = "",
        help = "Comma-separated file with pangolin results",
        metavar = "file"
    ),
    optparse::make_option(
        c("-m", "--metadata"),
        type = "character",
        default = "",
        help = "Comma-separated file with metadata information",
        metavar = "file"
    ),
    optparse::make_option(
        c("-o", "--out_dir"),
        type = "character",
        default = "./",
        help = "Output directory",
        metavar = "path"
    )
)

opt_parser <- optparse::OptionParser(option_list = option_list)
opt <- optparse::parse_args(opt_parser)

# Load file result data ---------------------------------------------------
# If file is null or not exist download from aws --------------------------
bucket <- "s3://covidseq-14012021-eu-west-1"
aws_objects <- bucket %>%
    aws.s3::get_bucket_df() %>%
    filter(str_detect(Key, "GISAID/DataFiles"))

if(!file.exists(opt$pangolin) | is.null(opt$pangolin)) {
    pangolin <- aws_objects %>%
        filter(str_detect(Key, "Pangolin")) %>%
        arrange(desc(LastModified)) %>%
        slice_head(n = 1) %>%
        pull(Key) %>%
        aws.s3::s3read_using(object = .,
                             FUN = readr::read_csv,
                             col_types = cols(),
                             bucket = bucket,) %>%
        rename(seqName = taxon)
} else {
    pangolin <- opt$pangolin %>% read_csv() %>% rename(seqName = taxon)
}

if(!file.exists(opt$metadata) | is.null(opt$metadata)) {
    gisaidcore <- aws_objects %>%
        filter(str_detect(Key, "metadata")) %>%
        arrange(desc(LastModified)) %>%
        slice_head(n = 1) %>%
        pull(Key) %>%
        aws.s3::s3read_using(object = .,
                             FUN = readr::read_tsv,
                             col_types = cols(),
                             bucket = bucket,) %>%
        rename(seqName = strain)
} else {
    gisaidcore <- opt$metadata %>%
        read_tsv() %>%
        hablar::retype() %>%
        rename(seqName = strain)
}

if (!file.exists(opt$nextclade) | is.null(opt$nextclade)) {
    nextclade <- aws_objects %>%
        filter(str_detect(Key, "NextClade")) %>%
        arrange(desc(LastModified)) %>%
        slice_head(n = 1) %>%
        pull(Key) %>%
        aws.s3::s3read_using(object = .,
                             FUN = readr::read_delim,
                             delim = ";",
                             col_types = cols(),
                             bucket = bucket)
} else {
    nextclade <- opt$nextclade %>% read_delim(delim = ";")
}

# Merge and export --------------------------------------------------------
mergedData <- gisaidcore %>%
    left_join(nextclade, by = "seqName") %>%
    left_join(pangolin, by = "seqName") %>%
    rename(collection_date = date, NCClade = clade) %>%
    filter(country == "Spain") %>%
    drop_na(collection_date) %>%
    mutate(
        collection_date = as.Date(collection_date, format = "%Y-%m-%d"),
        pct_cov = length - (qc.missingData.totalMissing / length) * 100,
        qpass = if_else(pct_cov > 80, "Yes", "No"),
        week_num = strftime(collection_date, format = "%y-%V"),
        week_num = if_else(week_num == "21-53", "20-53", week_num),
        acom_name = dplyr::case_when(
            division == "Andalusia" ~ "Andalucía",
            division == "Aragon" ~ "Aragón",
            division == "Asturias" ~ "Principado de Asturias",
            division == "Balear Islands" ~ "Illes Balears",
            division == "Basque Country" ~ "País Vasco",
            division == "Canary Islands" ~ "Canarias",
            division == "Castilla la Mancha" ~ "Castilla-La Mancha",
            division == "Castilla y Leon" ~ "Castilla y León",
            division == "Catalunya" ~ "Cataluña",
            division == "Ceuta" ~ "Ciudad Autónoma de Ceuta",
            division == "Melilla" ~ "Ciudad Autónoma de Melilla",
            division == "Madrid" ~ "Comunidad de Madrid",
            division == "Navarra" ~ "Comunidad Foral de Navarra",
            division == "Murcia" ~ "Región de Murcia",
            TRUE ~ division
        )
    )

readr::write_rds(
    mergedData,
    file = str_c(opt$out_dir, "/MergedData_spain.rds"),
)
