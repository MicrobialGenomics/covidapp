DROP DATABASE IF EXISTS `covidapp_test`;
CREATE DATABASE `covidapp_test`;
USE `covidapp_test`;

SET NAMES utf8mb4;
SET character_set_client = utf8mb4;


create table `sequences` (
    `seq_idx` mediumint NOT NULL AUTO_INCREMENT PRIMARY KEY,
    `gisaid_collection_date` timestamp NOT NULL,
    `gisaid_submission_date` timestamp NOT NULL,
    `gisaid_sequence` text NOT NULL,
    `gisaid_name`  varchar(50) NOT NULL,
    `gisaid_accession` varchar(50) NOT NULL,
    `gisaid_vaccine` varchar(10) NOT NULL,
    `gisaid_originating_lab` text NOT NULL,
    `gisaid_variant` varchar(10) NOT NULL,
    `host` varchar(50) NOT NULL,
    `NC_variant_idx` mediumint,
    `PG_variant_idx` mediumint,
    FOREIGN KEY (`NC_variant_idx`) REFERENCES `NC_variants` (`NC_variant_idx`) ON UPDATE CASCADE,
    FOREIGN KEY (`PG_variant_idx`) REFERENCES `PG_variants` (`PG_variant_idx`) ON UPDATE CASCADE,
)

create table `PG_variants` (
    `PG_variant_idx` mediumint NOT NULL AUTO INCREMENT PRIMARY KEY,

)
create table `NC_variants`(
    `NC_variant_idx` mediumint NOT NULL AUTO INCREMENT PRIMARY KEY,
)

create table `Mutations`(
    `mutation_idx` mediumint NOT NULL AUTOINCREMENT PRIMARY KEY,
    `mutation_string` varchar(10) NOT NULL,
    `mutation_protein` varchar(10) NOT NULL
)

create table `PG_variants_mutations`(
    `PG_variants_mutations_idx` mediumint NOT NULL AUTO INCREMENT PRIMARY KEY,
    `PG_variant_idx` mediumint,
    `mutation_idx` mediumint,
    FOREIGN KEY (`PG_variant_idx`) REFERENCES `PG_variants` (`PG_variant_idx`) ON UPDATE CASCADE,
    FOREIGN KEY (`mutation_idx`) REFERENCES `Mutations` (`mutation_idx`) ON UPDATE CASCADE
)

create table `NC_variants_mutations`(
    `NC_variants_mutations_idx` mediumint NOT NULL AUTO INCREMENT PRIMARY KEY,
    `NC_variant_idx` mediumint NOT NULL,
    `mutation_idx` mediumint NOT NULL,
    FOREIGN KEY (`NC_variant_idx`) REFERENCES `NC_variants` (`NC_variant_idx`) ON UPDATE CASCADE,
    FOREIGN KEY (`mutation_idx`) REFERENCES `Mutations` (`mutation_idx`) ON UPDATE CASCADE

)

create table `Mutation_comments`(
    `mutation_comment_idx` mediumint NOT NULL AUTO INCREMENT PRIMARY KEY,
    `mutation_idx` mediumint NOT NULL,
    `mutation_comment_type` varchar(10) NOT NULL,
    `mutation_comment_text` text NOT NULL,
    FOREIGN KEY (`mutation_idx`) REFERENCES `Mutations` (`mutation_idx`) ON UPDATE CASCADE
)

create table Variants(
    `variant_idx` mediumint NOT NULL AUTO INCREMENT PRIMARY KEY,
    `NC_variant_idx` mediumint NOT NULL,
    `PG_variant_idx` mediumint NOT NULL
    FOREIGN KEY (`NC_variant_idx`) REFERENCES `NC_variants` (`NC_variant_idx`) ON UPDATE CASCADE,
    FOREIGN KEY (`PG_variant_idx`) REFERENCES `PG_variants` (`PG_variant_idx`) ON UPDATE CASCADE
)
create table `Variant_comments`(
    `variant_comment_idx` mediumint NOT NULL AUTO INCREMENT PRIMARY KEY,
    `variant_idx` mediumint NOT NULL,
    `variant_comment_type` varchar(10) NOT NULL,
    `variant_comment_text` text NOT NULL,
    FOREIGN KEY (`variant_idx`) REFERENCES `Variants` (`variant_idx`) ON UPDATE CASCADE
)
