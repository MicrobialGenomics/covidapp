DROP DATABASE IF EXISTS `covidapp_test`;
CREATE DATABASE `covidapp_test`;
USE `covidapp_test`;

SET NAMES utf8mb4;
SET character_set_client = utf8mb4;


create table `Sequences` (
    `sequence_idx` mediumint NOT NULL AUTO_INCREMENT PRIMARY KEY,
    `gisaid_collection_date` timestamp NOT NULL,
    `gisaid_submission_date` timestamp NOT NULL,
-- gisaid_name is like Spain/CT-HUVH-91601/2020
    `gisaid_name`  varchar(50) NOT NULL,
-- gisaid_accession is like EPI_ISL_419660
    `gisaid_accession` varchar(50) NOT NULL,
    `gisaid_vaccine` varchar(10) NOT NULL,
    `gisaid_originating_lab` text NOT NULL,
-- gisaid classification follows different nomenclature

    `gisaid_variant` varchar(10) NOT NULL,
    `gisaid_region` varchar(20) NOT NULL,
    `gisaid_region` varchar(20) NOT NULL
-- Only use Human Host sequences. value=="Human"
    `gisaid_host` varchar(50) NOT NULL,
    `NC_variant_idx` mediumint,
    `NC_qc.overallScore` varchar(10),
-- NC_qc.overallStatus has three different values "bad","mediocre", "good" as reported by NextClade
    `NC_qc.overallStatus` varchar(10),
-- NC_substitutions has the form
-- C241T,C823T,C3037T,C14408T,A20268G,A23403G,G29734C
-- Not used by now.
    `NC_substitutions` text,
    `NC_deletions` text,
    `NC_insertions` text,
-- NC_aasubstitutions has the form
-- ORF1a:P892L,ORF1a:S2193T,ORF1b:P314L,S:D614G
-- It should be parsed to populate mutations and sequences_mutations
    `NC_aaSubstitutions` text,
-- NC_aaDeletions has the form
-- E:V47-,E:N48-,E:V49-. What to do wth them?
    `NC_aaDeletions` text,
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

create table `Sequences_mutations`(
    `sequence_mutation_idx` mediumint NOT NULL AUTO INCREMENT PRIMARY KEY,
    `sequence_idx` mediumint NOT NULL,
    `mutation_idx` mediumint NOT NULL,
     FOREIGN KEY (`mutation_idx`) REFERENCES `Mutations` (`mutation_idx`) ON UPDATE CASCADE,
     FOREIGN KEY (`sequence_idx`) REFERENCES `Sequences` (`sequence_idx`) ON UPDATE CASCADE,

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

