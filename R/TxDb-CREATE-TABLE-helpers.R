### =========================================================================
### Build CREATE TABLE statements
### -------------------------------------------------------------------------
###
### Nothing in this file is exported.


.build_SQL_CREATE_TABLE <- function(table, coldefs, constraints=NULL)
{
    SQL <- "CREATE TABLE %s (%s\n)"
    coldefs <- c(paste(names(coldefs), coldefs), constraints)
    coldefs <- paste("\n  ", coldefs, collapse=",")
    sprintf(SQL, table, coldefs)
}

build_SQL_CREATE_chrominfo_table <- function()
{
    .build_SQL_CREATE_TABLE("chrominfo", TXDB_CHROMINFO_COLDEFS)
}

build_SQL_CREATE_feature_table <- function(table)
{
    columns <- TXDB_COLUMNS[[table]]
    coldefs <- setNames(TXDB_FEATURE_COLDEFS[names(columns)], columns)
    foreign_key <- sprintf("FOREIGN KEY (%s) REFERENCES chrominfo (chrom)",
                           columns[["chrom"]])
    .build_SQL_CREATE_TABLE(table, coldefs, foreign_key)
}

build_SQL_CREATE_splicing_table <- function()
{
    unique_key <- "UNIQUE (_tx_id, exon_rank)"
    foreign_keys <- sprintf("FOREIGN KEY (_%s_id) REFERENCES %s",
                            c("tx", "exon", "cds"),
                            c("transcript", "exon", "cds"))
    constraints <- c(unique_key, foreign_keys)
    .build_SQL_CREATE_TABLE("splicing", TXDB_SPLICING_COLDEFS, constraints)
}

build_SQL_CREATE_gene_table <- function()
{
    unique_key <- "UNIQUE (gene_id, _tx_id)"
    foreign_key <- "FOREIGN KEY (_tx_id) REFERENCES transcript"
    constraints <- c(unique_key, foreign_key)
    .build_SQL_CREATE_TABLE("gene", TXDB_GENE_COLDEFS, constraints)
}

