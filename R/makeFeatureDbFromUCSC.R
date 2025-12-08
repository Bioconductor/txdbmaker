### makeFeatureDbFromUCSC() expects a UCSC table to have at
### least the following columns:
.UCSC_GENERICCOL2CLASS <- c(
    chrom="factor",
    strand="factor",
    chromStart="integer",
    chromEnd="integer"
)

## helper function to add missing strand information to table (if missing)
.addMissingStrandCols <- function(table){
  if(!"strand" %in% colnames(table)){
    strand <- rep("*", dim(table)[1])
    return(cbind(table,strand))
  }else{
    return(table)
  }
}

## helper function to correct for UCSC data having off by one start info.
.adjustchromStarts <- function(table){
  chromStart <- as.integer(table[["chromStart"]])
  chromStart <- chromStart + 1L
  table$chromStart <- chromStart
  table
}

## helper function to re-assign column names as required
## if the value has been set and there is precisely ONE col that matches the
## new val, then go ahead and rename it to the chrom, chromStart or chromEnd.
.renamColsHelper <- function(table, col, newString){
          tmpColNames <- colnames(table)
          tmpColNames[colnames(table)==col] <- newString
          colnames(table) <- tmpColNames
          table
}

.checkAndRenamCols <- function(table, chromCol, chromStartCol, chromEndCol){
    if(!is.null(chromCol) && table(colnames(table)==chromCol)[["TRUE"]]==1) {
          table <-.renamColsHelper(table, chromCol, "chrom")
    }
    if(!is.null(chromStartCol)
        && table(colnames(table)==chromStartCol)[["TRUE"]]==1){
          table <- .renamColsHelper(table, chromStartCol, "chromStart")
    }
    if(!is.null(chromEndCol)
        && table(colnames(table)==chromEndCol)[["TRUE"]]==1){
          table <-.renamColsHelper(table, chromEndCol, "chromEnd")
    }
  table
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### prepare and write out the DB table contents
###

.prepareUCSCFeatureMetadata <- function(genome, tablename, taxonomyId)
{
    message("Prepare the 'metadata' data frame ... ",
            appendLF=FALSE)
    organism <- get_organism_for_UCSC_genome(genome)
    if (is.na(taxonomyId)) {
        taxonomyId <- GenomeInfoDb:::lookup_tax_id_by_organism(organism)
    } else {
        GenomeInfoDb:::check_tax_id(taxonomyId)
    }

    metadata <- data.frame(
        name=c("Data source", "Genome", "UCSC Table", "Organism",
               "Taxonomy ID", "Resource URL"),
        value=c("UCSC", genome, tablename, organism,
                taxonomyId, "https://genome.ucsc.edu/")
    )
    message("OK")
    metadata
}

### Some of the metadata is added later.
.writeMetadataFeatureTable <- function(conn, metadata, tableName) {
    data_nrow <- dbEasyQuery(conn, paste("SELECT COUNT(*) FROM ",tableName,
                                         collapse=""))[[1L]]
    thispkg_version <- packageDescription("txdbmaker")$Version
    rsqlite_version <- packageDescription("RSQLite")$Version
    mat <- matrix(c(
        DB_TYPE_NAME, "FeatureDb",
        "Supporting package", "GenomicFeatures",
        "data_nrow", data_nrow,
        "Db created by", "txdbmaker package from Bioconductor",
        "Creation time", svn.time(),
        "txdbmaker version at creation time", thispkg_version,
        "RSQLite version at creation time", rsqlite_version,
        "DBSCHEMAVERSION", DB_SCHEMA_VERSION),
        ncol=2, byrow=TRUE
    )
    colnames(mat) <- c("name", "value")
    metadata <- rbind(data.frame(name=mat[ , "name"], value=mat[ , "value"],
                                 stringsAsFactors=FALSE),
                      metadata)
    dbWriteTable(conn, "metadata", metadata, row.names=FALSE)
}


## The following writes the data contents of our generic table
.writeGenericFeatureTable <- function(conn, data, tableName, columns)
{
    data <- unique(data)
    ## for now just drop lines that don't have values for chromStart
    ## we may need to be more stringent
    data <- S4Vectors:::extract_data_frame_rows(data, !is.na(data$chromStart))
    ## Create the table.
    sql1 <- c("CREATE TABLE ",tableName," (\n",
              "  chrom TEXT NOT NULL,\n",
              "  strand TEXT NOT NULL,\n",
              "  chromStart INTEGER NOT NULL,\n",
              "  chromEnd INTEGER NOT NULL,\n") ## always a comma = never done
    ## Add remaining rows (b/c there will ALWAYS be at least one "other" field)
    sql2 <- paste("  ", names(columns), " TEXT,\n")
    sql <- c(sql1, sql2,")")
    ## remove final comma
    sql[length(sql)-1] <- sub(",","",sql[length(sql)-1])
    dbExecute(conn, paste(sql, collapse=""))
    ## Fill the table.
    insert_data_into_table(conn, tableName, data)
}

## Discovery for supported tracks and associated primary tables.
supportedUCSCFeatureDbTracks <- function(genome)
{
  msg <- c("supportedUCSCFeatureDbTracks() is deprecated. Please use ",
           "list_UCSC_tracks() from the UCSC.utils package instead.")
  .Deprecated(msg=wmsg(msg))
  list_UCSC_tracks(genome)
}

## Discover table names available in Tracks
supportedUCSCFeatureDbTables <- function(genome, track)
{
  msg <- c("supportedUCSCFeatureDbTables() is deprecated. Please use ",
           "list_UCSC_tracks() from the UCSC.utils package instead.")
  .Deprecated(msg=wmsg(msg))
  list_UCSC_tracks(genome)
}

## Return the schema information (field names and types) for a given
## table.
UCSCFeatureDbTableSchema <- function(genome,
                                     track,
                                     tablename)
{
  if (!missing(track))
      .Deprecated(msg="the 'track' argument in deprecated")
  df <- UCSC_dbselect(genome, tablename, MoreSQL="LIMIT 0")
  col2Rtype <- vapply(df, function(col) class(col)[[1L]], character(1))
  col2Rtype[col2Rtype == "blob"] <- "character"
  col2Rtype
}

## Convert SQL types to R types by creating a
## dummy SQL table and reading it's type information
map_SQLtypes_to_Rtypes <- function(SQLtypes)
{
    stopifnot(is.character(SQLtypes))
    SQLtypes <- gsub(" *unsigned", "", SQLtypes)
    SQLtypes <- gsub("^enum\\(.*\\)$", "TEXT", SQLtypes)
    col_defs <- sprintf("col%d %s", seq_along(SQLtypes), SQLtypes)
    sql <- sprintf("CREATE TABLE dummy (%s)", paste0(col_defs, collapse=", "))
    conn <- dbConnect(SQLite())
    on.exit(dbDisconnect(conn))
    dbExecute(conn, sql)
    dummy <- dbReadTable(conn, "dummy")
    col_types <- vapply(dummy, function(col) class(col)[[1L]], character(1))
    # edge case
    col_types[col_types == "blob"] <- "character"
    setNames(col_types, SQLtypes)
}

## I will need a function to actually make the DB
makeFeatureDb <- function(data, tableName, columns, metadata=NULL, ...)
{
    ## Create the db in a temp file.
    conn <- dbConnect(SQLite(), dbname="")
    .writeGenericFeatureTable(conn, data, tableName, columns)
    .writeMetadataFeatureTable(conn, metadata, tableName)  # must come last!
    GenomicFeatures:::FeatureDb(conn)
}



## standard columns are chrom, chromStart, chromEnd and strand
## all others need to be specified
makeFeatureDbFromUCSC <- function(genome,
         track,
         tablename,
         columns=UCSCFeatureDbTableSchema(genome, track, tablename),
         url="https://genome.ucsc.edu/cgi-bin/",
         goldenPath.url=getOption("UCSC.goldenPath.url"),
         chromCol=NULL,
         chromStartCol=NULL,
         chromEndCol=NULL,
         taxonomyId=NA)
{
    if (!isSingleString(genome))
        stop("'genome' must be a single string")
    if (!missing(track))
        .Deprecated(msg="the 'track' argument in deprecated")
    if (!isSingleString(tablename))
        stop("'tablename' must be a single string")
    if (!missing(url))
        .Deprecated(msg="the 'url' argument in deprecated")
    if (!missing(goldenPath.url))
        .Deprecated(msg="the 'goldenPath.url' argument in deprecated")

    ## Check the column names
    if (anyDuplicated(names(columns)))
        stop("The default field names are not unique for this table.")
    ## Once we know the columns names are unique, we remove the default ones.
    columns <- columns[!(names(columns) %in% names(.UCSC_GENERICCOL2CLASS))]
    ## also have to remove any columns that are to be re-assigned!
    if(!is.null(chromCol) || !is.null(chromStartCol) || !is.null(chromEndCol)){
      ## if I concatenate to a vector, the NULL values will be MIA = perfect
      altCols <- c(chromCol,chromStartCol,chromEndCol)
      columns <- columns[!(names(columns) %in% altCols )]
    }

    ## Download the data table.
    message("Download the ", tablename, " table ... ", appendLF=FALSE)
    ucsc_table <- UCSC_dbselect(genome, tablename)

    ## check that we have strand info, and if not, add some in
    ucsc_table <- .addMissingStrandCols(ucsc_table)
    ## TODO: do any required substitutions (required column renames)
    ucsc_table <- .checkAndRenamCols(ucsc_table,
                                     chromCol,
                                     chromStartCol,
                                     chromEndCol)


    ## check that we have at least the 5 columns of data
    if(ncol(ucsc_table) < length(.UCSC_GENERICCOL2CLASS)+1)
        stop(wmsg("txdbmaker internal error: ", tablename, " table doesn't ",
                  "exist, was corrupted during download, or doesn't contain ",
                  "sufficient information. ",
                  "Please report the issue at ",
                  "https://github.com/Bioconductor/txdbmaker/issues, ",
                  "and sorry for the inconvenience."))
    message("OK")
    ## also check that our data CONTAINS the column names we need it to


    message("Checking that required Columns are present ... ")
    if(length(intersect(colnames(ucsc_table),names(.UCSC_GENERICCOL2CLASS)))<4)
        ## That means that some required cols are missing!
        stop(wmsg("txdbmaker internal error: ", tablename, " table doesn't ",
                  "contain a 'chrom', 'chromStart', or 'chromEnd' column, ",
                  "and no reasonable substitute has been designated via ",
                  "the 'chromCol', 'chromStartCol', or 'chromEndCol' ",
                  "argument. If this is not possible, please report ",
                  "that fact by opening an issue at ",
                  "https://github.com/Bioconductor/txdbmaker/issues. ",
                  "Thank you."))
    message("OK")


    ## Then adjust the chromStarts column (corrects for unusual UCSC counting)
    ucsc_table <- .adjustchromStarts(ucsc_table)


    ## then make our table, but remember, we have to add new columns to our
    ## base table type 1st:
    .UCSC_GENERICCOL2CLASS <- c(.UCSC_GENERICCOL2CLASS, columns)
    ucsc_table <- setDataFrameColClass(ucsc_table ,.UCSC_GENERICCOL2CLASS,
                                       drop.extra.cols=TRUE)
    ## ensure that the table columns conform to expectations
    ucsc_table <- ucsc_table[,names(.UCSC_GENERICCOL2CLASS)]

    ## Compile some of the metadata
    metadata <- .prepareUCSCFeatureMetadata(genome, tablename, taxonomyId)

    message("Make the AnnoDb object ... ")
    makeFeatureDb(data=ucsc_table, tableName=tablename,
                  metadata=metadata,
                  columns)
}


