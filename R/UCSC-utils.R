### =========================================================================
### Utilities for fetching data from UCSC
### -------------------------------------------------------------------------
###


.UCSC_REST_API_URL <- "https://api.genome.ucsc.edu"

.cached_rest_api_results <- new.env(parent=emptyenv())


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### list_UCSC_genomes()
###

.get_UCSC_genomes <- function(recache=FALSE)
{
    if (!isTRUEorFALSE(recache))
        stop(wmsg("'recache' must be TRUE or FALSE"))
    ans <- .cached_rest_api_results[["ucscGenomes"]]
    if (is.null(ans) || recache) {
        url <- .UCSC_REST_API_URL
        response <- GET(url, path="list/ucscGenomes")
        if (response$status_code != 200L)
            stop(wmsg("failed to retrieve list of UCSC genomes"))
        json <- content(response, as="text", encoding="UTF-8")
        ans <- fromJSON(json)[["ucscGenomes"]]
        stopifnot(is.list(ans))  # sanity check
        .cached_rest_api_results[["ucscGenomes"]] <- ans
    }
    ans
}

### Returns a data.frame with 1 row per genome and 5 columns:
### organism, genome, common_name, tax_id, description.
### A few things align with GenomeInfoDb::registered_UCSC_genomes():
###   - colnames "organism" and "genome" on the returned data frame;
###   - column "organism" returned as a factor;
###   - name, default value, and behavior of 'organism' argument, but with
###     the difference that we also search matches in the "common_name"
###     column (in addition to matches in the "organism" column);
###   - order of rows in the returned data frame.
list_UCSC_genomes <- function(organism=NA, recache=FALSE)
{
    if (!isSingleStringOrNA(organism))
        stop(wmsg("'organism' must be a single string or NA"))
    genomes <- .get_UCSC_genomes(recache=recache)

    ans_organism <- factor(vapply(genomes,
        function(genome) {
            stopifnot(is.list(genome))  # sanity check
            genome$scientificName
        },
        character(1), USE.NAMES=FALSE
    ))
    ans_common_name <- factor(vapply(genomes,
        function(genome) genome$organism,
        character(1), USE.NAMES=FALSE
    ))
    if (!is.na(organism)) {
        keep_idx <- which(grepl(organism, ans_organism, ignore.case=TRUE) |
                          grepl(organism, ans_common_name, ignore.case=TRUE))
        genomes <- genomes[keep_idx]
        ans_organism <- ans_organism[keep_idx]
        ans_common_name <- ans_common_name[keep_idx]
    }

    ans_genome <- names(genomes)
    ans_tax_id <- vapply(genomes,
        function(genome) as.integer(genome$taxId),
        integer(1), USE.NAMES=FALSE
    )
    ans_description <- vapply(genomes,
        function(genome) genome$description,
        character(1), USE.NAMES=FALSE
    )

    ans <- data.frame(
        organism=ans_organism,
        genome=ans_genome,
        common_name=ans_common_name,
        tax_id=ans_tax_id,
        description=ans_description
    )
    oo <- GenomeInfoDb:::order_organism_genome_pairs(ans$organism, ans$genome)
    S4Vectors:::extract_data_frame_rows(ans, oo)
}

### Convenience helper based on list_UCSC_genomes().
get_organism_for_UCSC_genome <- function(genome)
{
    if (!(isSingleString(genome) && nzchar(genome)))
        stop(wmsg("'genome' must be a single (non-empty) string"))
    df <- list_UCSC_genomes()
    idx <- match(genome, df$genome)
    if (is.na(idx))
        stop(wmsg(genome, ": unknown UCSC genome"))
    as.character(df$organism[idx])
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### list_UCSC_primary_tables()
###

.get_UCSC_genome_tracks <- function(genome, recache=FALSE)
{
    if (!(isSingleString(genome) && nzchar(genome)))
        stop(wmsg("'genome' must be a single (non-empty) string"))
    if (!isTRUEorFALSE(recache))
        stop(wmsg("'recache' must be TRUE or FALSE"))
    ans <- .cached_rest_api_results[[genome]]
    if (is.null(ans) || recache) {
        url <- .UCSC_REST_API_URL
        response <- GET(url, path="list/tracks", query=list(genome=genome))
        if (response$status_code != 200L)
            stop(wmsg(genome, ": unknown UCSC genome (or ", url, " is down?)"))
        json <- content(response, as="text", encoding="UTF-8")
        ans <- fromJSON(json)[[genome]]
        stopifnot(is.list(ans))  # sanity check
        .cached_rest_api_results[[genome]] <- ans
    }
    ans
}

### Typical usage:
###     list_UCSC_primary_tables("ce2")
###     list_UCSC_primary_tables("mm9", track_group="genes")
###     list_UCSC_primary_tables("hg38", track_group=NA)
### Returns a data.frame with 1 row per primary table and 5 columns:
### primary_table, track, type, track_group, composite_track.
### Note that the "track_group" and "composite_track" columns can contain NAs.
### Passing 'track_group=NA' is accepted and keeps only rows for tracks that
### don't belong to any group. This is why default value for the 'track_group'
### argument is NULL and not NA like for the 'organism' argument in
### list_UCSC_genomes() above.
list_UCSC_primary_tables <- function(genome, track_group=NULL, recache=FALSE)
{
    if (!(is.null(track_group) || isSingleStringOrNA(track_group)))
        stop(wmsg("'track_group' must be a single string, or NA, or NULL"))
    genome_tracks <- .get_UCSC_genome_tracks(genome, recache=recache)

    track_groups <- vapply(genome_tracks,
        function(track) {
            stopifnot(is.list(track))  # sanity check
            idx <- match("group", names(track))
            if (is.na(idx)) NA_character_ else track[[idx]]
        },
        character(1), USE.NAMES=FALSE
    )
    if (!is.null(track_group)) {
        keep_idx <- which(track_groups %in% track_group)
        genome_tracks <- genome_tracks[keep_idx]
        track_groups <- track_groups[keep_idx]
    }

    track_names <- vapply(genome_tracks,
        function(track) track$shortLabel,
        character(1), USE.NAMES=FALSE
    )
    track_types <- vapply(genome_tracks,
        function(track) track$type,
        character(1), USE.NAMES=FALSE
    )

    ## Extract tracks nested in composite tracks.
    is_composite <- vapply(genome_tracks,
        function(track) identical(track$compositeTrack, "on"),
        logical(1), USE.NAMES=FALSE
    )
    nested_tracks <- lapply(genome_tracks[is_composite],
        function(track) {
            track[vapply(track, is.list, logical(1), USE.NAMES=FALSE)]
        }
    )
    nested_primary_tables <- lapply(nested_tracks, names)
    nested_track_names <- lapply(nested_tracks,
        function(tracks) vapply(tracks,
                                function(track) track$shortLabel,
                                character(1), USE.NAMES=FALSE)
    )
    nested_track_types <- lapply(nested_tracks,
        function(tracks) vapply(tracks,
                                function(track) track$type,
                                character(1), USE.NAMES=FALSE)
    )
    nested_tracks_count <- lengths(nested_tracks)

    ## Sanity checks.
    stopifnot(
        identical(lengths(nested_primary_tables), nested_tracks_count),
        identical(lengths(nested_track_names), nested_tracks_count),
        identical(lengths(nested_track_types), nested_tracks_count)
    )

    ## Prepare columns of final data frame.
    times <- rep.int(1L, length(genome_tracks))
    times[is_composite] <-  nested_tracks_count
    ans_is_composite <- rep.int(is_composite, times)
    ans_primary_table <- rep.int(names(genome_tracks), times)
    ans_primary_table[ans_is_composite] <-
        unlist(nested_primary_tables, use.names=FALSE)
    stopifnot(anyDuplicated(ans_primary_table) == 0L)  # sanity check
    ans_track <- ans_composite_track <- rep.int(track_names, times)
    ans_track[ans_is_composite] <-
        unlist(nested_track_names, use.names=FALSE)
    ans_type <- rep.int(track_types, times)
    ans_type[ans_is_composite] <-
        unlist(nested_track_types, use.names=FALSE)
    ans_group <- rep.int(track_groups, times)
    ans_composite_track[!ans_is_composite] <- NA_character_

    data.frame(
        primary_table=ans_primary_table,
        track=ans_track,
        type=ans_type,
        track_group=ans_group,
        composite_track=ans_composite_track
    )
}


### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### UCSC_dbselect()
###

### See https://genome.ucsc.edu/goldenpath/help/mysql.html for how to connect
### to a MySQL server at UCSC.
### Here is an example of how to query the server on the US west coast from
### the Unix command line:
###
###   mysql --user=genome --host=genome-mysql.soe.ucsc.edu mm10 -e "select count(*) from knownToLocusLink;"
###
### By default UCSC_dbselect() uses the server located on the US west coast.
UCSC_dbselect <- function(dbname, from, columns=NULL, where=NULL,
                          server="genome-mysql.soe.ucsc.edu")
{
    columns <- if (is.null(columns)) "*" else paste0(columns, collapse=",")
    SQL <- sprintf("SELECT %s FROM %s", columns, from)
    if (!is.null(where)) {
        stopifnot(isSingleString(where))  # sanity check
        SQL <- paste(SQL, "WHERE", where)
    }
    dbconn <- dbConnect(RMariaDB::MariaDB(), dbname=dbname,
                                             username="genome",
                                             host=server,
                                             port=3306)
    on.exit(dbDisconnect(dbconn))
    dbGetQuery(dbconn, SQL)
}

