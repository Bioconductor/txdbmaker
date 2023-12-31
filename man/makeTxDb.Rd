\name{makeTxDb}

\alias{makeTxDb}

\title{
  Making a TxDb object from user supplied annotations
}

\description{
  \code{makeTxDb} is a low-level constructor for making a
  \link[GenomicFeatures]{TxDb} object from user supplied transcript
  annotations.

  Note that the end user will rarely need to use \code{makeTxDb} directly
  but will typically use one of the high-level constructors
  \code{\link{makeTxDbFromUCSC}}, \code{\link{makeTxDbFromEnsembl}},
  or \code{\link{makeTxDbFromGFF}}.
}

\usage{
makeTxDb(transcripts, splicings, genes=NULL,
         chrominfo=NULL, metadata=NULL,
         reassign.ids=FALSE, on.foreign.transcripts=c("error", "drop"))
}

\arguments{
  \item{transcripts}{
    Data frame containing the genomic locations of a set of transcripts.
  }
  \item{splicings}{
    Data frame containing the genomic locations of exons and CDS parts of
    the transcripts in \code{transcripts}.
  }
  \item{genes}{
    Data frame containing the genes associated to a set of transcripts.
  }
  \item{chrominfo}{
    Data frame containing information about the chromosomes hosting
    the set of transcripts.
  }
  \item{metadata}{
    2-column data frame containing meta information about this set of
    transcripts like organism, genome, UCSC table, etc...
    The names of the columns must be \code{"name"} and \code{"value"}
    and their type must be character.
  }
  \item{reassign.ids}{
    \code{TRUE} or \code{FALSE}.
    Controls how internal ids should be assigned for each type of feature
    i.e. for transcripts, exons, and CDS parts. For each type, if
    \code{reassign.ids} is \code{FALSE} (the default) and if the ids are
    supplied, then they are used as the internal ids, otherwise the internal
    ids are assigned in a way that is compatible with the order defined by
    ordering the features first by chromosome, then by strand, then by start,
    and finally by end.
  }
  \item{on.foreign.transcripts}{
    Controls what to do when the input contains \emph{foreign transcripts}
    i.e. transcripts that are on sequences not in \code{chrominfo}.
    If set to \code{"error"} (the default)
  }
}

\details{
  The \code{transcripts} (required), \code{splicings} (required)
  and \code{genes} (optional) arguments must be data frames that
  describe a set of transcripts and the genomic features related
  to them (exons, CDS parts, and genes at the moment).
  The \code{chrominfo} (optional) argument must be a data frame
  containing chromosome information like the length of each chromosome.

  \code{transcripts} must have 1 row per transcript and the following
  columns:
  \itemize{
    \item \code{tx_id}: Transcript ID. Integer vector. No NAs. No duplicates.

    \item \code{tx_chrom}: Transcript chromosome. Character vector (or factor)
          with no NAs.

    \item \code{tx_strand}: Transcript strand. Character vector (or factor)
          with no NAs where each element is either \code{"+"} or \code{"-"}.

    \item \code{tx_start}, \code{tx_end}: Transcript start and end.
          Integer vectors with no NAs.

    \item \code{tx_name}: [optional] Transcript name. Character vector (or
          factor). NAs and/or duplicates are ok.

    \item \code{tx_type}: [optional] Transcript type (e.g. mRNA, ncRNA, snoRNA,
          etc...). Character vector (or factor). NAs and/or duplicates are ok.

    \item \code{gene_id}: [optional] Associated gene. Character vector (or
          factor). NAs and/or duplicates are ok.
  }
  Other columns, if any, are ignored (with a warning).

  \code{splicings} must have N rows per transcript, where N is the nb
  of exons in the transcript. Each row describes an exon plus, optionally,
  the CDS part associated with this exon. Its columns must be:
  \itemize{
    \item \code{tx_id}: Foreign key that links each row in the \code{splicings}
          data frame to a unique row in the \code{transcripts} data frame.
          Note that more than 1 row in \code{splicings} can be linked to the
          same row in \code{transcripts} (many-to-one relationship).
          Same type as \code{transcripts$tx_id} (integer vector). No NAs.
          All the values in this column must be present in
          \code{transcripts$tx_id}.

    \item \code{exon_rank}: The rank of the exon in the transcript.
          Integer vector with no NAs. (\code{tx_id}, \code{exon_rank})
          pairs must be unique.

    \item \code{exon_id}: [optional] Exon ID.
          Integer vector with no NAs.

    \item \code{exon_name}: [optional] Exon name.  Character vector (or factor).
          NAs and/or duplicates are ok.

    \item \code{exon_chrom}: [optional] Exon chromosome.
          Character vector (or factor) with no NAs.
          If missing then \code{transcripts$tx_chrom} is used.
          If present then \code{exon_strand} must also be present.

    \item \code{exon_strand}: [optional] Exon strand.
          Character vector (or factor) with no NAs.
          If missing then \code{transcripts$tx_strand} is used
          and \code{exon_chrom} must also be missing.

    \item \code{exon_start}, \code{exon_end}: Exon start and end.
          Integer vectors with no NAs.

    \item \code{cds_id}: [optional] ID of the CDS part associated with the
          exon. Integer vector.
          If present then \code{cds_start} and \code{cds_end} must also
          be present.
          NAs are allowed and must match those in \code{cds_start} and
          \code{cds_end}.

    \item \code{cds_name}: [optional] Name of the CDS part. Character
          vector (or factor).
          If present then \code{cds_start} and \code{cds_end} must also be
          present. NAs and/or duplicates are ok. Must contain NAs at least
          where \code{cds_start} and \code{cds_end} contain them.

    \item \code{cds_start}, \code{cds_end}: [optional] Start/end of the
          CDS part. Integer vectors.
          If one of the 2 columns is missing then all \code{cds_*} columns
          must be missing.
          NAs are allowed and must occur at the same positions in
          \code{cds_start} and \code{cds_end}.

    \item \code{cds_phase}: [optional] Phase of the CDS part. Integer vector.
          If present then \code{cds_start} and \code{cds_end} must also
          be present.
          NAs are allowed and must match those in \code{cds_start} and
          \code{cds_end}.
  }
  Other columns, if any, are ignored (with a warning).

  \code{genes} should not be supplied if \code{transcripts} has a
  \code{gene_id} column. If supplied, it must have N rows per transcript,
  where N is the nb of genes linked to the transcript (N will be 1 most
  of the time). Its columns must be:
  \itemize{
    \item \code{tx_id}: [optional] \code{genes} must have either a
          \code{tx_id} or a \code{tx_name} column but not both.
          Like \code{splicings$tx_id}, this is a foreign key that
          links each row in the \code{genes} data frame to a unique
          row in the \code{transcripts} data frame.

    \item \code{tx_name}: [optional]
          Can be used as an alternative to the \code{genes$tx_id}
          foreign key.

    \item \code{gene_id}: Gene ID. Character vector (or factor). No NAs.
  }
  Other columns, if any, are ignored (with a warning).

  \code{chrominfo} must have 1 row per chromosome and the following
  columns:
  \itemize{
    \item \code{chrom}: Chromosome name.
          Character vector (or factor) with no NAs and no duplicates.

    \item \code{length}: Chromosome length.
          Integer vector with either all NAs or no NAs.

    \item \code{is_circular}: [optional] Chromosome circularity flag.
          Logical vector. NAs are ok.
  }
  Other columns, if any, are ignored (with a warning).
}

\value{A \link[GenomicFeatures]{TxDb} object.}

\author{Hervé Pagès}

\seealso{
  \itemize{
    \item \code{\link{makeTxDbFromUCSC}}, \code{\link{makeTxDbFromBiomart}},
          and \code{\link{makeTxDbFromEnsembl}}, for making a
          \link[GenomicFeatures]{TxDb} object from online resources.

    \item \code{\link{makeTxDbFromGRanges}} and \code{\link{makeTxDbFromGFF}}
          for making a \link[GenomicFeatures]{TxDb} object from a
          \link[GenomicRanges]{GRanges} object, or from a GFF or GTF file.

    \item \link[GenomicFeatures]{TxDb} objects implemented in the
          \pkg{GenomicFeatures} package.

    \item \code{\link[AnnotationDbi]{saveDb}} and
          \code{\link[AnnotationDbi]{loadDb}} in the \pkg{AnnotationDbi}
          package for saving and loading a \link[GenomicFeatures]{TxDb}
          object as an SQLite file.
  }
}

\examples{
transcripts <- data.frame(
                   tx_id=1:3,
                   tx_chrom="chr1",
                   tx_strand=c("-", "+", "+"),
                   tx_start=c(1, 2001, 2001),
                   tx_end=c(999, 2199, 2199))
splicings <-  data.frame(
                   tx_id=c(1L, 2L, 2L, 2L, 3L, 3L),
                   exon_rank=c(1, 1, 2, 3, 1, 2),
                   exon_start=c(1, 2001, 2101, 2131, 2001, 2131),
                   exon_end=c(999, 2085, 2144, 2199, 2085, 2199),
                   cds_start=c(1, 2022, 2101, 2131, NA, NA),
                   cds_end=c(999, 2085, 2144, 2193, NA, NA),
                   cds_phase=c(0, 0, 2, 0, NA, NA))

txdb <- makeTxDb(transcripts, splicings)
}
