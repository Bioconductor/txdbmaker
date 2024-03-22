\name{makeTxDbFromUCSC}

\alias{supportedUCSCtables}
\alias{browseUCSCtrack}
\alias{makeTxDbFromUCSC}

\title{
  Make a TxDb object from annotations available at the
  UCSC Genome Browser
}
\description{
  The \code{makeTxDbFromUCSC} function allows the user to make a
  \link[GenomicFeatures]{TxDb} object from transcript annotations
  available at the UCSC Genome Browser.

  Note that it uses the \pkg{RMariaDB} package internally so make sure
  that this package is installed.
}
\usage{
makeTxDbFromUCSC(genome="hg19", tablename="knownGene",
        transcript_ids=NULL,
        circ_seqs=NULL,
        goldenPath.url=getOption("UCSC.goldenPath.url"),
        taxonomyId=NA,
        miRBaseBuild=NA)

supportedUCSCtables(genome="hg19")

browseUCSCtrack(genome="hg19", tablename="knownGene",
                url="https://genome.ucsc.edu/cgi-bin/")
}
\arguments{
  \item{genome}{
    The name of a UCSC genome assembly e.g. \code{"hg19"} or \code{"panTro6"}.
    Get the list of UCSC genomes currently available with
    \code{\link{list_UCSC_genomes}()[ , "genome"]}.
  }
  \item{tablename}{
    The name of the UCSC table containing the transcript genomic locations
    to retrieve. Use the \code{supportedUCSCtables} utility function to get
    the list of tables known to work with \code{makeTxDbFromUCSC}.
  }
  \item{transcript_ids}{
    Optionally, only retrieve transcript locations for the specified
    set of transcript ids. If this is used, then the meta information
    displayed for the resulting \link[GenomicFeatures]{TxDb} object will
    say 'Full dataset: no'. Otherwise it will say 'Full dataset: yes'.
  }
  \item{circ_seqs}{
    Like \link[GenomicRanges]{GRanges} objects,
    \link[SummarizedExperiment]{SummarizedExperiment} objects,
    and many other objects in Bioconductor, the \link[GenomicFeatures]{TxDb}
    object returned by \code{makeTxDbFromUCSC} contains a
    \link[GenomeInfoDb]{seqinfo} component that can be
    accessed with \code{\link[GenomeInfoDb]{seqinfo}()}.
    This component contains various sequence-level information
    like the sequence names, lengths, and circularity flag for the
    genome assembly of the \link[GenomicFeatures]{TxDb} object.

    As far as we know the information of which sequences are circular
    is not available in the UCSC Genome Browser. However, for the
    most commonly used UCSC genome assemblies \code{makeTxDbFromUCSC}
    will get this information from a knowledge database
    stored in the \pkg{GenomeInfoDb} package (see
    \code{?\link[GenomeInfoDb]{registered_UCSC_genomes}}).

    For less commonly used UCSC genome assemblies, \code{makeTxDbFromUCSC}
    will make a guess based on the chromosome names (e.g. chrM or 2micron
    will be assumed to be circular). Even though this works most of the
    time, it is not guaranteed to work \emph{all the time}. So in this
    case a warning is issued. If you think the guess is incorrect then
    you can supply your own list of circular sequences (as a character
    vector) via the \code{circ_seqs} argument.
  }
  \item{goldenPath.url}{
    A single string specifying the URL to the UCSC goldenPath location
    where the chromosome sizes are expected to be found.
  }
  \item{url}{
    Use to specify the location of an alternate UCSC Genome Browser.
  }
  \item{taxonomyId}{
    By default this value is NA and the organism inferred will be used
    to look up the correct value for this. But you can use this argument
    to supply your own valid taxId here.
  }
  \item{miRBaseBuild}{
    Specify the string for the appropriate build information from
    \pkg{mirbase.db} to use for microRNAs.  This can be learned by
    calling \code{supportedMiRBaseBuildValues}. By default, this value
    will be set to \code{NA}, which will inactivate the \code{microRNAs}
    accessor.
  }
}
\details{
  \code{makeTxDbFromUCSC} is a convenience function that feeds
  data from the UCSC source to the lower level \code{\link{makeTxDb}}
  function.
  See \code{?\link{makeTxDbFromEnsembl}} for a similar function that
  feeds data from an Ensembl database.
}

\value{
  For \code{makeTxDbFromUCSC}: A \link[GenomicFeatures]{TxDb} object.

  For \code{supportedUCSCtables}: A data frame with 3 columns
  (\code{tablename}, \code{track}, and \code{subtrack}) and 1 row
  per table known to work with \code{makeTxDbFromUCSC}.
  IMPORTANT NOTE: In the returned data frame, the set of tables associated
  with a track with subtracks might contain tables that don't exist for the
  specified genome.
}

\author{
  M. Carlson and H. Pagès
}

\seealso{
  \itemize{
    \item \code{\link{makeTxDbFromEnsembl}} and
          \code{\link{makeTxDbFromBiomart}} for making a
          \link[GenomicFeatures]{TxDb} object from other online resources.

    \item \code{\link{makeTxDbFromGRanges}} and \code{\link{makeTxDbFromGFF}}
          for making a \link[GenomicFeatures]{TxDb} object from a
          \link[GenomicRanges]{GRanges} object, or from a GFF or GTF file.

    \item \code{\link{list_UCSC_genomes}} to get the list of UCSC genomes.

    \item The \code{\link{supportedMiRBaseBuildValues}} function for
          listing all the possible values for the \code{miRBaseBuild}
          argument.

    \item \link[GenomicFeatures]{TxDb} objects implemented in the
          \pkg{GenomicFeatures} package.

    \item \code{\link{makeTxDb}} for the low-level function used
          by the \code{makeTxDbFrom*} functions to make the
          \link[GenomicFeatures]{TxDb} object returned to the user.
  }
}

\examples{
## Use list_UCSC_genomes() to obtain the list of all UCSC genomes:
list_UCSC_genomes()[ , "genome"]

## To search genomes for a particular organism:
list_UCSC_genomes("human")

## Display the list of tables known to work with makeTxDbFromUCSC():
supportedUCSCtables("hg38")
supportedUCSCtables("hg19")

## Open the UCSC track page for a given organism/table:
browseUCSCtrack("hg38", tablename="knownGene")
browseUCSCtrack("hg19", tablename="knownGene")

browseUCSCtrack("hg38", tablename="ncbiRefSeqSelect")
browseUCSCtrack("hg19", tablename="ncbiRefSeqSelect")

browseUCSCtrack("hg19", tablename="pseudoYale60")

browseUCSCtrack("sacCer3", tablename="ensGene")

## Retrieve a full transcript dataset for Yeast from UCSC:
txdb1 <- makeTxDbFromUCSC("sacCer3", tablename="ensGene")
txdb1

## Retrieve an incomplete transcript dataset for Mouse from UCSC (only
## transcripts linked to Entrez Gene ID 22290):
transcript_ids <- c(
    "uc009uzf.1",
    "uc009uzg.1",
    "uc009uzh.1",
    "uc009uzi.1",
    "uc009uzj.1"
)

txdb2 <- makeTxDbFromUCSC("mm10", tablename="knownGene",
                          transcript_ids=transcript_ids)
txdb2
}
