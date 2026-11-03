\name{makeTxDbFromEnsembl}

\alias{makeTxDbFromEnsembl}

\title{
  Make a TxDb object from an Ensembl database
}

\description{
  The \code{makeTxDbFromEnsembl} function creates a
  \link[GenomicFeatures]{TxDb} object for a given organism by importing
  the genomic locations of its transcripts, exons, CDS, and genes from an
  Ensembl database.

  Note that it uses the \pkg{RMariaDB} package internally so make sure
  that this package is installed.
}

\usage{
makeTxDbFromEnsembl(organism="Homo sapiens",
                    release=NA, kingdom=NA,
                    circ_seqs=NULL,
                    server="ensembldb.ensembl.org",
                    username="anonymous", password=NULL, port=0L,
                    tx_attrib=NULL,
                    taxonomyId=NA)
}

\arguments{
  \item{organism}{
    The \emph{scientific name} (i.e. genus and species, or genus and species
    and subspecies) of the organism for which to import the data.
    Case is not sensitive. Underscores can be used instead of white spaces
    e.g. \code{"homo_sapiens"} is accepted.
  }
  \item{release}{
    The Ensembl release to query e.g. 89. If set to \code{NA} (the default),
    the current release is used.
  }
  \item{kingdom}{
    For non-vertebrates (e.g. Arabidopsis thaliana), the kingdom must be
    specified (e.g. \code{"plants"}). Must be one of \code{"bacteria"},
    \code{"fungi"}, \code{"metazoa"}, \code{"plants"}, and \code{"protists"}.
    Note that, when specifying a non-vertebrate kingdom, server must be
    set to \code{"mysql-eg-publicsql.ebi.ac.uk"} and \code{port} to 4157.
    See \code{server} argument below.
  }
  \item{circ_seqs}{
    A character vector to list out which chromosomes should be marked
    as circular.
  }
  \item{server}{
    The name of the MySQL server to query.
    See \url{https://www.ensembl.org/info/data/mysql.html} for the list of
    Ensembl public MySQL servers.
    Note that an updated list of Ensembl public MySQL servers was published
    more recently (in June 2026) here:
    \url{https://www.ensembl.info/2026/06/02/ensembl-mysql-services-will-be-upgraded-to-mysql-8-soon/}.

    Make sure to use the server that is nearest to you as it can make a big
    difference in terms of speed of data transfer!
  }
  \item{username}{
    Login username for the MySQL server.
  }
  \item{password}{
    Login password for the MySQL server.
  }
  \item{port}{
    Port of the MySQL server.
  }
  \item{tx_attrib}{
    If not \code{NULL}, only select transcripts with an attribute of
    the given code, a string, like \code{"gencode_basic"}.
  }
  \item{taxonomyId}{
    By default this argument is set to NA and the taxonomy ID for the
    supplied \code{organism} will be automatically obtained and stored
    in the returned \link[GenomicFeatures]{TxDb} object.
    However, you can override this by supplying your own (valid) taxonomy ID,
    even though you should never need to do this.
  }
}

\value{
  A \link[GenomicFeatures]{TxDb} object.
}

\note{
  \code{makeTxDbFromEnsembl} tends to be faster and more reliable than
  \code{\link{makeTxDbFromBiomart}}.
}

\author{H. Pagès}

\seealso{
  \itemize{
    \item \code{\link{makeTxDbFromUCSC}} and \code{\link{makeTxDbFromBiomart}}
          for making a \link[GenomicFeatures]{TxDb} object from other online
          resources.

    \item \code{\link{makeTxDbFromGRanges}} and \code{\link{makeTxDbFromGFF}}
          for making a \link[GenomicFeatures]{TxDb} object from a
          \link[GenomicRanges]{GRanges} object, or from a GFF or GTF file.

    \item \link[GenomicFeatures]{TxDb} objects implemented in the
          \pkg{GenomicFeatures} package.

    \item \code{\link{makeTxDb}} for the low-level function used
          by the \code{makeTxDbFrom*} functions to make the
          \link[GenomicFeatures]{TxDb} object returned to the user.
  }
}

\examples{
## Note that, right after a new Ensembl release, it can take up to 24 or
## 48 hours for the MySQL server at useastdb.ensembl.org to get updated
## with the new release. During that period, the server will be out-of-sync
## with the content at https://ftp.ensembl.org/pub/current/mysql/, which
## can cause makeTxDbFromEnsembl() to fail with an error like:
##   Error: Failed to connect: Unknown database 'some_organism_core_114_4'
## To avoid running into this issue in the context of this example, we
## first try useastdb.ensembl.org, and, if it fails, we fall back to
## ensembldb.ensembl.org.
txdb1 <- try(makeTxDbFromEnsembl("Saccharomyces cerevisiae",
                                 server="useastdb.ensembl.org"))
if (inherits(txdb1, "try-error")) {
    txdb1 <- try(makeTxDbFromEnsembl("Saccharomyces cerevisiae",
                                     server="ensembldb.ensembl.org"))
}
txdb1

## Remember to supply 'kingdom', 'server', and 'port' when the organism
## is not a vertebrate (see 'kingdom' argument above):
\donttest{
## Can be very slow, depending on your location!
txdb2 <- makeTxDbFromEnsembl("Arabidopsis thaliana", kingdom="plants",
                             server="mysql-eg-publicsql.ebi.ac.uk",
                             port=4157)
txdb2
}
}
