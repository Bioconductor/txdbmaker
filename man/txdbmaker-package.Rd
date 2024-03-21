\name{txdbmaker-package}
\docType{package}

\alias{txdbmaker-package}
\alias{txdbmaker}

\title{Tools for making TxDb objects from genomic annotations}

\description{
  The \pkg{txdbmaker} package contains a set of tools for making TxDb objects
  from genomic annotations from various sources (e.g. UCSC, Ensembl, and
  GFF files). These tools allow the user to download the genomic locations
  of transcripts, exons, and CDS, for a given assembly, and to import them
  in a TxDb object.

  Note that \link[GenomicFeatures]{TxDb} objects are implemented in the
  \pkg{GenomicFeatures} package, together with flexible methods for
  extracting the desired features in convenient formats.
}

\details{
  For a quick overview of the provided tools, please see
  the "Making TxDb Objects" vignette included in this package.

  To access the vignette from your R session, run
  \code{browseVignettes(package="txdbmaker")}. This requires the
  \pkg{txdbmaker} package to be already installed.

  Alternatively this vignette should also be available online here:
  \link{https://bioconductor.org/packages/release/bioc/vignettes/txdbmaker/inst/doc/txdbmaker.html}
}

\keyword{package}
