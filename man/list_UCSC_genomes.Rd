\name{list_UCSC_genomes}

\alias{list_UCSC_genomes}

\title{List UCSC genomes}

\description{
  Get the list of UCSC genomes.
}

\usage{
list_UCSC_genomes(organism=NA, recache=FALSE)
}

\arguments{
  \item{organism}{
    By default all UCSC genomes are returned (in a data frame).
    When \code{organism} is specified, \code{list_UCSC_genomes()}
    will only return the rows associated with the specified organism.
    \code{organism} must be supplied as a single string that will be
    used to perform a search (with \code{grep()}) on the \code{organism}
    and \code{common_name} columns of the data frame to return.
    The search is case-insensitive.
  }
  \item{recache}{
    \code{list_UCSC_genomes()} uses a cache mechanism so the list of genomes
    only gets downloaded once during the current R session (note that the
    caching is done in memory so cached information does NOT persist across
    sessions).
    Setting \code{recache} to \code{TRUE} forces a new download (and
    recaching) of the list of genomes.
  }
}

\value{
  A data frame with 1 row per genome and 5 columns: \code{organism},
  \code{genome}, \code{common_name}, \code{tax_id}, \code{description}.
}

\seealso{
  \itemize{
    \item \url{https://genome.ucsc.edu/FAQ/FAQreleases.html} for the online
          HTML page that lists all UCSC genome releases, including archived
          ones.

    \item \code{\link[GenomeInfoDb]{registered_UCSC_genomes}} in the
          \pkg{GenomeInfoDb} package for a similar function that returns
          only UCSC genomes registered in the \pkg{GenomeInfoDb} package.
  }
}

\examples{
list_UCSC_genomes("human")

list_UCSC_genomes("pacos")
}

\keyword{manip}
