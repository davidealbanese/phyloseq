\docType{methods}
\name{[}
\alias{[}
\alias{[,otu_table,ANY,ANY,ANY-method}
\alias{[,sample_data,ANY,ANY,ANY-method}
\alias{[,taxonomyTable,ANY,ANY,ANY-method}
\alias{[,XStringSet,character,ANY,ANY-method}
\title{Method extensions to extraction operator for phyloseq objects.}
\description{
  See the documentation for the \code{\link[base]{Extract}}
  generic, defined in the R
  \code{\link[base]{base-package}} for the expected
  behavior.
}
\details{
  One special exception to standard behavior of these
  methods in phyloseq is that the \code{drop} argument is
  set internally to \code{FALSE}. This helps avoid bugs
  during complicated subsetting with multiple components,
  where it is necessary to be able to use a two dimensional
  indexing even if one of those dimensions has only 1 rank.
  Put another way, these phyloseq-defined extractions never
  collapse their result into a vector. See the
  documentation of \code{\link[base]{Extract}} for more
  information about the \code{drop} argument.
}
\examples{
data(esophagus)
nrow(otu_table(esophagus))
nrow(otu_table(esophagus)[1:5, ])
}
\seealso{
  \code{\link[base]{Extract}}
}

