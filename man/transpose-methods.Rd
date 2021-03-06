\docType{methods}
\name{t}
\alias{t}
\alias{t,otu_table-method}
\alias{t,phyloseq-method}
\title{Transpose \code{\link{otu_table-class}} or \code{\link{phyloseq-class}}}
\usage{
  t(x)
}
\arguments{
  \item{x}{An \code{otu_table} or
  \code{\link{phyloseq-class}}.}
}
\value{
  The class of the object returned by \code{t} matches the
  class of the argument, \code{x}. The \code{otu_table} is
  transposed, and \code{\link{taxa_are_rows}} value is
  toggled.
}
\description{
  Extends the base transpose method, \code{\link[base]{t}}.
}
\examples{
data(GlobalPatterns)
otu_table(GlobalPatterns)
t( otu_table(GlobalPatterns) )
}

