\name{distance}
\alias{distance}
\title{General distance / dissimilarity index calculator}
\usage{
  distance(physeq, method = "unifrac", type = "samples",
  ...)
}
\arguments{
  \item{physeq}{(Required).  A \code{\link{phyloseq-class}}
  or an \code{\link{otu_table-class}} object. The latter is
  only appropriate for methods that do not require any
  additional data (one-table). For example, the ``unifrac''
  option (\code{\link{UniFrac}}) requires
  \code{\link{phyloseq-class}} that contains both an
  \code{otu_table} and a phylogenetic tree (\code{phylo}).}

  \item{method}{(Optional). A character string. Default is
  \code{"unifrac"}. Provide one of the 45 currently
  supported options. To see a list of supported options,
  enter the following into the command line:

  \code{distance("list")}

  For further details and additional arguments, see the
  documentation for the supprting functions, linked below
  under ``See Also''.

  In particular, there are three methods included by the
  \code{\link{phyloseq-package}}, and accessed by the
  following \code{method} options:

  \code{"unifrac"}, for (unweighted) UniFrac distance,
  \code{\link{UniFrac}};

  \code{"wunifrac"}, for weighted-UniFrac distance,
  \code{\link{UniFrac}};

  \code{"dpcoa"}, sample-wise distance from Double
  Principle Coordinate Analysis, \code{\link{DPCoA}};

  \code{"jsd"}, for Jensen-Shannon Divergence,
  \code{\link{JSD}};

  and it is recommended that you see their documentation
  for details, references, background and examples for use.

  Alternatively, you can provide a character string that
  defines a custom distance method, if it has the form
  described in \code{\link{designdist}}.}

  \item{type}{(Optional). A character string. The type of
  pairwise comparisons being calculated: sample-wise or
  taxa-wise. The default is \code{c("samples")}.}

  \item{...}{Additional arguments passed on to the
  appropriate distance function, determined by the
  \code{method} argument.}
}
\value{
  An object of class ``\code{\link{dist}}'' suitable for
  certain ordination methods and other distance-based
  analyses.
}
\description{
  Takes a \code{\link{phyloseq-class}} object and method
  option, and returns a \code{\link{dist}}ance object
  suitable for certain ordination methods and other
  distance-based analyses. There are currently 45
  explicitly supported method options, as well as
  user-provided arbitrary methods via an interface to
  \code{\link{designdist}}. For the complete list of
  currently supported options/arguments to the
  \code{method} parameter, type \code{distance("list")} at
  the command-line. Only sample-wise distances are
  currently supported (the \code{type} argument), but
  eventually species-wise (OTU-wise) distances will be
  supported as well.
}
\details{
  Depending on the \code{method} argument,
  \code{distance()} wraps one of \code{\link{UniFrac}},
  \code{\link{DPCoA}}, \code{\link{JSD}},
  \code{\link[vegan]{vegdist}},
  \code{\link[vegan]{betadiver}},
  \code{\link[vegan]{designdist}}, or \code{\link{dist}}.
}
\examples{
data(esophagus)
distance(esophagus) # Unweighted UniFrac
distance(esophagus, "wunifrac") # weighted UniFrac
distance(esophagus, "jaccard") # vegdist jaccard
distance(esophagus, "gower") # vegdist option "gower"
distance(esophagus, "g") # designdist method option "g"
distance(esophagus, "minkowski") # invokes a method from the base dist() function.
distance(esophagus, "(A+B-2*J)/(A+B)") # designdist custom distance
distance("help")
distance("list")
help("distance")
}
\seealso{
  \code{\link{plot_ordination}}, \code{\link{UniFrac}},
  \code{\link{DPCoA}}, \code{\link{JSD}},
  \code{\link[vegan]{vegdist}},
  \code{\link[vegan]{betadiver}},
  \code{\link[vegan]{designdist}}, \code{\link{dist}}.
}

