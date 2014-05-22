################################################################################
#' The S4 class for storing taxa-abundance information.
#'
#' Because orientation of these tables can vary by method, the orientation is
#' defined explicitly in the \code{taxa_are_rows} slot (a logical).
#' The \code{otu_table} class inherits the \code{\link{matrix}} class to store
#' abundance values.
#' Various standard subset and assignment nomenclature has been extended to apply
#' to the \code{otu_table} class, including square-bracket, \code{\link{t}}, etc.
#'
#' \describe{
#'    \item{taxa_are_rows}{
#'		A single logical specifying the orientation of the abundance table.
#'    }
#'
#'    \item{.Data}{This slot is inherited from the \code{\link{matrix}} class.}
#'  }
#' @name otu_table-class
#' @rdname otu_table-class
#' @exportClass otu_table
setClass("otu_table", prototype = prototype(taxa_are_rows=TRUE),
         contains = "matrix", slots = list(taxa_are_rows="logical"))
################################################################################
#' The S4 for storing sample variables.
#'
#' Row indices represent samples, while column indices represent experimental
#' categories, variables (and so forth) that describe the samples.
#'
#' \describe{
#'
#'    \item{.Data}{data-frame data, inherited from the data.frame class.}
#' 
#'    \item{row.names}{
#'	     Also inherited from the data.frame class;
#'       it should contain the sample names.
#'    }
#' 
#'    \item{names}{Inherited from the data.frame class.}
#' 
#'  }
#' 
#' @name sample_data-class
#' @rdname sample_data-class
#' @exportClass sample_data
setClass("sample_data", contains="data.frame")
################################################################################
#' An S4 class that holds taxonomic classification data as a character
#' matrix.
#'
#' Row indices represent taxa, columns represent taxonomic classifiers.
#' 
#' \describe{
#'    \item{.Data}{This slot is inherited from the \code{\link{matrix}} class.}
#' }
#'
#' @name taxonomyTable-class
#' @rdname taxonomyTable-class
#' @exportClass taxonomyTable
setClass("taxonomyTable", contains = "matrix", prototype = matrix("", 0 , 0))
#metaMDS
################################################################################
#' S3 class placeholder definition (list) for metaMDS
#' 
#' The ape package does export a version of its \code{\link[vegan]{metaMDS}}-class,
#' partly because it is not really defined formally anywhere.
#' Instead, it is an S3 class extended from the base class, \code{\link{list}} --
#' this is a very common and easy approach --
#' and proper behavior of any method taking an instance of this class 
#' requires exact naming conventions for element names of the list components.
#' The phyloseq package does not provide any validity checks that a given phylo
#' instance is valid (conforms to the conventions in the ape package)... yet.
#' If problems arise, this might be considered, and they could be defined
#' judiciously and within phyloseq.
#' 
#' @seealso 
#' \code{\link[vegan]{metaMDS}}
#' 
#' @keywords internal
metaMDS <- structure(list(), class = "metaMDS")
###
# Remove if this ever works
# @importClassesFrom vegan metaMDS
################################################################################
#' S3 class placeholder definition (list) for decorana
#' 
#' The ape package does export a version of its \code{\link[vegan]{decorana}}-class,
#' partly because it is not really defined formally anywhere.
#' Instead, it is an S3 class extended from the base class, \code{\link{list}} --
#' this is a very common and easy approach --
#' and proper behavior of any method taking an instance of this class 
#' requires exact naming conventions for element names of the list components.
#' The phyloseq package does not provide any validity checks that a given phylo
#' instance is valid (conforms to the conventions in the ape package)... yet.
#' If problems arise, this might be considered, and they could be defined
#' judiciously and within phyloseq. 
#' 
#' @seealso 
#' \code{\link[vegan]{decorana}}
#' 
#' @keywords internal
decorana <- structure(list(), class = "decorana")
###
# Remove if this ever works
# @importClassesFrom vegan decorana
################################################################################
#' S3 class placeholder definition (list) for dpcoa
#' 
#' The ade4 package does not export a version of its \code{\link[ade4]{dpcoa}}-class,
#' partly because it is not really defined formally anywhere.
#' Instead, it is an S3 class extended from the base class, \code{\link{list}} --
#' this is a very common and easy approach --
#' and proper behavior of any method taking an instance of this class 
#' requires exact naming conventions for element names of the list components.
#' The phyloseq package does not provide any validity checks that a given phylo
#' instance is valid (conforms to the conventions in the ape package). Yet.
#' If problems arise, this might be considered, and they could be defined
#' judiciously and within phyloseq. 
#' 
#' An instance of this class can be produced from within phyloseq using either
#' the \code{\link{DPCoA}} function, or the higher-level wrapping function
#' \code{\link{ordinate}}.
#' 
#' @seealso 
#' \code{\link[ade4]{dpcoa}}
#' 
#' \code{\link{DPCoA}}
#' 
#' \code{\link{ordinate}}
#' 
#' @keywords internal
dpcoa <- structure(list(), class = "dpcoa")
################################################################################
# If this ever works
# @importClassesFrom ade4 dpcoa
################################################################################
# If this ever works
# @importClassesFrom ape phylo
################################################################################
# Define an S4 equivalent to the S3-phylo class in ape
# Define a minimal generic instantiation of a "phylo" class (S3) tree
#' @keywords internal
phyloproto <- structure(list(edge=matrix(NA_integer_, nrow = 0, ncol = 2),
                             tip.label=vector("character"),
                             Nnode=0L),
                        class = "phylo")
#' @keywords internal
setOldClass("phylo", prototype = phyloproto)
################################################################################
#' An S4 equivalent to the \code{\link[ape]{phylo}} class in ape.
#'
#' See the \code{\link[ape]{ape}} package for details about this type of
#' representation of a phylogenetic tree. It is used throught ape.
#' 
#' This is an S4 placeholder of the main phylogenetic tree class from the ape package.
#' The ape package does not export a version of its \code{\link[ape]{phylo}}-class,
#' in large part because it is not defined formally anywhere.
#' Instead, it is an S3 class extended from a base class, \code{\link{list}}.
#' This is a very common and easy approach utilized in many S3 methods.
#' However, proper behavior of any method taking an instance of 
#' the \code{\link[ape]{phylo}} class
#' requires exact naming conventions for element names of the components.
#' The phyloseq package now includes some validity checks on required components,
#' and this may improve over time as this S4/S3 transition is tested.
#' If a formal definition for the the phylo-class is ever exported
#' by ape, the current philosophy of phyloseq would be to 
#' import those from ape rather than maintain duplicate
#' internal definitions. Note that there is still some 
#' work going on for the phylobase package, which is addressing these same 
#' exact issues for S4 phylogenetic tree interaction. 
#' A very large number of packages (around 60 at my last count), depend on ape,
#' making it easily the de facto standard for representing phylogenetic trees in R;
#' and the phyloseq team would prefer to use any exported definitions from
#' the ape package if possible and available.
#' 
#' @slot edge A two-column matrix of mode numeric where each row represents 
#'  an edge of the tree; the nodes and the tips are symbolized with numbers;
#'  the tips are numbered from 1 to the number of tips, 
#'  and the nodes are numbered after the tips. 
#'  For each row, the first column gives the ancestor.
#' @slot edge.length (Optional) a numeric vector giving the lengths 
#'  of the branches given by \code{edge}.
#' @slot tip.label A vector of mode character giving the names of the tips;
#'  the order of the names in this vector corresponds to the
#'  (positive) number in edge.
#' @slot Nnode The number of (internal) nodes.
#' @slot node.label (Optional). A vector of mode character 
#'  giving the names of the nodes.
#' @slot root.edge (Optional). A numeric value giving the length 
#'  of the branch at the root if it exists.
#'  
#' @seealso \code{\link[ape]{phylo}}, \code{\link{setOldClass}}
#'
#' @name phyloS4
#' @exportClass phyloS4
setClass("phyloS4",
         prototype = prototype(edge=matrix(NA_integer_, nrow = 0, ncol = 2),
                               tip.label=vector("character"),
                               Nnode=0L),
         slots = list(edge="matrix", 
                      edge.length="numeric", 
                      tip.label="character", 
                      Nnode="integer",
                      node.label="character", root.edge="numeric"))
################################################################################
setAs(from = "phyloS4", to = "phylo", def = function(from){
  # from = new("phyloS4")
  phylo = sapply(names(getSlots("phyloS4")), function(i, from){slot(from, i)},
                 from, simplify = FALSE, USE.NAMES = TRUE)
  # Remove empty optional elements
  optionalElements = c("edge.length", "node.label", "root.edge")
  removeElements = which(sapply(phylo, length) < 1)[optionalElements]
  if(length(removeElements) > 0){
    phylo <- phylo[-removeElements]
  }
  # Add the class attribute
  phylo <- structure(phylo, class = "phylo")
  return(phylo)
})
################################################################################
setAs(from = "phylo", to = "phyloS4", def = function(from){
  # from = as(new("phyloS4"), "phylo")
  phyloS4 = do.call("new", c(list(Class = "phyloS4"), 
                             sapply(names(from), function(i, from){from[[i]]},
                                    from, simplify = FALSE, USE.NAMES = TRUE)
  ))
  return(phyloS4)
})
################################################################################
# An S4 placeholder for the basic \code{\link[stats]{dist}}ance matrix class.
# 
# See the \code{\link[ape]{ape}} package for details about this type of
# representation of a phylogenetic tree. It is used throught ape.
# 
# @seealso
#  \code{\link[stats]{dist}}
#  
#  \code{\link{setOldClass}}
# 
# @name dist-class
# @rdname dist-class
#' @keywords internal
setOldClass("dist")
# #' @exportClass dist
################################################################################
# S3 class for ape-calculated MDS results
# 
# Nothing to import, because ape doesn't (yet) export this S3 class.
# We will define it here, but keep it internal.
# For the moment, its only use is for proper dispatch in our extensions
# to the scores S3 generic from vegan,
# for generic extraction of coordinates and possibly other features from
# any ordination results.
# 
# @keywords internal
pcoa <- structure(list(), class = "pcoa")
# @importMethodsFrom ape print
################################################################################
#' The main experiment-level class for phyloseq data
#'
#' Contains all currently-supported component data classes: 
#' \code{\link{otu_table-class}},
#' \code{\link{sample_data-class}},
#' \code{\link{taxonomyTable-class}} (\code{"tax_table"} slot),
#' \code{\link{phyloS4}}-class (\code{"phy_tree"} slot),
#' and the \code{\link[Biostrings]{XStringSet-class}} (\code{"refseq"} slot).
#' There are several advantages
#' to storing your phylogenetic sequencing experiment as an instance of the
#' phyloseq class, not the least of which is that it is easy to return to the
#' data later and feel confident that the different data types ``belong'' to
#' one another. Furthermore, the \code{\link{phyloseq}} constructor ensures that
#' the different data components have compatible indices (e.g. OTUs and samples),
#' and performs the necessary trimming automatically when you create your
#' ``experiment-level'' object. Downstream analyses are aware of which data
#' classes they require -- and where to find them -- often making your 
#' \code{phyloseq-class} object the only data argument required for analysis and plotting
#' functions (although there are many options and parameter arguments available
#' to you). 
#'
#' In the case of missing component data, the slots are set to \code{NULL}. As
#' soon as a \code{phyloseq-class} object is to be updated with new component
#' data (previously missing/\code{NULL} or not), the indices of all components
#' are re-checked for compatibility and trimmed if necessary. This is to ensure
#' by design that components describe the same taxa/samples, and also that these
#' trimming/validity checks do not need to be repeated in downstream analyses.
#' 
#' @slot otu_table A single object of class otu_table.
#' @slot sam_data A single object of class sample_data.
#' @slot tax_table A single object of class taxonomyTable.
#' @slot phy_tree A single phylogenetic tree, class \code{\link{phyloS4}}
#' @slot refseq A biological sequence set object of a class that
#'   inherits from the \code{\link[Biostrings]{XStringSet-class}},
#'   from the Biostrings package.
#'
#' @seealso
#'  The constructor, \code{\link{phyloseq}}, 
#'  the merger \code{\link{merge_phyloseq}}, and also the component 
#'  constructor/accessors \code{\link{otu_table}}, \code{\link{sample_data}},
#'  \code{\link{tax_table}}, \code{\link{phy_tree}}, and \code{\link{refseq}}.
#' 
#' @importClassesFrom Biostrings XStringSet
#' @importClassesFrom Biostrings BStringSet
#' @importClassesFrom Biostrings DNAStringSet
#' @importClassesFrom Biostrings RNAStringSet
#' @importClassesFrom Biostrings AAStringSet
#' @importClassesFrom Biostrings QualityScaledXStringSet
#' @importClassesFrom Biostrings XStringQuality
#' @importClassesFrom Biostrings PhredQuality
#' @importClassesFrom Biostrings SolexaQuality
#' @importClassesFrom Biostrings IlluminaQuality
#' @importClassesFrom Biostrings QualityScaledBStringSet
#' @importClassesFrom Biostrings QualityScaledDNAStringSet
#' @importClassesFrom Biostrings QualityScaledRNAStringSet
#' @importClassesFrom Biostrings QualityScaledAAStringSet
#' @name phyloseq-class
#' @exportClass phyloseq
setClass(Class="phyloseq",
         slots = list(
           otu_table="otu_table",
           tax_table="taxonomyTable",
           sam_data="sample_data",
           phy_tree="phyloS4",
           refseq = "XStringSet"),
         prototype=prototype(
           otu_table = new("otu_table"),
           tax_table=new("taxonomyTable"), sam_data=new("sample_data"), 
           phy_tree=new("phyloS4"), refseq=new("DNAStringSet"))
)
