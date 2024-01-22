# Auxiliar commands for package documentation with roxygen2 

#--------------------
# NAMESPACE COMMANDS
#--------------------

#'@import ggplot2
#'@import methods
#'@import terra
#'@import reshape2
#'@importFrom SoDA geoXY
#'@importFrom grDevices nclass.FD
#'@importFrom graphics abline
#'@importFrom graphics hist
#'@importFrom graphics points
#'@importFrom graphics text
#'@importFrom grid viewport grid.layout grid.newpage pushViewport viewport
#'@importFrom grDevices rainbow
#'@importFrom graphics lines par
#'@importFrom party ctree
#'@importFrom party where
#'@importFrom raster addLayer
#'@importFrom raster brick
#'@importFrom raster crs
#'@importFrom raster calc
#'@importFrom raster crop
#'@importFrom raster extent
#'@importFrom raster intersect
#'@importFrom raster ncell
#'@importFrom raster raster
#'@importFrom raster writeRaster
#'@importFrom rkt rkt
#'@importFrom sp coordinates
#'@importFrom stats as.dist
#'@importFrom stats coef
#'@importFrom stats cor
#'@importFrom stats dist
#'@importFrom stats lm
#'@importFrom stats median
#'@importFrom stats p.adjust
#'@importFrom stats qt
#'@importFrom stats quantile
#'@importFrom stats step
#'@importFrom stats var
#'@importFrom utils browseURL
#'@importFrom utils capture.output
#'@importFrom utils combn
#'@importFrom utils data
#'@importFrom utils head
#'@importFrom utils write.table
#'@importFrom plotly ggplotly
#'@importFrom networkD3 forceNetwork
#'@importFrom edgebundleR edgebundle
#'@importFrom igraph plot.igraph
#'@importFrom igraph graph_from_adjacency_matrix
#'@importFrom igraph V
#'@importFrom igraph graph.data.frame
#'@importFrom igraph E
#'@importFrom grDevices heat.colors
#'@importFrom graphics barplot legend
#'@importFrom stats sd
#'@importFrom foreach foreach "%dopar%" getDoParWorkers getDoParName
#'@importFrom parallel makeCluster stopCluster detectCores
#'@importFrom doParallel registerDoParallel stopImplicitCluster
0

#--------------------
# DATA FILES 
#--------------------

#' tab
#' @name tab
#' @docType data
#' @description Data frame with information of bands 3 and 4 for two 
#' dates, corresponding to real Landsat 5 images and used in this 
#' package as pedagogic material complementing simulated data.
#' Date and sun elevation data were extracted from the header
#' provided with the image in \url{http://glovis.usgs.gov/}. 
#' The starting haze values (SHV) were estimated checking the 
#' profiles of the bands.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(tab)
#' tab
#' @keywords data

0


#' eco
#' @name eco
#' @docType data
#' @description ecogen object with simulated data of 225 individuals, with codominant markers
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(eco.test)
#' eco
#' @keywords data

0

#' eco_dom
#' @name eco_dom
#' @docType data
#' @description ecogen object with simulated data of 225 individuals, with dominant markers
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(eco.test)
#' eco
#' @keywords data

0


#' eco2
#' @name eco2
#' @docType data
#' @description ecogen object with simulated data of 900 individuals.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(eco2)
#' eco2
#' @keywords data

0

#' eco3
#' @name eco3
#' @docType data
#' @description ecogen object with simulated data of 173 individuals.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(eco3)
#' eco3
#' @keywords data

0

#' eco4
#' @name eco4
#' @docType data
#' @description data frames with simulated data of 173 individuals. 
#' The data frams can be used to construct an ecogen object that includes
#' genetic data separated by a character ad with non uniform of number of characters
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(eco3)
#' eco3
#' @keywords data
#' 

0


#' XY
#' @name XY
#' @docType data
#' @description Factor with simulated coordinates of 173 individuals.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(eco4)
#' XY
#' @keywords data

0


#' P
#' @name P
#' @docType data
#' @description Factor with simulated phenotypic data of 173 individuals.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(eco4)
#' P
#' @keywords data

0

#' G
#' @name G
#' @docType data
#' @description data frame with simulated genetic data of 173 individuals.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(eco4)
#' G
#' @keywords data

0

#' E
#' @name E
#' @docType data
#' @description data frame with simulated environmental data of 173 individuals.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(eco4)
#' E
#' @keywords data

0


#' S
#' @name S
#' @docType data
#' @description Factor with simulated groups of 173 individuals.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(eco4)
#' S
#' @keywords data

0

#' coordinates
#' @name coordinates
#' @docType data
#' @description Data frame with cartesian coordinates of 
#' 225 simulated individuals.
#' @keywords data
#' @usage
#' data(eco.test)
#' coordinates
#' @author Leandro Roser \email{learoser@@gmail.com}
#' 

0


#' phenotype
#' @name phenotype
#' @docType data
#' @description Data frame with simulated morphometric 
#' data of 225 individuals.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(eco.test)
#' phenotype
#' @keywords data

0


#' genotype
#' @name genotype
#' @docType data
#' @description Data frame with simulated microsatellite 
#' data of 225 individuals.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(eco.test)
#' genotype
#' @keywords data

0

#' environment
#' @name environment
#' @docType data
#' @description Data frame with simulated environmental variables 
#' of 225 individuals.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(eco.test)
#' environment
#' @keywords data

0

#' structure
#' @name structure
#' @docType data
#' @description Factor with simulated groups of 225 individuals.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(eco.test)
#' structure
#' @keywords data

0


#' genotype_dom
#' @name genotype_dom
#' @docType data
#' @description Data frame with simulated dominant data
#' data of 225 individuals.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(eco.test)
#' genotype_dom
#' @keywords data

0


#' my_ecopop
#' @name my_ecopop
#' @docType data
#' @description ecopop object generated with the object eco
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(eco.test)
#' my_ecopop
#' @keywords data

0

#' table.sokal
#' @name table.sokal
#' @docType data
#' @description Allelic frequency table from 50 villages, analyzed in Sokal et al. (1986). 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @usage
#' data(sokal1986)
#' table.sokal
#' @keywords data

0

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

 
# #' dopar operator
# #'
# #' @name %dopar%
# #' @rdname dopar
# #' @keywords internal
# #' @export
# #' @importFrom foreach %dopar%
# #' @usage obj %dopar% ex
# NULL
