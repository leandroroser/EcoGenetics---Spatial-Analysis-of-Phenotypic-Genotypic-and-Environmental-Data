% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eco.correlog.R
\name{eco.correlog}
\alias{eco.correlog}
\title{Moran's I, Geary's C and bivariate Moran's I correlograms, omnidirectional and directional}
\usage{
eco.correlog(
  Z,
  XY,
  Y = NULL,
  int = NULL,
  smin = 0,
  smax = NULL,
  nclass = NULL,
  size = NULL,
  seqvec = NULL,
  method = c("I", "C", "CC"),
  nsim = 99,
  test = c("permutation", "bootstrap"),
  alpha = 0.05,
  alternative = c("auto", "two.sided", "greater", "less"),
  adjust = "holm",
  sequential = ifelse((as.deg), FALSE, TRUE),
  include.zero = TRUE,
  cummulative = FALSE,
  bin = c("sturges", "FD"),
  row.sd = FALSE,
  latlon = FALSE,
  angle = NULL,
  as.deg = TRUE
)
}
\arguments{
\item{Z}{Vector, matrix or data frame with variable/s
(in matrix or data frame formats, variables in columns).}

\item{XY}{Data frame or matrix with individual's positions (projected coordinates).}

\item{Y}{Vector with the second variable for Mantel's Ixy cross-correlograms.
If Z has multiple variables, the program will compute the cross-correlograms 
for each with Y.}

\item{int}{Distance interval in the units of XY.}

\item{smin}{Minimum class distance in the units of XY.}

\item{smax}{Maximum class distance in the units of XY.}

\item{nclass}{Number of classes.}

\item{size}{Number of individuals per class.}

\item{seqvec}{Vector with breaks in the units of XY.}

\item{method}{Correlogram method. It can be I for Moran's I, C for Geary's C
and CC for Bivariate Moran's Ixy. 
If method = "CC", the program computes for the first interval (d = 0)
the corresponding P-value and CI with \code{\link[stats]{cor.test}}.}

\item{nsim}{Number of Monte-Carlo simulations.}

\item{test}{If test = "bootstrap", the program generates a bootstrap 
resampling and the associated confidence intervals of the null hypothesis.
 If test = "permutation" (default) a permutation test is made and the P-values 
 are computed.}

\item{alpha}{Value for alpha (significance level). Default alpha = 0.05.}

\item{alternative}{The alternative hypothesis. If "auto" is selected (default) the
program determines the alternative hypothesis.
Other options are: "two.sided", "greater" and "less".}

\item{adjust}{P-values correction method for multiple tests. 
The selected method is passed as argument to \code{\link[stats]{p.adjust}} (defalut = "holm").
For bearing correlograms, the corrections (and permutation tests) are performed for individual correlograms 
of fixed variables (i.e.,  angles fixed [distances variable] or distances fixed [angles variable]).}

\item{sequential}{Should a Holm-Bonberroni correction of P-values (Legendre and Legendre, 2012) be performed?
Defalult TRUE (only available for omnidirectional correlograms or correlograms for fixed angles).}

\item{include.zero}{Should be included the distance = 0 in cross correlograms 
(i.e., the intra- individual correlation)?. Defalut TRUE.}

\item{cummulative}{Should be construced a cummulative correlogram?.}

\item{bin}{Rule for constructing intervals when a partition parameter (int, 
nclass or size) is not given. Default is Sturge's rule (Sturges, 1926). Other
option is Freedman-Diaconis method (Freedman and Diaconis, 1981).}

\item{row.sd}{Logical. Should be row standardized the matrix? Default FALSE 
(binary weights).}

\item{latlon}{Are the coordinates in decimal degrees format? Defalut FALSE. If TRUE,
the coordinates must be in a matrix/data frame with the longitude in the first
column and latitude in the second. The position is projected onto a plane in
meters with the function \code{\link{geoXY}}.}

\item{angle}{for computation of bearing correlogram (angle between 0 and 180).
Default NULL (omnidirectional).}

\item{as.deg}{in case of bearing correlograms for multiple angles, 
generate an output for each lag in function of the angle? Default TRUE.}
}
\value{
The program returns an object of class "eco.correlog" 
with the following slots:

> OUT analysis output

> IN analysis input data

> BEAKS breaks

> CARDINAL number of elements in each class

> NAMES variables names

> METHOD analysis method

> DISTMETHOD method used in the construction of breaks

> TEST test method used (bootstrap, permutation)

> NSIM number of simulations

> PADJUST P-values adjust method for permutation tests


\strong{ACCESS TO THE SLOTS}
The content of the slots can be accessed 
with the corresponding accessors, using
the generic notation of EcoGenetics 
(<ecoslot.> + <name of the slot> + <name of the object>).
See help("EcoGenetics accessors") and the Examples
section below.
}
\description{
This program computes Moran's, Geary's and bivariate Moran's correlograms, 
for single or multiple variables, with P-values or bootstrap confidence intervals.
Correlograms can be omnidirectional or directional, the latter based in the bearing method 
(Rosenberg, 2000).
The program allows high flexibility for the construction of intervals. For detailed
information about the range partition methods see \code{\link{eco.lagweight}}
}
\examples{

\dontrun{

data(eco.test)
require(ggplot2)


##########################
# Moran's I correlogram
##########################

## single test with phenotypic traits
moran <- eco.correlog(Z=eco[["P"]][,1], XY = eco[["XY"]], 
method = "I", smax=10, size=1000)

# interactive plot via plotly
eco.plotCorrelog(moran)

# standard plot via ggplot2
eco.plotCorrelog(moran, interactivePlot = FALSE)


#-------------------------------------------------------
## A directional approach based in bearing correlograms
#-------------------------------------------------------

moran_b <- eco.correlog(Z=eco[["P"]][,1], XY = eco[["XY"]], 
method = "I", smax = 10, size = 1000, angle  = seq(0, 175, 5))

 # use eco.plotCorrelogB for this object
eco.plotCorrelogB(moran_b)

 # plot for the first distance class, 
 use a number between 1 and the number of classes to select the corresponding class
eco.plotCorrelogB(moran_b, var = 1) 

#-----------------------------
## Multivariable correlograms
#-----------------------------

## multiple tests with phenotypic traits
moran2 <- eco.correlog(Z=eco[["P"]], XY = eco[["XY"]],
method = "I", smax=10, size=1000)

eco.plotCorrelog(moran2, var ="P2") ## single plots
eco.plotCorrelog(moran2, var ="P3") ## single plots


 ## Multivariable interactive plot with mean correlogram 
 ## and jackknifed confidence intervals.
 
 graf <- eco.plotCorrelog(moran2, meanplot = TRUE)
 
 # Only mean
 graf$mean.correlog
 
 # Mean and variables
 graf$multi.correlog
 
 # Information
 - correlogram data for individual variables
 - manhattan distance matrix
 - mean correlogram data
 - method used for analysis
 - names and numbers (column in data frame) of significant variables 
 
 
 
 graf$data
 
 
 # plot only alleles
 graf <- eco.plotCorrelog(moran2, meanplot = FALSE)
 graf
 
 # Both plots can also be constructed using ggplot2
 
 gg_graf <- eco.plotCorrelog(moran2, meanplot = TRUE, interactivePlot = FALSE)
 gg_graf[[1]]
 gg_graf[[2]]
 
 gg_graf <- eco.plotCorrelog(moran2, meanplot = FALSE, interactivePlot = FALSE)
 gg_graf


# standard ggplot2 correlograms support the use of ggplot2 syntax
require(ggplot2)
moranplot <- eco.plotCorrelog(moran2, var ="P3", interactivePlot = FALSE) 
moranplot <- moranplot + theme_bw() + theme(legend.position="none")
moranplot

moranplot2 <- gg_graf[[2]] + theme_bw() + theme(legend.position="none")
moranplot2


#-----------------------
Analyzing genetic data
#-----------------------

# single test with genotypic traits

# eco[["A"]] is a matrix with the genetic data of "eco" 
# as frequencies for each allele in each individual. Each allele
# can be analyzed as single traits. 

head(eco[["A"]])      # head of the matrix

# analyzing allele 1
moran <- eco.correlog(Z=[["A"]][,1], XY = eco[["XY"]], method = "I",
smax=10, size=1000)                
eco.plotCorrelog(moran)

# multiple tests with genotypic traits. 
# nsim is set to 10 only for speed in the example
moran2 <- eco.correlog(Z = eco[["A"]], XY = eco[["XY"]], 
method = "I",smax=10, size=1000, nsim=99)


## multiple plot with mean 
## correlogram and jackknifed 
## confidence intervals.

graf <- eco.plotCorrelog(moran2, meanplot = TRUE)

## the same example, but with nsim = 99. 
moran3 <- eco.correlog(Z = eco[["A"]], XY = eco[["XY"]], method = "I", 
smax=10, size=1000, nsim=99)  
       
## plot for alleles with at least one significant value after
## Bonferroni-Holm sequential P correction
## (set adjust "none" for no family-wise 
## P correction in "eco.correlog")

eco.plotCorrelog(moran3, meanplot = TRUE, significant.M = TRUE)

#-----------------------
# ACCESSORS USE EXAMPLE
#-----------------------

# the slots are accesed with the generic format 
# (ecoslot. + name of the slot + name of the object). 
# See help("EcoGenetics accessors")

ecoslot.OUT(moran)      # slot OUT
ecoslot.BREAKS(moran)   # slot BREAKS
                                             
#---------------------------------------------------------------------------#

##########################
# Geary's C correlogram
##########################

geary <- eco.correlog(Z = eco[["P"]][,1], XY = eco[["XY"]], method = "C",
smax=10, size=1000)
# Interactive plot
eco.plotCorrelog(geary)
# ggplot2 plot
eco.plotCorrelog(geary, interactivePlot = FALSE)

#---------------------------------------------------------------------------#

##########################
# Bivariate Moran's Ixy
##########################   

cross <- eco.correlog(Z=eco[["P"]][,1], XY = eco[["XY"]], Y = eco[["P"]][, 1],
method = "CC", int= 2, smax=15)
# Interactive plot
eco.plotCorrelog(cross)
# ggplot2 plot
eco.plotCorrelog(cross, interactivePlot = FALSE)

}

}
\references{
Freedman D., and P. Diaconis. 1981. On the histogram as a density estimator: 
L 2 theory. Probability theory and related fields, 57: 453-476.

Geary R. 1954. The contiguity ratio and statistical mapping. 
The incorporated statistician, 115-146.

Legendre P., and L. Legendre. 2012. Numerical ecology. Third English edition.
Elsevier Science, Amsterdam, Netherlands

Moran P. 1950. Notes on continuous stochastic phenomena. Biometrika, 17-23. 

Reich R., R. Czaplewski and W. Bechtold. 1994. 
Spatial cross-correlation of undisturbed, natural shortleaf pine stands 
in northern Georgia. Environmental and Ecological Statistics, 1: 201-217.

Rosenberg, M. 2000. The bearing correlogram: a new method 
of analyzing directional spatial autocorrelation. 
Geographical Analysis, 32: 267-278.

Sokal R. and N. Oden 1978. Spatial autocorrelation in biology: 
1. Methodology. Biological journal of the Linnean Society, 10: 199-228.

Sokal R. and N. Oden. 1978. Spatial autocorrelation in biology. 
2. Some biological implications and four applications of evolutionary and 
ecological interest. Biological Journal of the Linnean Society, 10: 229-49.

Sokal R. 1979. Ecological parameters inferred from spatial correlograms. 
In: G. Patil and M. Rosenzweig, editors. Contemporary Quantitative Ecology and 
elated Ecometrics. International Co-operative Publishing House: Fairland,
MD, pp. 167-96.
 
Sturges  H. 1926. The choice of a class interval. Journal of the American 
Statistical Association, 21: 65-66.
}
\author{
Leandro Roser \email{learoser@gmail.com}
}
