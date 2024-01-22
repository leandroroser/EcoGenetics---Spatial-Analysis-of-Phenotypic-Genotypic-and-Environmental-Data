#' Moran's I, Geary's C and bivariate Moran's I correlograms, omnidirectional and directional 
#' 
#' @description This program computes Moran's, Geary's and bivariate Moran's correlograms, 
#' for single or multiple variables, with P-values or bootstrap confidence intervals.
#' Correlograms can be omnidirectional or directional, the latter based in the bearing method 
#' (Rosenberg, 2000).
#' The program allows high flexibility for the construction of intervals. For detailed
#' information about the range partition methods see \code{\link{eco.lagweight}}
#' 
#' @param Z Vector, matrix or data frame with variable/s
#'  (in matrix or data frame formats, variables in columns).
#' @param XY Data frame or matrix with individual's positions (projected coordinates).
#' @param Y Vector with the second variable for Mantel's Ixy cross-correlograms.
#' If Z has multiple variables, the program will compute the cross-correlograms 
#' for each with Y.
#' @param int Distance interval in the units of XY.
#' @param smin Minimum class distance in the units of XY.
#' @param smax Maximum class distance in the units of XY.
#' @param nclass Number of classes.
#' @param seqvec Vector with breaks in the units of XY.
#' @param size Number of individuals per class.
#' @param bin Rule for constructing intervals when a partition parameter (int, 
#' nclass or size) is not given. Default is Sturge's rule (Sturges, 1926). Other
#' option is Freedman-Diaconis method (Freedman and Diaconis, 1981).
#' @param method Correlogram method. It can be I for Moran's I, C for Geary's C
#' and CC for Bivariate Moran's Ixy. 
#' If method = "CC", the program computes for the first interval (d = 0)
#' the corresponding P-value and CI with \code{\link[stats]{cor.test}}.
#' @param nsim Number of Monte-Carlo simulations. 
#' @param test If test = "bootstrap", the program generates a bootstrap 
#' resampling and the associated confidence intervals of the null hypothesis.
#'  If test = "permutation" (default) a permutation test is made and the P-values 
#'  are computed. 	
#' @param alpha Value for alpha (significance level). Default alpha = 0.05.
#' @param alternative The alternative hypothesis. If "auto" is selected (default) the
#' program determines the alternative hypothesis.
#' Other options are: "two.sided", "greater" and "less".	
#' @param adjust P-values correction method for multiple tests. 
#' The selected method is passed as argument to \code{\link[stats]{p.adjust}} (defalut = "holm").
#' For bearing correlograms, the corrections (and permutation tests) are performed for individual correlograms 
#' of fixed variables (i.e.,  angles fixed [distances variable] or distances fixed [angles variable]). 
#' @param sequential Should a Holm-Bonberroni correction of P-values (Legendre and Legendre, 2012) be performed?
#' Defalult TRUE (only available for omnidirectional correlograms or correlograms for fixed angles). 
#' @param include.zero Should be included the distance = 0 in cross correlograms 
#' (i.e., the intra- individual correlation)?. Defalut TRUE.
#' @param cummulative Should be construced a cummulative correlogram?.
#' @param row.sd Logical. Should be row standardized the matrix? Default FALSE 
#' (binary weights).
#' @param latlon Are the coordinates in decimal degrees format? Defalut FALSE. If TRUE,
#' the coordinates must be in a matrix/data frame with the longitude in the first
#' column and latitude in the second. The position is projected onto a plane in
#' meters with the function \code{\link[sf]{st_distance}}.
#' @param angle for computation of bearing correlogram (angle between 0 and 180).
#' Default NULL (omnidirectional).
#' @param as.deg in case of bearing correlograms for multiple angles, 
#' generate an output for each lag in function of the angle? Default TRUE.
#' 
#' @return The program returns an object of class "eco.correlog" 
#' with the following slots:
#' @return > OUT analysis output
#' @return > IN analysis input data
#' @return > BEAKS breaks
#' @return > CARDINAL number of elements in each class
#' @return > NAMES variables names
#' @return > METHOD analysis method 
#' @return > DISTMETHOD method used in the construction of breaks
#' @return > TEST test method used (bootstrap, permutation)
#' @return > NSIM number of simulations
#' @return > PADJUST P-values adjust method for permutation tests
#' 
#' 
#' \strong{ACCESS TO THE SLOTS}
#' The content of the slots can be accessed 
#' with the corresponding accessors, using
#' the generic notation of EcoGenetics 
#' (<ecoslot.> + <name of the slot> + <name of the object>).
#' See help("EcoGenetics accessors") and the Examples
#' section below.
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' data(eco.test)
#' require(ggplot2)
#' 
#' 
#' ##########################
#' # Moran's I correlogram
#' ##########################
#' 
#' ## single test with phenotypic traits
#' moran <- eco.correlog(Z=eco[["P"]][,1], XY = eco[["XY"]], 
#' method = "I", smax=10, size=1000)
#' 
#' # interactive plot via plotly
#' eco.plotCorrelog(moran)
#' 
#' # standard plot via ggplot2
#' eco.plotCorrelog(moran, interactivePlot = FALSE)
#' 
#' 
#' #-------------------------------------------------------
#' ## A directional approach based in bearing correlograms
#' #-------------------------------------------------------
#' 
#' moran_b <- eco.correlog(Z=eco[["P"]][,1], XY = eco[["XY"]], 
#' method = "I", smax = 10, size = 1000, angle  = seq(0, 175, 5))
#' 
#'  # use eco.plotCorrelogB for this object
#' eco.plotCorrelogB(moran_b)
#' 
#'  # plot for the first distance class, 
#'  use a number between 1 and the number of classes to select the corresponding class
#' eco.plotCorrelogB(moran_b, var = 1) 
#' 
#' #-----------------------------
#' ## Multivariable correlograms
#' #-----------------------------
#' 
#' ## multiple tests with phenotypic traits
#' moran2 <- eco.correlog(Z=eco[["P"]], XY = eco[["XY"]],
#' method = "I", smax=10, size=1000)
#' 
#' eco.plotCorrelog(moran2, var ="P2") ## single plots
#' eco.plotCorrelog(moran2, var ="P3") ## single plots
#' 
#'
#'  ## Multivariable interactive plot with mean correlogram 
#'  ## and jackknifed confidence intervals.
#'  
#'  graf <- eco.plotCorrelog(moran2, meanplot = TRUE)
#'  
#'  # Only mean
#'  graf$mean.correlog
#'  
#'  # Mean and variables
#'  graf$multi.correlog
#'  
#'  # Information
#'  - correlogram data for individual variables
#'  - manhattan distance matrix
#'  - mean correlogram data
#'  - method used for analysis
#'  - names and numbers (column in data frame) of significant variables 
#'  
#'  
#'  
#'  graf$data
#'  
#'  
#'  # plot only alleles
#'  graf <- eco.plotCorrelog(moran2, meanplot = FALSE)
#'  graf
#'  
#'  # Both plots can also be constructed using ggplot2
#'  
#'  gg_graf <- eco.plotCorrelog(moran2, meanplot = TRUE, interactivePlot = FALSE)
#'  gg_graf[[1]]
#'  gg_graf[[2]]
#'  
#'  gg_graf <- eco.plotCorrelog(moran2, meanplot = FALSE, interactivePlot = FALSE)
#'  gg_graf
#'
#' 
#' # standard ggplot2 correlograms support the use of ggplot2 syntax
#' require(ggplot2)
#' moranplot <- eco.plotCorrelog(moran2, var ="P3", interactivePlot = FALSE) 
#' moranplot <- moranplot + theme_bw() + theme(legend.position="none")
#' moranplot
#' 
#' moranplot2 <- gg_graf[[2]] + theme_bw() + theme(legend.position="none")
#' moranplot2
#' 
#' 
#' #-----------------------
#' Analyzing genetic data
#' #-----------------------
#' 
#' # single test with genotypic traits
#' 
#' # eco[["A"]] is a matrix with the genetic data of "eco" 
#' # as frequencies for each allele in each individual. Each allele
#' # can be analyzed as single traits. 
#' 
#' head(eco[["A"]])      # head of the matrix
#' 
#' # analyzing allele 1
#' moran <- eco.correlog(Z=[["A"]][,1], XY = eco[["XY"]], method = "I",
#' smax=10, size=1000)                
#' eco.plotCorrelog(moran)
#' 
#' # multiple tests with genotypic traits. 
#' # nsim is set to 10 only for speed in the example
#' moran2 <- eco.correlog(Z = eco[["A"]], XY = eco[["XY"]], 
#' method = "I",smax=10, size=1000, nsim=99)
#' 
#' 
#' ## multiple plot with mean 
#' ## correlogram and jackknifed 
#' ## confidence intervals.
#' 
#' graf <- eco.plotCorrelog(moran2, meanplot = TRUE)
#' 
#' ## the same example, but with nsim = 99. 
#' moran3 <- eco.correlog(Z = eco[["A"]], XY = eco[["XY"]], method = "I", 
#' smax=10, size=1000, nsim=99)  
#'        
#' ## plot for alleles with at least one significant value after
#' ## Bonferroni-Holm sequential P correction
#' ## (set adjust "none" for no family-wise 
#' ## P correction in "eco.correlog")
#' 
#' eco.plotCorrelog(moran3, meanplot = TRUE, significant.M = TRUE)
#' 
#' #-----------------------
#' # ACCESSORS USE EXAMPLE
#' #-----------------------
#' 
#' # the slots are accesed with the generic format 
#' # (ecoslot. + name of the slot + name of the object). 
#' # See help("EcoGenetics accessors")
#' 
#' ecoslot.OUT(moran)      # slot OUT
#' ecoslot.BREAKS(moran)   # slot BREAKS
#'                                              
#' #---------------------------------------------------------------------------#
#' 
#' ##########################
#' # Geary's C correlogram
#' ##########################
#' 
#' geary <- eco.correlog(Z = eco[["P"]][,1], XY = eco[["XY"]], method = "C",
#' smax=10, size=1000)
#' # Interactive plot
#' eco.plotCorrelog(geary)
#' # ggplot2 plot
#' eco.plotCorrelog(geary, interactivePlot = FALSE)
#' 
#' #---------------------------------------------------------------------------#
#' 
#' ##########################
#' # Bivariate Moran's Ixy
#' ##########################   
#'
#' cross <- eco.correlog(Z=eco[["P"]][,1], XY = eco[["XY"]], Y = eco[["P"]][, 1],
#' method = "CC", int= 2, smax=15)
#' # Interactive plot
#' eco.plotCorrelog(cross)
#' # ggplot2 plot
#' eco.plotCorrelog(cross, interactivePlot = FALSE)
#' 
#'}
#'
#' @references 
#' 
#' Freedman D., and P. Diaconis. 1981. On the histogram as a density estimator: 
#' L 2 theory. Probability theory and related fields, 57: 453-476.
#' 
#' Geary R. 1954. The contiguity ratio and statistical mapping. 
#' The incorporated statistician, 115-146.
#' 
#' Legendre P., and L. Legendre. 2012. Numerical ecology. Third English edition.
#' Elsevier Science, Amsterdam, Netherlands
#' 
#' Moran P. 1950. Notes on continuous stochastic phenomena. Biometrika, 17-23. 
#' 
#' Reich R., R. Czaplewski and W. Bechtold. 1994. 
#' Spatial cross-correlation of undisturbed, natural shortleaf pine stands 
#' in northern Georgia. Environmental and Ecological Statistics, 1: 201-217.
#' 
#' Rosenberg, M. 2000. The bearing correlogram: a new method 
#' of analyzing directional spatial autocorrelation. 
#' Geographical Analysis, 32: 267-278.
#' 
#' Sokal R. and N. Oden 1978. Spatial autocorrelation in biology: 
#' 1. Methodology. Biological journal of the Linnean Society, 10: 199-228.
#' 
#' Sokal R. and N. Oden. 1978. Spatial autocorrelation in biology. 
#' 2. Some biological implications and four applications of evolutionary and 
#' ecological interest. Biological Journal of the Linnean Society, 10: 229-49.
#' 
#' Sokal R. 1979. Ecological parameters inferred from spatial correlograms. 
#' In: G. Patil and M. Rosenzweig, editors. Contemporary Quantitative Ecology and 
#' elated Ecometrics. International Co-operative Publishing House: Fairland,
#' MD, pp. 167-96.
#'  
#' Sturges  H. 1926. The choice of a class interval. Journal of the American 
#' Statistical Association, 21: 65-66.
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' 
#' @export

setGeneric("eco.correlog", 
           function(Z, XY, Y = NULL, 
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
                    alternative = c("auto", "two.sided", 
                                    "greater", "less"),
                    adjust = "holm", 
                    sequential =  ifelse((as.deg), FALSE, TRUE), 
                    include.zero = TRUE,
                    cummulative = FALSE,
                    bin = c("sturges", "FD"),
                    row.sd = FALSE,
                    latlon = FALSE,
                    angle = NULL,
                    as.deg = TRUE) {
             
             
             # We start with some checks.
             
             
             cat("\n")
             
             method <- match.arg(method)
             alternative <- match.arg(alternative)
             test <- match.arg(test)  
             bin <- match.arg(bin)
             
             if(length(angle) > 1 && as.deg && test == "bootstrap") {
               stop("Only permutation test is available for bearing correlograms with degrees format output")
             }
             
             if(length(angle) > 1 && as.deg && sequential) {
               stop("Sequential correction of P values is only available for correlograms with fixed angles")
             }
             
             
             Z.class <- class(Z)
             if(Z.class != "numeric" & Z.class != "integer" &  Z.class != "matrix" & Z.class != "data.frame") {
               stop("Z must me a numeric vector, a matrix or a data.frame")
             }
             if(!is.null(Y)) {
               Y.class <- class(Y)
               if(Y.class != "numeric" & Y.class != "integer" & Y.class != "vector") {
                 stop("Y must me a numeric vector")
                 if(is.null(method)) {
                   method <- "CC"
                 }
               }
             }
             
             
             Z <- as.data.frame(Z)
             nvar <- ncol(Z)
             
             if(length(angle) > 1 && nvar > 1) {
                 stop(aue.formatLine("bearing correlograms for multiple angles 
                                     is only allowed for single variables"))
               }
             
             if(method != "CC") {
               include.zero = FALSE
             }
             
             XY <- as.data.frame(XY)
             if(ncol(XY) > 2) {
               message("XY slot with > 2 columns. The first two are taken as X-Y coordinates")
               XY <- XY[,1:2]
             } 
             
             distancia <- dist(XY)
             if(latlon == TRUE) {
               #XY <- SoDA::geoXY(XY[,2], XY[,1], unit=1)
               distancia <- as.dist(sf::st_distance(sf::st_as_sf(XY, coords=c(2,1))))
             }
             
             
             if(is.null(smax) & is.null(nclass) & is.null(seqvec)) {
               smax <- max(distancia)
             }
             
             if(!is.null(int) & !is.null(smax)) {
               
               hmuch <- sum(distancia > 0 & distancia < int)
               if(hmuch < 5) {
                 stop("Scale not apropiated.Increase distance interval")
               }
               hlast <- sum(distancia > smax - int)
               if(hlast < 5) {
                 stop("Range not apropiated. Decrease smax value")
               }
             }
             
             #additional check when class(XY) == "dist"
             if(!is.null(smax)) {
               if(smax > max(distancia)) {
                 stop("scale not apropiated. Decrease smax")
               }
             }
             
             
             j <- 0
             
             
             # Funtion to estimate the stat in each iteration
             
             select_method <- function(u, ...) {
               
               if(method == "I") {
                 out <- int.moran(Z = u,  ...)
               } else if(method == "C") {
                 out <- int.geary(Z = u,  ...)
               } else if(method == "CC") {
                 out <- int.crosscor(Z = u, Y = Y, ...)
               }    
               out
             }
             
             
             # Iterating the latter with each individual variable
             
             lista<-list()
             listaw <- eco.lagweight(XY, 
                                    int = int, 
                                    smin = smin,
                                    smax = smax, 
                                    nclass = nclass,
                                    size = size,
                                    seqvec = seqvec,
                                    row.sd = row.sd,
                                    bin = bin,
                                    cummulative = cummulative)
            
              if(!is.null(angle)) {
                if(any(angle < 0)  || any(angle > 180)) {
                  stop("angle must be a number between 0 and 180")
                }
                listanglew <- list()
                # compute directional weights
                for(i in seq_along(angle)) {
                listanglew[[i]] <- eco.bearing(listaw, angle[i])
                }
              }
             
             # unfold weights and create dummy iterators for
             # lag selection during computations. This allows
             # to select a list of lags for each angle
             if(!is.null(angle)) {
             lag <- lapply(listanglew, function(x) x@W)
             dummylag <- seq_along(lag)
             } else {
             lag <- listaw@W
             dummylag <- rep(1, nvar)
             }
             
             breaks<- listaw@BREAKS
             
             d.max <- round(breaks[-1], 3)
             d.min <- round(breaks[-length(breaks)], 3)
             classint <- listaw@MEAN
             classint <- round(classint, 3)
             cardinal <- listaw@CARDINAL
             
             #output data frame/s construction
             
             
             
             # create iterator to work around each angle with a single variable,
             # and covering the other cases
             if(length(angle) > 1) {
               seqvar <- seq_along(angle)
               seqdummy <- rep(1, length(angle))
               table_length <- length(angle)
             } else {
               seqvar <- seq_len(nvar)
               seqdummy <- seqvar
               table_length <- nvar
             }
             
             counter <- 1          
             n.classes <- length(d.min)
             # for the counter
             N_counter <- length(seqvar)
             
             #bootstrap case
             if(test == "bootstrap") {
               
               tabla <- data.frame(matrix(0, length(d.min), 5))
               tabla[, 1] <- classint
               colnames(tabla) <- c("d.mean", "obs", "lwr", "uppr", "size")
               rownames(tabla) <- paste("d=", d.min, "-", d.max, sep = "")
               lista <- replicate(table_length, tabla, simplify = FALSE)
               
               if(!is.null(angle)) {
                 names(lista) <- paste0(colnames(Z), " - ", angle, " degrees")
               } else {
                 names(lista) <- colnames(Z)
               }
               
               #repetition of select_method for each run 
               
               
               for(j in seqvar) {
                 
                 var.test <- Z[, seqdummy[j]]
                 thislag <- lag[[dummylag[j]]]
                 
                 for(i in seq_len(n.classes))  {
                 
                   cat(paste("\r", "simulations...computed",
                             round(100 * i / n.classes), "%\t\t"))
                   
                   if(!is.null(angle)) {               
                     lag2 <- thislag[[i]]
                   } else {
                     lag2 <- thislag
                   }
                   
                   est <- select_method(u = var.test, 
                                        con = lag2, 
                                        nsim = nsim,
                                        alternative = alternative,
                                        test =  test,
                                        plotit = FALSE)
                   lista[[j]][i, 2] <- est$observation
                   lista[[j]][i, 3:4] <- est$quantile
                 }
                 lista[[j]][, 5] <- cardinal
                 
                 #when zero is included, use cor.test for d = 0
                 if(include.zero) {
                   cor.zero <- cor.test(var.test, Y)
                   lista[[j]] <- rbind(c(0, 0, 0, 0, 0), lista[[j]])
                   rownames(lista[[j]])[1] <- "d=0"
                   lista[[j]][1, 1] <- 0
                   lista[[j]][1, 2] <- cor.zero$estimate
                   lista[[j]][1, 3:4] <- cor.zero$conf.int
                   lista[[j]][1, 5] <- nrow(Z) 
                 }
                 cat(paste("\r", "variable", counter, "--- total progress",
                           round(100 * counter / N_counter), "%    "))
                 counter <- counter + 1
                 cat("\n")
                
               }
               
               #permutation case
             } else if(test == "permutation") {
               
               
               tabla <- data.frame(matrix(0, length(d.min), 4))
               tabla[, 1] <- classint
               colnames(tabla) <- c("d.mean", "obs", "p.val", "size")
               rownames(tabla) <- paste("d=", d.min, "-", d.max, sep = "")
               lista <- replicate(table_length, tabla, simplify = FALSE)
               
               
               if(!is.null(angle)) {
                 names(lista) <- paste0(colnames(Z), " - ", angle, " degrees")
               } else {
                 names(lista) <- colnames(Z)
               }
               
               #repetition of select_method for each run 
               
               
               for(j in seqvar) {
                 var.test <- Z[, seqdummy[j]]
                 
                 if(!is.null(angle)) {    
                 thislag <- lag[[dummylag[j]]]
                 }
                 
                 for(i in seq_len(n.classes))  {
                   
                   cat(paste("\r", "simulations...computed",
                             round(100 * i / n.classes), "%\t\t"))
  
                   if(!is.null(angle)) {               
                   lag2 <- thislag[[i]]
                   } else {
                   lag2 <- lag[[i]]
                   }
                   
                   est <- select_method(u = var.test, 
                                        con = lag2, 
                                        nsim = nsim,
                                        alternative = alternative,
                                        test =  test,
                                        plotit = FALSE)
                   lista[[j]][i, 2] <- est$observation
                   lista[[j]][i, 3] <- est$p.value
                 }
                 lista[[j]][, 4] <- cardinal
                 #when zero is included, use cor.test for d = 0
                 if(include.zero) {
                   cor.zero <- cor.test(var.test, Y)
                   lista[[j]] <- rbind <- rbind(c(0, 0, 0, 0), lista[[j]])
                   rownames(lista[[j]])[1] <- "d=0"
                   lista[[j]][1, 1] <- 0
                   lista[[j]][1, 2] <- cor.zero$estimate
                   lista[[j]][1, 3] <- cor.test(var.test, Y)$p.value
                   lista[[j]][1, 4] <- nrow(Z)
                 }
                 
                 cat(paste("\r", "variable", counter, "--- total progress",
                           round(100 * counter / N_counter), "%"))
                 counter <- counter + 1
                 cat("\n")
               }
             }
             
             
             # Configuring the output
             
             if(length(angle) > 1 && as.deg) {
             salida <- new("eco.correlogB")
             bearing <- TRUE
             lista <- int.corvarToDeg(lista, angle)
             breaks <- angle
             } else {
             salida <- new("eco.correlog")
             bearing <- FALSE
             }

             
             if(method == "I") {
               outname <- "Moran's I"
             } else if(method == "C") {
               outname <- "Geary's C"
             } else if(method == "CC") {
               outname <- "Moran's Ixy"
             }
              
 
             # p adjustment for permutation case
             if(test == "permutation") {
             for(j in seq_along(lista)) {
               rowlen <- seq_len(nrow(lista[[1]]))
               #sequential P correction
               if(sequential) {
                 for(i in rowlen) {
                   lista[[j]][i, 3] <- (p.adjust(lista[[j]][1:i, 3], 
                                                 method= adjust))[i]
                 }
                 
               } else {
                 #standard-multiple P correction 
                 lista[[j]][ , 3] <- p.adjust(lista[[j]][ , 3], 
                                              method = adjust)
               }
             }
             }
             # end p adjustment
             
             
             salida@OUT <- lista
             salida@IN <- list(XY = XY, Z = Z,  Y =Y)
             salida@NAMES <- names(lista)
             salida@BREAKS <- breaks
             salida@CARDINAL <- as.numeric(lista[[1]][, 4])
             salida@METHOD <- outname
             if(!is.null(angle)) {
               salida@METHOD <- paste0(salida@METHOD, " (directional)")
             }
             salida@DISTMETHOD <- listaw@METHOD
             salida@TEST <- test
             salida@NSIM <- nsim
             salida@PADJUST <- paste(adjust, "-sequential:", sequential)
             salida@ANGLE <- angle
             salida@BEARING <- bearing

             cat("\ndone!\n\n")
             
             
             salida
             
           })
