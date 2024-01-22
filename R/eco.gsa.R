#' Global spatial analysis
#' 
#' @description 
#' Univariate and multivariable global spatial analysis.This program computes Moran's I, Geary's C,  Bivariate Moran's I or
#' Join-count statistics with P-values.
#' 
#' The program allows the analysis of a single variable or multiple variables. 
#' in this latter last case, the variables must be in columns and the individuals in rows.
#' 
#' In join-count analysis, a ploidy argument must be supplied. The data is then ordered with the 
#' function \code{\link{aue.sort}}. This step is required for the analysis of
#' genotypic data. An individual with the alleles A and B, coded as AB, is identical 
#' to other coded as BA. The order step ensures the AB and BA 
#' will be considered the same genotype. For the analysis of frequencies of single alleles, the input 
#' is count data (ploidy times the frequency, as provided by the slot
#' A of an ecogen object: the count data A' can be obtained as A' <- ploidy * A),
#' using the function with the arguments ploidy = 1. 
#'  
#' 
#' @param Z Vector with a variable, or matrix/data frame with variables in columns. 
#' @param Y Vector with the second variable for Moran's Ixy.
#' If Z has multiple variables, the program will compute the coefficent 
#' for each with Y.
#' @param con An object of class eco.weight obtained with the function \code{\link{eco.weight}},
#' a listw object or a matrix, giving the spatial weights for the analysis. If "con" is a matrix,
#' an attribute "xy" including the projected coordinates is required. 
#' @param method Method of analysis: "I" for Moran's I, "C" for Geary's C, "CC" for
#' the Bivariate Moran's or "JC" for Join-count.
#' @param ploidy For join count analysis: number of elements for the values of the vector passed, given value: for example,
#'  if ploidy=1, "13" and "31" are considered a same level ("31" is sorted by the program as "13"); 
#'  if ploidy = 1, "13" and "31" represent two different levels.  
#' @param nsim Number of Monte-Carlo simulations. 
#' @param alternative The alternative hypothesis. If "auto" is selected (default) the
#' program determines the alternative hypothesis.
#' Other options are: "two.sided", "greater" and "less".  
#' @param adjust Correction method of P-values for multiple tests, 
#' passed to \code{\link[stats]{p.adjust}}. Defalut is "fdr".
#' @param plotit should be generated a plot for univariate results?
#' @return The program returns an object of class "eco.gsa" with the following slots:
#' @return > METHOD method used in the analysis 
#' @return > OBS observed value when a single variable is tested
#' @return > EXP expected value when a single variable is tested
#' @return > PVAL P-value when a single variable is tested
#' @return > ALTER alternative hypotesis when a single variable is tested
#' @return > NSIM number of simulations
#' @return > MULTI table with observed and expected values, P-values and alternative
#' hypoteses when multiple variables are tested
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
#'\dontrun{
#'
#' data(eco.test)
#' 
#' # Moran's I 
#' 
#' ### one test
#' con <- eco.weight(eco[["XY"]], method = "circle", d1 = 0, d2 = 2)
#' global <- eco.gsa(Z = eco[["P"]][, 1], con = con, method = "I", nsim = 200)
#' global
#' 
#' require(adegenet)
#' con2<-chooseCN(eco[["XY"]], type = 1, result.type = "listw", plot.nb = FALSE)
#' global <- eco.gsa(Z = eco[["P"]][, 1], con = con2, method = "I", nsim = 200)
#' global
#' 
#' #-----------------------
#' # ACCESSORS USE EXAMPLE
#' #-----------------------
#' 
#' # the slots are accesed with the generic format 
#' # (ecoslot. + name of the slot + name of the object). 
#' # See help("EcoGenetics accessors")
#' 
#' # observed value
#' ecoslot.OBS(global)
#' 
#' # p-value
#' ecoslot.PVAL(global)
#' 
#' #----------------
#' # multiple tests
#' #----------------
#' data(eco3)
#' con <- eco.weight(eco3[["XY"]], method = "circle", d1 = 0, d2 = 500)
#' global <- eco.gsa(Z = eco3[["P"]], con = con, method = "I", nsim = 200)
#' global 
#' # Plot method for multivariable eco.gsa objects:
#' eco.plotGlobal(global)
#' 
#' #--------------------------------
#' # accessor use in multiple tests
#' #--------------------------------
#' 
#' ecoslot.MULTI(global)
#' 
#' 
#' #----------------------------------------
#' 
#' # Gearys's C 
#' 
#' con <- eco.weight(eco[["XY"]], method = "circle", d1 = 0, d2 = 2)
#' global.C <- eco.gsa(Z = eco[["P"]][, 1], con = con, method = "C", nsim = 200)
#' global.C
#' 
#' #----------------------------------------
#' 
#' # Bivariate's Moran's Ixy
#' 
#' con <- eco.weight(eco[["XY"]], method = "circle", d1 = 0, d2 = 2)
#' global.Ixy <- eco.gsa(Z = eco[["P"]][, 1], Y = eco[["E"]][, 1], 
#' con = con, method = "CC", nsim = 200)
#' global.Ixy
#' 
#' #----------------------------------------
#' 
#' # Join-count
#' 
#' ## using the allelic frequency matrix of an ecogen object. 
#' ## The data is diploid. Frequencies are transformed into counts
#' ## as ploidy * frequency_matrix:
#' 
#' Z = 2* eco[["A"]]
#' 
#' 
#' jc <- eco.gsa(Z[, 1], con = con, method = "JC")
#' eco.plotGlobal(jc)
#' 
#' # multiple tests
#' # using the first ten alleles of the matrix
#' global.JC <- eco.gsa(Z[, 1:10], con = con, method = "JC", nsim = 99)
#' global.JC
#' # plot method for multivariable join-count
#' eco.plotGlobal(global.JC)
#' 
#' # counting joins between genotypes in the first locus the G matrix:
#' global.JC <- eco.gsa(Z = eco[["G"]][, 1], ploidy = 2, con = con, method = "JC", nsim = 99)
#' global.JC
#' eco.plotGlobal(global.JC)
#' 
#'}
#'
#' @references 
#' 
#' Geary R. 1954. The contiguity ratio and statistical mapping. 
#' The incorporated statistician, 115-146.
#' 
#' Moran P. 1950. Notes on continuous stochastic phenomena. Biometrika, 17-23. 
#' 
#' Reich R., R. Czaplewski and W. Bechtold. 1994. 
#' Spatial cross-correlation of undisturbed, natural shortleaf pine stands 
#' in northern Georgia. Environmental and Ecological Statistics, 1: 201-217.
#' 
#' Sokal R. and N. Oden 1978. Spatial autocorrelation in biology: 
#' 1. Methodology. Biological journal of the Linnean Society, 10: 199-228.
#' 
#' Sokal R. and N. Oden. 1978. Spatial autocorrelation in biology. 
#' 2. Some biological implications and four applications of evolutionary and 
#' ecological interest. Biological Journal of the Linnean Society, 10: 229-49.
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' 
#' @export


setGeneric("eco.gsa",
           function(Z, con, Y = NULL, 
                    method =c("I", "C", "CC", "JC"),
                    nsim = 99, 
                    alternative = c("auto", "two.sided", 
                                    "greater", "less"),
                    ploidy = 1,
                    adjust = "fdr",
                    plotit =TRUE) {
             
             
             alternative <- match.arg(alternative)
             method <- match.arg(method)
             
             if(any(inherits(con, "ecoweight"))) {
               XY <- attr(con, "XY")
             } else {
               XY <- attr(con, "xy")
               con <- int.check.con(con)
             }
             if(u <- nrow(as.data.frame(Z)) != nrow(con)) {
               stop(paste0("incompatible dimension between <Z> (", u, ") and <XY> (",  nrow(con), ")"))
             }
             
             Z.class <- class(Z)
             if (method != "JC") {
               if (inherits(Z, "data.frame") || inherits(Z, "matrix")) {
                 c.Z <- apply(Z, 2, class)
                 if (any(!inherits(c.Z, c("integer", "numeric")))) {
                   stop(paste("Non-numeric data.", method, "requires numeric data"))
                 }
               } else if (!inherits(Z, c("numeric", "integer"))) {
                 stop(paste("Non-numeric data.", method, "requires numeric data"))
               }
             } else {
               if (inherits(Z, "data.frame") || inherits(Z, "matrix")) {
                 c.Z <- apply(Z, 2, class)
                 if (any(!inherits(c.Z, "factor"))) {
                   message("Note: Non-factor data present. Elements in variables will be treated as factor levels")
                 }
               } else if (!inherits(Z, c("numeric", "integer", "factor", "character"))) {
                 stop("Unknown data class")
               }
             }
             
             
             if (!is.null(Y)) {
               Y.class <- class(Y)
               if (!inherits(Y, c("numeric", "integer", "vector"))) {
                 stop("Y must be a numeric vector")
               }
             }
             
             
             nvar <- ncol(Z)
             
             
             ##### END OF CHECKS #####
             
             
             
             multiple <- NULL
             if(ncol(as.matrix(Z)) == 1) {
               multiple <- FALSE
               
               if(length(ploidy) > 1) {
                 stop("ploidy argument of length > 1 for a unique variable")
               }
             } else {
               multiple <- TRUE
               plotit <- FALSE
               
               if(method == "JC") {
                 if(length(ploidy) == 1) {
                   message("Only one ploidy value was passed. The ploidy was set as <", ploidy, "> for all the variables. ")
                   ploidy <- rep(ploidy, nvar)
                   #test
                 } else {
                   if(length(ploidy) != length(nvar)) {
                     stop("The length of ploidy argument and the number of variables is different")
                   }
                 }
               }
             }
             
             ###test#####
             
             # Funtion to estimate the stat in each iteration
             
             select_method <- function(u, Y = NULL,  
                                       ploidy = ploidy, 
                                       adjust = "none",
                                       plotit = TRUE,
                                       ...) {
               
               if(method == "I") {
                 out <- int.moran(Z = u, plotit = plotit, ...)
               } else if(method == "C") {
                 out <- int.geary(Z = u, plotit = plotit, ...)
               } else if(method == "CC") {
                 out <- int.crosscor(Z = u, Y = Y, plotit = plotit,  ...)
               } else if(method == "JC") {
                 if(ploidy > 1) {
                   # just in case of the presence of cases such as "AB", "BA"
                   u <- aue.sort(X = u, ploidy = ploidy)
                 }
                 out <- int.joincount(Z = u,  
                                      adjustjc = adjust,
                                      plotit = plotit,
                                      ...)
               }
               out
             }
             
             sel <- match(method,  c("I", "C", "CC", "JC"))
             name <- c("Moran' I", "Geary's C", 
                       "Bivariate Moran's Ixy", "Join-count")
             name <- name[sel]
             salida <- new("eco.gsa")
             
             #one test
             if(!multiple) {
               
               res <- select_method(Z,con = con, 
                                    Y = Y, 
                                    nsim = nsim,
                                    alternative = alternative,
                                    ploidy = ploidy,
                                    adjust = adjust,
                                    plotit = plotit) 

             #rearranging no JC
               
            if(method == "JC") {
                 salida@METHOD <- name
                 salida@NSIM <- nsim
                 salida@ADJUST <- adjust
                 salida@MULTI <- res$results
                 #eco.plotGlobal(salida)
                 
               
              } else {
               
               salida@METHOD <- name
               salida@OBS <- res$obs
               salida@EXP <- res$exp
               salida@PVAL <- res$p.val
               salida@ALTER <- res$alter
               salida@NSIM <- res$nsim
              }
             }
             
             if(multiple) {
               #multiple tests
               #plotit was defined as FALSE above
               
               if(method == "JC") {
                 
                 res <- lapply(1:nvar, function(i) {
                   
                   select_method(Z[, i], 
                                 con = con,
                                 nsim = nsim,
                                 alternative = alternative,
                                 ploidy = ploidy[i], 
                                 plotit = plotit,
                                 adjust = adjust) 
                   
                 })
                 
                 names(res) <- paste0(colnames(Z), "###")
                 salida@METHOD <- name
                 salida@NSIM <- nsim
                 salida@ADJUST <- adjust
                 # generate a tidy matrix
                 
                 outmat <- do.call("rbind", lapply(res, function(x) x$results))[, 1:5]
                 theNames <- gsub("###.*", "", rownames(outmat))
                 rownames(outmat)<- gsub("###", "", rownames(outmat))
                 outmat <- cbind(theNames, outmat)
                 colnames(outmat)[1] <- "var"
                 salida@MULTI <- outmat
               
               } else {
                 
                 
                 res <- lapply(1:nvar, function(i) {
                   
                   select_method(Z[, i],
                                 Y = Y,
                                 con = con, 
                                 nsim = nsim,
                                 alternative = alternative,
                                 plotit = plotit) 
                   
                 })
                 
                 names(res) <- paste0(colnames(Z))
                 res <- int.multitable(res)
                 salida@METHOD <- name
                 salida@NSIM <- nsim
                 salida@ADJUST <- adjust
                 salida@MULTI <- res$results
                 
                  } 
             }

             salida
             })


