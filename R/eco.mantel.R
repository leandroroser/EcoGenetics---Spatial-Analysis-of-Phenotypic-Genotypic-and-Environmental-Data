#' Mantel and partial Mantel tests, with truncation option
#' 
#' @description Mantel test or Partial Mantel test for distance matrices d1 and d2, 
#' or partial Mantel test for d1 and d2, conditioned on the matrix dc.
#'  The test can be performed for truncated distances (Legendre et al. 2015) or for a particular direction
#' (Falsetti and Sokal, 1993) using a weights object generated with  \code{\link{eco.bearing}}.
#' @param d1 Distance matrix.
#' @param d2 Distance matrix.
#' @param dc Distance matrix (optional).
#' @param con Binary eco.weight object used for truncation, or a weights object obtained with eco.bearing.
#' @param thres Threshold distance used for truncation. Distances above the threshold are
#' set as 4 times the threshold. If thres is null, and con is not null,
#' the parameter set to the maximum distance observed in d2.
#' @param truncMat Matrix used for truncation (default = d2)
#' @param method Correlation method used for the construction of the statistic 
#' ("pearson", "spearman" or "kendall"). Kendall's tau computation is slow.
#' @param nsim Number of Monte-Carlo simulations. 
#' @param alternative The alternative hypothesis. If "auto" is selected (default) the
#' program determines the alternative hypothesis. Other options are: "two.sided", "greater" and "less".  
#' @param plotit Plot a histogram of the simulations? 
#' @param ... Additional arguments passed to \code{\link[stats]{cor}}.
#' @return An object of class "eco.gsa" with the following slots:
#' @return > METHOD method used in the analysis 
#' @return > OBS observed value 
#' @return > EXP expect value 
#' @return > PVAL P-value 
#' @return > ALTER alternative hypotesis 
#' @return > NSIM number of simulations
#' 
#' 
#' \strong{ACCESS TO THE SLOTS}
#' The content of the slots can be accessed 
#' with the corresponding accessors, using
#' the generic notation of EcoGenetics 
#' (<ecoslot.> + <name of the slot> + <name of the object>).
#' See help("EcoGenetics accessors") and the Examples
#' section below
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' data(eco.test)
#' 
#' ### Ordinary Mantel test ###
#' eco.mantel(d1 = dist(eco[["P"]]), d2 = dist(eco[["E"]]), nsim = 99)  
#' 
#' ### Partial Mantel test ###
#' pm <- eco.mantel(d1 = dist(eco[["P"]]), d2 = dist(eco[["E"]]), 
#' dc = dist(eco[["XY"]]), nsim = 99)                               
#' 
#' ### Truncated Mantel test ###
#' # checking threshold in a correlogram:
#' corm <- eco.cormantel(M = dist(eco[["P"]]), XY = eco[["XY"]], nsim = 99)
#' eco.plotCorrelog(corm)
#' # Correlation is around 0 when distance between points is > 5
#' 
#' # creating a weights object for truncation
#' con <- eco.weight(eco@XY, method="circle", d2=5)
#' # compute a truncated mantel test
#' eco.mantel(dist(eco[["P"]]), dist(eco[["XY"]]), con=con)
#' 
#' ### Directional Mantel test ###
#' # analyzing with a Mantel test, in a direction of 35 degrees counterclockwise from E.
#' con2 <- eco.bearing(XY = eco[["XY"]], theta = 37)
#' eco.mantel(dist(eco[["P"]]), dist(eco[["XY"]]), con = con2)
#' 
#' #-----------------------
#' # ACCESSORS USE EXAMPLE
#' #-----------------------
#' 
#' # the slots are accessed with the generic format 
#' # (ecoslot. + name of the slot + name of the object). 
#' # See help("EcoGenetics accessors")
#' 
#' ecoslot.OBS(pm)     # slot OBS (observed value)
#' ecoslot.PVAL(pm)    # slot PVAL (P-value) 
#' 
#' }
#'
#' @references 
#' 
#' Falsetti A., and Sokal R. 1993. Genetic structure of human populations
#'  in the British Isles. Annals of Human Biology 20: 215-229.
#' 
#' Legendre P. 2000. Comparison of permutation methods for the partial correlation
#' and partial Mantel tests. Journal of Statistical Computation and Simulation,
#' 67: 37-73.
#' 
#' Legendre P., and M. Fortin. 2010. Comparison of the Mantel test and 
#' alternative approaches for detecting complex multivariate relationships 
#' in the spatial analysis of genetic data. Molecular Ecology Resources, 
#' 10: 831-844.
#' 
#' Mantel N. 1967. The detection of disease clustering and a generalized 
#' regression approach. Cancer research, 27: 209-220.
#' 
#' Smouse P. Long and R. Sokal. 1986. Multiple regression and correlation 
#' extensions of the Mantel test of matrix correspondence. Systematic zoology, 627-632.
#' 
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' 
#' @export


setGeneric("eco.mantel", 
           function(d1, d2, dc = NULL, con = NULL, thres = NULL,
                    truncMat = c("d2","d1","dc"),
                    method = c("pearson", "spearman", "kendall"),
                    nsim = 99, alternative = c("auto", "two.sided", "less", "greater"), 
                    plotit = TRUE, ...) {
             
             alternative <- match.arg(alternative)
             method <- match.arg(method)
             truncMat <- match.arg(truncMat)
             
             control <- c(inherits(d1, "dist"), inherits(d2, "dist"), inherits(dc, "dist"))
             sumcontrol<- sum(control)
             
             
             if(!is.null(con)) {
               if(!inherits(con, "eco.weight")) {
                 stop("con must be an eco.weight object")
               }
               if(!is.null(thres) && (!is.numeric(thres) ||length(thres) > 1)) {
                 stop("please provide a threshold value (numeric of length 1)")
               }
               if(is.null(thres)) {
                 thres <- max(d2)
                 message(paste0("the threshold was set as the maximum distance found in ", truncMat, " (", round(thres, 6),")"))
               }
               if(!all(con@W %in% c(0,1)) &&  is.null(con@ANGLE)) {
                 stop("con must have binary weights")
               }
               dTrunc <- as.dist(con@W)
               # for truncated test 
               if(is.null(con@ANGLE)){
               dTrunc[dTrunc == 0] <- 4 * thres
               } 
               if(truncMat == "d1") {
                 d1 <-  d1 * dTrunc
               } else if(truncMat == "d2") {
                 d2 <- d2 * dTrunc
               }  else if(truncMat == "d3") {
                 d3 <- d3 * dTrunc
               }
               
             }
             
             
             if(sumcontrol != 3 & !is.null(dc)) { 
               nondist <- which(!(control))
               dcont <- c("d1", "d2", "dc")
               dcont <- dcont[nondist]
               dcont <- paste(dcont, collapse=", ")
               stop(paste("non dist object/s found in the arguments. 
                          The imput data must be of class dist:", dcont))
             }
             
             res <- int.mantel(d1 = d1, d2 = d2, dc = dc,
                               method = method, nsim = nsim,
                               test = "permutation", 
                               alternative = alternative, 
                               plotit = plotit)
             
             
             if(is.null(dc)) { 
               method.mantel <- "Mantel test"
             } else {
               method.mantel <- "Partial Mantel test"
             }
             
             salida <- new("eco.gsa")
             salida@METHOD <- c(method.mantel, method)
             salida@OBS <- round(res$obs, 4)
             salida@EXP <- round(res$exp, 4)
             salida@PVAL <- res$p.val
             salida@ALTER <- res$alter
             salida@NSIM <- nsim
             
             salida
             
             })
