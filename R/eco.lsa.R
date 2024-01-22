#' Local spatial analysis
#' 
#' @description 
#' Univariate and multivariable local spatial analysis. This program computes Getis-Ord G
#' and G*, and LISA's (local Moran and local Geary) statistics for the data Z,
#' with P-values or bootstrap confidence intervals.
#' 
#' @param var Vector, matrix or data frame for the analysis. Multiple variables in columns.
#' @param con An object of class eco.weight obtained with the function \code{\link{eco.weight}},
#' a "listw" object, or a matrix, containing the weights for the analysis. 
#' If a matrix, an attribute "xy" with the projected coordinates is required. 
#' @param method Method of analysis: "G" for Getis-Ord G, "G*" for Getis-Ord G*, 
#'  "I" for local Moran's I or "C" for local Geary's C. 
#' @param zerocon If zerocon = 0 the program assigns the value 0 to those individuals
#'  with no connections; if zerocon = NA the program assigns NA. Default is NA.
#' @param nsim Number of Monte-Carlo simulations. 
#' @param conditional Logical. Should be used a
#' conditional randomization? (Anselin 1998, Sokal and Thomson 2006). The option "auto"
#' sets conditional = TRUE for LISA methods and G, as suggested by Sokal (2008).
#' @param test If test = "bootstrap", for each individual test,
#' the program generates a bootstrap 
#' resampling and the associated confidence intervals of the null hypotesis.
#' If test = "permutation" (default) a permutation test is made and the P-value
#' is computed.   
#' @param alternative The alternative hypothesis for "permutation" test.
#'  If "auto" is selected (default) the
#' program determines the alternative hypothesis in each individual test.
#' Other options are: "two.sided", "greater" and "less".
#' @param adjust Correction method of P-values for multiple tests, 
#' passed to \code{\link[stats]{p.adjust}}. Defalut is "none" (no correction).
#' @param multi multiple output format results. "list" for object with a list 
#' of individual test for each variable, or "matrix" for results as matrices
#' of multiples variables.
#' @param pop numeric factor with the population of each individual. Optional 
#' for multiple tests with multi = "matrix".
#' 
#' @return For single test, the program returns an object of class "eco.lsa" 
#' with the following slots:
#' @return > OUT results - table with output results. 
#' 
#' --> If test =  "permutation": observed value of the statistic , null confidence 
#' interval and #rescaled observed value to [-1, 1] range, as in Sokal (2006)
#' 
#' --> If test =  "bootstrap": observed and expected value
#' of the statistic, alternative hypotesis, null confidence interval and 
#' rescaled observed value to [-1, 1] range, as in Sokal (2006)
#' 
#' @return > METHOD method (coefficent) used in the analysis 
#' @return > TEST test method used (bootstrap, permutation)
#' @return > NSIM number of simulations
#' @return > PADJUST P-values adjust method for permutation tests
#' @return > COND conditional randomization (logical)
#' @return > XY input coordinates 
#' 
#' @return For multiple test, if the parameter multi = "list", the program
#' returns a list of eco.lsa objects (one element for each variable).
#' 
#' @return For multiple test, if the parameter multi = "matrix", the program
#' returns an object of class "eco.multilsa" with the following slots:
#' @return > METHOD method used in the analysis 
#' @return >  TEST test method used (bootstrap, permutation)
#' @return >  NSIM number of simulations
#' @return >  PADJUST P-values adjust method for permutation tests
#' @return >  COND conditional randomization (logical)
#' @return >  XY input coordinates  
#' @return >  OBS observed value
#' @return >  EXP expected value
#' @return >  ALTER test alternative
#' @return >  PVAL pvalue for permutation test
#' @return >  LWR lower confidence interval bound of the null hypotesis
#' @return >  UPPR upper confidence interval bound of the null hypotesis
#' @return >  OBS.RES rescaled observed value to [-1, 1] range, as in Sokal (2006)
#' 
#' \strong{ACCESS TO THE SLOTS}
#' The content of the slots can be accessed 
#' with the corresponding accessors, using
#' the generic notation of EcoGenetics 
#' (<ecoslot.> + <name of the slot> + <name of the object>).
#' See help("EcoGenetics accessors") and the Examples
#' section below
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' data(eco.test)
#' 
#' #---------------------------------------------------------------------------#
#' 
#' 
#' #####################
#' # LOCAL MORAN'S I
#' #####################
#' 
#' DETAILED EXAMPLE
#' 
#' #-------------------------
#' # TESTING PHENOTYPIC DATA-
#' #-------------------------
#' 
#' set.seed(10)
#' 
#' # test for a single variable---------------------------------
#' #computing weights
#' 
#' con <- eco.weight(eco[["XY"]], method = "knearest",  k = 4, row.sd = TRUE) 
#' # row standardized weights = TRUE
#' 
#' # test for the first trait of the data frame P 
#' localmoran <- eco.lsa(eco[["P"]][, 1], con, method = "I", nsim = 99)     
#' 
#' # "rankplot" graph
#' eco.plotLocal(localmoran)
#' 
#' # test for several variables---------------------------------
#' 
#' # ordering the factor "pop" in increasing order and the object "eco"
#' # in relation to this ordered factor prior to the multivariate analysis.
#' # This step is important for "localplot" graphs.
#' 
#' eco <- eco[order(eco[["S"]][,1])]
#' 
#' #computing weights with the ordered object
#' 
#' con <- eco.weight(eco[["XY"]], method = "knearest",  k = 4, row.sd = TRUE) 
#' # row standardized weights = TRUE
#' 
#' all.traits <- eco.lsa(eco[["P"]], con, method = "I", nsim = 99)     
#' 
#' # Plot of the phenotypic spatial patterns
#' 
#' # "rasterplot" graph 
#' eco.plotLocal(all.traits)
#' 
#' # in grey: non significant results (P > 0.05)
#' # set significant = FALSE for showing significant and no significant results
#' eco.plotLocal(all.traits, significant = FALSE)
#'
#' # single plots using "rankplot" graphs
#' all.single.traits <- eco.lsa(eco[["P"]],con, method = "I", nsim = 99, multi="list")
#' eco.plotLocal(all.single.traits)
#' 
#' # removing legends for a better visualization
#' eco.plotLocal(all.single.traits, legend = FALSE)
#' # - individual plots support ggplot2 sintax (plot equivalent to the previous):
#' eco.plotLocal(all.single.traits) + ggplot2::theme(legend.position="none")
#' 
#' 
#' #-------------------------
#' # TESTING GENOTYPIC DATA-
#' #-------------------------
#' 
#' # eco[["A"]] is a matrix with the genetic data of "eco"
#' # as frequencies for each allele in each individual.
#' 
#' head(eco[["A"]])      # head of the matrix - 40 alleles
#' 
#' # ordering the factor "pop" in increasing order and the object "eco"
#' # in relation to this ordered factor prior to the multivariate analysis.
#' # This step is important for "localplot" graphs
#' 
#' data(eco.test) # for security this resets the data (unordered)
#'
#' eco <- eco[order(eco[["S"]][,1])] # ordering
#' 
#' # computing weights with the ordered object
#' 
#' con <- eco.weight(eco[["XY"]], method = "knearest",  k = 4, row.sd = TRUE) 
#' # row standardized weights = TRUE
#' 
#' # test for a single allele
#' localmoran.geno <-  eco.lsa(eco[["A"]][, 32], con, method = "I", nsim = 99)
#' 
#' # test for several alleles -  40 alleles (it runs in less than 1 min 
#' # for 99 simulations per allele;  999 simulations takes ~ 11 s per allele, 
#' # less than 8 min in total.) 
#' all.alleles <-  eco.lsa(eco[["A"]], con, method = "I", nsim = 99)
#' 
#' # plot all alleles to get an overview of the spatial patterns
#' eco.plotLocal(all.alleles)
#' 
#' # in grey: non significant results (P > 0.05)
#' # set significant = FALSE for showing significant and no significant results
#' eco.plotLocal(all.alleles, significant = FALSE)
#' 
#' # counting individuals with P < 0.05 for each allele (5 * 225 /100 ~  12 significant tests 
#' # by random)
#' signif <- apply(ecoslot.PVAL(all.alleles), 2, function(x) sum (x < 0.05))
#' 
#' # filtering alleles, loci with > 12 significant individual tests
#' 
#' A.local <- eco[["A"]][, signif > 12]     #filtered matrix
#' 
#' all.alleles.f <-  eco.lsa(eco[["A"]][, signif > 12] , con, method = "I", nsim = 99)
#' 
#' 
#' # Plot of the genotypic spatial patterns using "localplot" graphs
#' 
#' eco.plotLocal(all.alleles.f)
#' 
#' 
#' ## using "rankplot" graphs
#' 
#' all.sf <- eco.lsa(A.local,  2, eco.lsa, con, method = "I", nsim = 99, multi = "list")
#' eco.plotLocal(all.sf, legend = FALSE) 
#' 
#' 
#' #####################
#' # GETIS-ORD'S G*
#' #####################
#' 
#' con<- eco.weight(eco[["XY"]], method = "knearest",  k = 4, self = TRUE) # self = TRUE for G*
#' getis.ak <- eco.lsa(eco[["P"]][, 1], con, method = "G*", nsim = 99, adjust = "none")
#' getis.ak
#' 
#' ### to plot the results, the function "eco.lsa" calls "eco.rankplot"
#' ### (see ?eco.rankplot) when test = "permutation" and "eco.forestplot" (see ?eco.forestplot)
#' ###  when test = "bootstrap"
#' 
#' p <- eco.plotLocal(getis.ak)      # rankplot graph
#' p    #  points with colors of the color-scale:  
#'      #  points with P < 0.05. Yellow points : points with P > 0.05
#' p <- eco.plotLocal(getis.ak, significant = FALSE)  
#' p    # all points have a color of the color-scale 
#' 
#' #-----------------------
#' # ACCESSORS USE EXAMPLE
#' #-----------------------
#' 
#' # the slots are accessed with the generic format 
#' # (ecoslot. + name of the slot + name of the object). 
#' # See help("EcoGenetics accessors")
#' 
#' ecoslot.OUT(getis.ak)
#' 
#' ## bootstrap example
#' getis.akb <- eco.lsa(eco[["P"]][, 1], con, method = "G*", nsim = 99, test = "bootstrap")
#' p <- eco.plotLocal(getis.akb)      # forestplot graph
#' 
#' p2 <- eco.plotLocal(getis.akb, interactivePlot = FALSE)  
#' p2 + ggplot2::theme_bw()   # the plot can be modified with ggplot2
#'                           # In this case, the background is modified  (white color)
#' 
#' #---------------------------------------------------------------------------#
#'  
#' #####################
#' # GETIS-ORD'S G
#' #####################
#' 
#' con <- eco.weight(eco[["XY"]], method = "knearest", k = 4) 
#' # self = FALSE for G
#' getis <- eco.lsa(eco[["P"]][, 1], con, method = "G", nsim = 99)
#' eco.plotLocal(getis)
#'
#' #---------------------------------------------------------------------------#
#' 
#' #####################
#' # LOCAL GEARY'S C
#' #####################
#' 
#' con<- eco.weight(eco[["XY"]], method = "knearest",  k = 4, row.sd = TRUE) 
#' # row standardized weights = TRUE
#' localgeary <- eco.lsa(eco[["P"]][, 1], con, method = "C", nsim = 99, adjust = "none")
#' eco.plotLocal(localgeary)
#' 
#'}
#'
#' @references 
#' 
#' Anselin L. 1995. Local indicators of spatial association-LISA. 
#' Geographical analysis. 27: 93-115.
#' 
#' Getis A., and J. Ord. 1992. The analysis of spatial association by
#' use of distance statistics. Geographical analysis, 24: 189-206. 
#' 
#' Ord J., and A. Getis. 1995. Local spatial autocorrelation statistics:
#' distributional issues and an application. Geographical analysis, 27: 286-306.
#' 
#' Sokal R., N. Oden and B. Thomson. 1998. Local spatial autocorrelation
#' in a biological model. Geographical Analysis, 30: 331-354.
#' 
#' Sokal R. and B. Thomson. 2006. Population structure inferred by local 
#' spatial autocorrelation: an example from an Amerindian tribal population. 
#' American journal of physical anthropology, 129: 121-131.
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export


setGeneric("eco.lsa",
           function(var, con, method = c("G*","G", "I", "C"),
                    zerocon = NA, nsim = 99, 
                    conditional = c("auto", "TRUE", "FALSE"),
                    test = c("permutation", "bootstrap"),
                    alternative = c("auto", "two.sided", 
                                    "greater", "less"), 
                    adjust = "none",
                    multi = c("matrix", "list"),
                    pop = NULL) {
             
             
             # GENERAL CONFIGURATION ------------------------------------------------#
            
             var <- as.data.frame(var)
             
             if(dim(var)[2] == 1) {
               multitest <- FALSE
             } else {
               multitest <- TRUE
             }
             
             
             method <- toupper(method)
             method <- match.arg(method)
             
             test <- match.arg(test)
             alternative.i <- match.arg(alternative)
             multi <- match.arg(multi)
             
             conditional <- as.character(conditional)
             ## 'conditional'  configuration - no class/case dependent
             conditional <- match.arg(conditional)
             if(conditional == "auto") {
               if(method == "G*") {
                 conditional <- FALSE
               } else {
                 conditional <- TRUE
               }
             }
             if(conditional == "TRUE") {
               conditional <- TRUE
             }
             if(conditional == "FALSE") {
               conditional <- FALSE
             }
             
             # population configuration
             
             if(!is.null(pop) && multi == "matrix") {
               
               if(length(pop) != nrow(var)) {
                 stop(paste("incorrect factor length", paste("(", length(pop), ")", sep = "")))
               }
               
               # create a matrix with pop
               pop <- as.integer(as.factor(pop))
               orden <- order(pop)
               pop <- pop[orden]
               grp <- t(matrix(rep(pop, ncol(var)), ncol = ncol(var)))
               
               # order var
               var <- var[orden, ]

             } else {
               grp <- NULL
             }
           
             
             ## weights configuration
             
             zerocon2 <- match(zerocon, c(0, NA))
             if(is.null(zerocon2)) {
               stop("zerocon argument must be 0 or NA")
             }
             
             
             if(inherits(con, "eco.weight")) {
               XY <- con@XY
               con <- con@W
             } else {
               con.temp <- int.check.con(con)
               if(is.null(attr(con, "xy"))) {
                 stop("The weight matrix requires an attribute <xy> with the coordinates")
               }
               con <- con.temp
               XY <- attr(con, "xy")
             }
             
             if(u <- nrow(as.data.frame(var)) != nrow(con)) {
               stop(paste0("incompatible dimension between <var> (", u, ") and <con> (",  nrow(con), ")"))
             }
             ## single/multiple test configuration 
             
         
             
             ########################################################################
             # SINGLE TEST (CORE) FUNCTION------------------------------------------#
             ########################################################################
             
             singletest <- function(Z) {
             
             ## general configuration. method selection
             var.names <- colnames(Z)
             Z <- as.numeric(Z[, 1]) # control numeric variable
             ## NAs removed. When finalized the algorithm, NA individuals are restored.
             noNA <- !is.na(Z) 
             Z <- Z[noNA]
             con.cl <- con[noNA, noNA]
             n <- length(Z)
             ## element to fill during restore of NAs
             
             counter <- 0
             cat("\n")
             
             # STATISTICS SECTION---------------------------------------------------#
             ############## Getis - Ord's G*/G ####################
             
             if(method == "G*"| method == "G") {
               classG <- method
               
               Gm <- t(replicate(n, Z))
               
               if(classG=="G") {
                 if(any(diag(con.cl) != 0)) {
                   diag(con.cl) <- 0
                   msg <- "Non zero elements in the diagonal of the weight matrix, 
                                self-included individuals. These values are set to 0 for G"
                   warning()
                 }
                 diag(Gm) <- 0
                 n2 <- n - 1
               } else if(classG=="G*") {
                 if(any(diag(con.cl) == 0)) {
                   stop(paste("Individuals non self-included in the weight matrix",
                              "(zeros present in the diagonal). Self-included individuals",
                              "are required for G*"))
                 }
                 n2 <- n
               }
               
               #specific function for G* or G
               select.method <- function(Gm) {
                 
                 G <- con.cl * Gm
                 G <- apply(G, 1, sum)
                 Gm2 <- Gm ^ 2
                 
                 meanG <- apply(Gm, 1, sum) / n2
                 desvsqG <- apply(Gm2, 1, sum) / n2
                 desvsqG <- desvsqG - meanG ^ 2
                 W <- apply(con.cl, 1, sum)
                 S1 <- apply(con.cl ^ 2, 1, sum)
                 denom <- n2 * S1 - W ^ 2
                 denom <- sqrt(desvsqG * denom / (n2-1))
                 numer <- (G - W * meanG)
                 G <- numer / denom
                 G
               }
               
               
               ############## moran ####################
               
             } else if(method == "I") {
               
               Z2 <- Z - mean(Z)
               m2 <-  sum(Z2 ^ 2) / n 
               Gm <- t(replicate(length(Z), Z2))
               
               select.method <- function(Gm) {
                 coef.sup <- apply(Gm * con.cl, 1, sum)
                 out <- Z2 * coef.sup / m2
                 out
               }
               
               ############## geary ####################
               
             } else if(method == "C") {
               
               Z2 <- Z - mean(Z)
               m2 <-  sum(Z2 ^ 2) / n 
               Gm <- as.matrix(dist(Z, upper = T))
               
               select.method <- function(Gm) {
                 
                 num <- Gm ^ 2
                 num <- con.cl * Gm 
                 num <- apply(num, 1, sum)
                 out <- num / m2
                 out
                 
               }
               
             }
             
             obs <- select.method(Gm)
             
             # no simulations case
             if(nsim == 0) {
               res <- new("eco.lsa")
               tab <- data.frame(obs)
               
               #NA action - restore NA individuals
               if(any(!noNA)) {
                 tmp <- matrix(nrow = length(noNA), ncol = ncol(tab))
                 colnames(tmp) <- colnames(tab)
                 tab <- as.matrix(tab)
                 tmp[noNA, ] <- tab
                 tab <- as.data.frame(tmp)
                 for(i in c(1,2,4,5,6)) {
                   tab[, i] <- as.numeric(as.character(tab[, i]))
                 }
               }
               
               if(!is.null(rownames(Z))) {
                 rownames(tab) <- rownames(Z)
               }
               
               tab <- data.frame(tab, round(aue.rescale(tab$obs, "one.one"), 4))
               colnames(tab)[2] <- "obs.res"
               
               res@NAMES <- var.names
               res@OUT <- tab
               res@XY <- data.frame(XY)
               res@METHOD <- method
               res@NSIM <- nsim
               res@COND <- conditional
               res
               return(res)
             }
             
             # TEST SECTION---------------------------------------------------------#
             
             if(test == "permutation") {
               replace <- FALSE
             } else {
               replace <- TRUE
             }
             
             monte.c <- matrix(0, nrow(Gm), nsim)
             #fixed pivot
             if(conditional) {
               for(k in 1:nsim) {
                 Gm.test <- Gm
                 samp <- 1:nrow(Gm)
                 for(i in samp) {
                   order.Z <- sample(samp[-i], replace = replace)
                   Gm.test[i, -i]<- Gm.test[i, order.Z]
                 }
                 monte.c[, k] <- select.method(Gm.test)
                 
                 counter <- counter + 1
                 
                 cat(paste("\r", "simulations...computed",
                          round(100 * counter / nsim), "%"))
                 
               }
               #free sampling
             } else {
               for(i in 1:nsim) {
                 monte.c[, i] <- select.method(Gm[,sample(ncol(Gm), 
                                                          replace = replace)])
                 
                 
                 # only for single tests- multiple test avoid this
                 if(!multitest) {
                 counter <- counter + 1
                 cat(paste("\r", "simulations...computed",
                           ceiling(100 * counter / nsim), "%"))
                 }
               }
             }
             
             
             monte.c <- t(monte.c)
             
             tab <- int.random.test(repsim = monte.c, 
                                    obs = obs,
                                    nsim = nsim,
                                    test = test, 
                                    alternative = alternative,
                                    adjust = adjust)
             
             
             
             # OUTPUT SECTION-------------------------------------------------------#
            
             connect <- apply(con.cl, 1, sum)
             if(is.na(zerocon)) {
               tab[which(connect == 0), ] <- rep(NA, 3)
             } else {
               tab[which(connect == 0), ] <- rep(0, 3)
             }
             
             
             res <- new("eco.lsa")
            
              #NA action - restore NA individuals
             if(any(!noNA)) {
               tmp <- matrix(nrow = length(noNA), ncol = ncol(tab))
               colnames(tmp) <- colnames(tab)
               tab <- as.matrix(tab)
               tmp[noNA, ] <- tab
               tab <- as.data.frame(tmp)
               for(i in c(1,2,4,5,6)) {
               tab[, i] <- as.numeric(as.character(tab[, i]))
                     }
             }
             
             
             if(!is.null(rownames(Z))) {
               rownames(tab) <- rownames(Z)
             }
             
             tab <- data.frame(tab, round(aue.rescale(tab$obs, "one.one"), 4))
             colnames(tab)[ncol(tab)] <- "obs.res"

             res@NAMES <- var.names
             res@OUT <- tab
             res@XY <- data.frame(XY)
             res@METHOD <- method
             res@TEST <- test
             res@NSIM <- nsim
             res@COND <- conditional
             
             if(test == "permutation") {
               res@TEST <- test
               res@NSIM <- nsim
               res@COND <- conditional
               res@PADJ <- adjust
             }
             res
           }
           # END SINGLE TEST FUNCTION-----------------------------------------------#
           
           
           ##########################################################################
           # RUN NOW SINGLE / MULTIPLE TEST ----------------------------------------#
           ##########################################################################
           
           #-(1) single test case -------------------------------#
           #-----------------------------------------------------#
           if(!multitest) {
             res <- singletest(var[, 1, drop = FALSE])
           } 
           
           #-(2) multiple tests case ----------------------------#
           #-----------------------------------------------------#
           counter <- 1
           ntests <- ncol(var)
           
           # (2- 1/2)(list format--------------------------------#
           if(multitest && multi == "list") {
             res <- new("eco.listlsa")
             counter <- 1
             for(i in 1:ncol(var)) {
             res[[i]] <- singletest(var[, i, drop = FALSE])
           
             cat(paste("\r", "variable", counter, "--- total progress",
                       round(100 * counter / ntests), "%"))
             counter <- counter  + 1
             }
           }
           
           # (2- 2/2) matrix format------------------------------#
           #-----------------------------------------------------#
           if(multitest && multi == "matrix") {
             # names configuration-------------
             ind.names <- rownames(var)
             var.names <- colnames(var)
             # add names function
             add.names <- function(outmat) {
               rownames(outmat) <- rownames(var)
               colnames(outmat) <- colnames(var)
               outmat
             }
             #perform here a multiple test. Use for for counting progress
             all.traits <- list()
             for(i in seq_len(ntests)) {
             all.traits[[i]] <- singletest(var[, i, drop = FALSE])
             
             cat(paste("\r", "variable", counter, "--- total progress",
                       round(100 * counter / ntests), "%"))
             
             counter <- counter  + 1
             }
             
             # GENERATE RASTERS (individuals x traits)
             obs.multi <- sapply(all.traits, function(x) ecoslot.OUT(x)[,1])
            
             nmax <- ncol(ecoslot.OUT(all.traits[[1]]))
             obs.res.multi <- sapply(all.traits, function(x) ecoslot.OUT(x)[, nmax])
             
             if(nsim != 0) {
             if(test  == "permutation") {
             exp.multi <- sapply(all.traits, function(x) ecoslot.OUT(x)[,2])
             
             # alternative as intergers
             alter.inter <- function(v) {
               vout <- as.integer(rep(0, length(v)))
               vout[v == "two.sided"] <- 0L
               vout[v == "greater"] <- 1L
               vout[v == "less"] <- 2L
               vout
             }
             
             alter.multi <- sapply(all.traits, function(x) alter.inter(ecoslot.OUT(x)[,3]))
             pval.multi <- sapply(all.traits, function(x) ecoslot.OUT(x)[,4])
             lwr.multi <- sapply(all.traits, function(x) ecoslot.OUT(x)[,5])
             uppr.multi <- sapply(all.traits, function(x) ecoslot.OUT(x)[,6])
             }
             if(test  == "bootstrap") {
               lwr.multi <- sapply(all.traits, function(x) ecoslot.OUT(x)[,2])
               uppr.multi <- sapply(all.traits, function(x) ecoslot.OUT(x)[,3])
             }
             }
             
             #---output construction----#
             res <- new("eco.multilsa") 
             res@METHOD <- method
             res@NSIM <- nsim
             res@XY <- data.frame(XY)
             res@OBS <- t(add.names(obs.multi))
             res@OBS.RES <- t(add.names(obs.res.multi))
             
             if(nsim != 0) {
             res@TEST <- test
             res@COND <- conditional
             res@LWR <- t(add.names(lwr.multi))
             res@UPPR <- t(add.names(uppr.multi))
             }
             
             res@POP <- grp
             
               if(test == "permutation" && nsim != 0) {
                 # permutation case specific
                 res@PADJ <- adjust
                 res@EXP <- t(add.names(exp.multi))
                 res@ALTER <- t(add.names(alter.multi))
                 res@PVAL <- t(add.names(pval.multi))
               }
             # end multiple - matrix format  -----------------------#
             }
           
           cat("\t\n\ndone!\n\n")
           
             res
             
           })
