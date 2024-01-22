#' Global and local kinship analysis 
#' 
#' @description 
#' The program computes, for a kinship matrix,  a global multilocus correlogram,
#'  or a local analysis. When a kinship matrix is not given as input, the program
#' computes the Loiselle's Fij (Kalisz et al., 2001; Loiselle et al., 1995). 
#' The program can compute a bearing correlogram (Rosenberg 2000, Born et al.
#' 2012) for the obtention of a directional approach in the global test.
#' 
#' @param eco Object of class ecogen.
#' @param method Analysis method: "global" or "local".
#' @param kinmatrix Alternative kinship matrix. The program computes
#' the Loiselle's kinship matrix (codominant data) with the genetic information
#' of the ecogen object if kinmatrix = NULL (Defaul option).
#' @param int Distance interval in the units of XY.
#' @param smin Minimum class distance in the units of XY.
#' @param smax Maximum class distance in the units of XY.
#' @param kmax Number of nearest-neighbors for local analysis.
#' @param nclass Number of classes.
#' @param seqvec Vector with breaks in the units of XY.
#' @param size Number of individuals per class.
#' @param type Weighting mode for local analysis: "knearest" for nearest neigbors,
#' "radialdist" for radial distances. Default is knearest.
#' @param cubic Should a cubic interpolation (res~ ln(dij)) be performed, 
#' for the regression residuals (res)  of (kinship)ij ~ ln(dij) ? Default TRUE.
#' @param testclass.b Carry a permutation test within each individual class? Default TRUE.
#' @param testmantel.b Should a Mantel test for testing the slope (b) be performed? Default TRUE.
#' @param jackknife Compute jackknife within each individual class for obtention of
#' the standard deviation (SD) of the coancestry (class) values. Default TRUE.
#' @param normLocal Normalize the local kinship values ([local_kinship-mean]/sd)? Default TRUE
#' @param bin Rule for constructing intervals when a partition parameter (int, 
#' nclass or size) is not given. Default is Sturge's rule (Sturges, 1926). Other
#' option is Freedman-Diaconis method (Freedman and Diaconis, 1981).
#' @param nsim Number of Monte-Carlo simulations. 
#' @param test If test = "bootstrap", the program generates a bootstrap 
#' resampling and the associated confidence intervals of the null hypothesis.
#'  If test = "permutation" (default) a permutation test is made and the P-values 
#'  are computed.   
#' @param alternative The alternative hypothesis. If "auto" is selected (default) the
#' program determines the alternative hypothesis.
#' Other options are: "two.sided", "greater" and "less".  
#' @param adjust P-values correction method for multiple tests 
#' passed to \code{\link[stats]{p.adjust}}. Default is "holm".
#' @param sequential Use the Holm-Bonberroni sequential method for
#' adjustment of P-values (Legendre and Legendre, 2012) in global analysis? Default TRUE.
#' @param conditional Logical. Use a conditional randomization? (Anselin 1998, Sokal and Thomson 2006). The option "auto"
#' sets conditional = TRUE for LISA methods and G, as suggested by Sokal (2008).
#' @param cummulative Should a cummulative correlogram be construced?.
#' @param row.sd Logical. Should be row standardized the matrix? Default FALSE 
#' (binary weights).
#' @param latlon Are the coordinates in decimal degrees format? Default FALSE. If TRUE,
#' the coordinates must be in a matrix/data frame with the longitude in the first
#' column and latitude in the second. The position is projected onto a plane in
#' meters with the function \code{\link{geoXY}}.
#' @param angle direction for computation of a bearing correlogram (angle in degrees between 0 and 180).
#' Default NULL (omnidirectional).
#' 
#' @details
#' The GLOBAL ANALYSIS mode, computes a multilocus correlogram, with a detailed
#' summary (see the content of the slot OUT  in the "return" section). 
#' It also computes (see details about the slot SP in the "return" section):
#' - the slope of the kinship individual values vs the logarithm of the distance, (kinship)ij ~ ln(dij), with a jackknife confidence
#' interval 
#' - a Mantel test for testing the association between  (kinship)ij
#' and ln(dij) 
#' - The Sp statistic (Vekemans and Hardy, 2004) with confidence intervals
#' - A cubic interpolation of (kinship)ij ~ ln(dij) residuals vs ln(dij)
#' 
#' A directional approach is based on the bearing analysis method, and consists in the 
#' obtention of a directional correlogram using the method of Rosenberg (2000). A
#' slope is computed for the logarithm of D' (Born et al 2012), where D' is the distance matrix
#' between individuals weighted by cos(alpha - B)^2, being alpha the angle
#' between individuals and B the desired direction angle. With B = 0
#' the direcction analyzed follows the positive x axis, with B = 0 the positive y axis,
#'  and with B = 180 the negative x axis, respectively.
#' 
#' 
#' The LOCAL ANALYSIS mode, computes a local kinship estimate, based in a weighted 
#' mean (for each individual). The significance of each local statistic
#' is computed using a permutation test, as in eco.lsa (see ?"eco.lsa"). 
#' Default option do not adjust the individual P values 
#' for multiple comparisons.
#' 
#' @return 
#' 
#' 
#' For the global analysis, the program returns an object of class "eco.IBD" 
#' with the following slots:
#' 
#' @return > OUT analysis output. 
#' 
#' 
#' In the permutation test case contains: 
#' - d.mean: mean class distance;
#' - d.log: mean logarithm of the class distance;
#' - obs, exp, alter, p.val: observed, and expected value of the statistic
#' under randomization, alternative, P value;
#' - mean.jack, sd.jack, Jack.CI.inf, Jack.CI.sup: jackknifed mean and SD,
#' and confidence intervals for the statistic;
#' - null.lwr, nul.uppr: lower and upper bound of the jackknife
#'  confidence interval for the statistic;
#' - cardinal: number of individuals in each class;
#' 
#' 
#' In the bootstrap test case contains: 
#' - d.mean: mean class distance;
#' - d.log: mean logarithm of the class distance;
#' - obs: observed value of the statistic;
#' - mean.jack, sd.jack, Jack.CI.inf, Jack.CI.sup: jackknifed mean and SD,
#' and confidence intervals for the statistic;
#' - null.lwr, nul.uppr: lower and upper bound of the jackknife
#'  confidence interval for the statistic;
#' - cardinal: number of individuals in each class;
#' 
#' @return > GLOBALTEST Oden's (1984) global test of significance for the correlogram.
#' The test consists in checking if the most significant kinship coefficent is 
#' significant at a Bonferroni- corrected significance level of alpha' = alpha/k, where
#' k is the number of distance classes of the correlogram; alpha is set to 0.05.
#' The program return the values: "SIGNIFICANT" or "NOT-SIGNIFICANT"
#' 
#' @return > IN analysis input data
#' @return > SP Sp statistic results
#' 
#' 
#' It contains:
#' 
#' - the regression model;
#' - information about the distance interval used 
#' for the regression (restricted);
#' -  slope (bhat) information (bhat = estimate, SD= bhat jacknife SD, theta =  bhat jackknife mean, 
#' CI 5\% and 95\% = 95\% confidence interval for bhat);
#' -  X-intercept = dij intercept (in the original units) for the line with slope "bhat", 
#' F1 = first class statistic value, and  F1 5\% and 95\% = confidence interval
#' for the first class statistic;
#' - mantel.obs.b = observed value of the Mantel test between
#' kinship(Fij) and ln(dij); mantel.pval.b = Mantel test P value;
#' - sp = Sp statistics (sp = Sp observed value, 
#' CI 5\% and 95\% = 95\% confidence interval 
#' for Sp);
#' - cubic_model = cubic model for (kinship)ij ~ ln(dij) r
#' esiduals vs ln(dij);
#' 
#' 
#' @return > BEAKS breaks
#' @return > CARDINAL number of elements in each class
#' @return > NAMES variables names
#' @return > METHOD analysis method 
#' @return > DISTMETHOD method used in the construction of breaks
#' @return > TEST test method used (bootstrap, permutation)
#' @return > NSIM number of simulations
#' @return > PADJUST P-values adjust method for permutation tests
#' 
#' @return ------
#' 
#' 
#' @return For the local analysis, the program returns an object of class "eco.lsa" 
#' with the following slots:
#' @return > OUT results
#' 
#' 
#' > In the permutation test case it contains: 
#' 
#' - d.mean: mean class distance
#' - obs, exp, alter, p.val: observed, and expected value of the statistic
#' under randomization, alternative, P value;
#' - null.lwr, nul.uppr: lower and upper bound of the jackknife
#'  confidence interval for the statistic;
#' - cardinal: number of individuals in each class;
#' 
#' 
#' > In the bootstrap test case it contains: 
#' - d.mean: mean class distance;
#' - obs: observed value of the statistic;
#' - null.lwr, nul.uppr: lower and upper bound of the jackknife;
#'  confidence interval for the statistic;
#' - cardinal: number of individuals in each class;
#' 
#' 
#' @return > METHOD method (coefficent) used in the analysis 
#' @return > TEST test method used (bootstrap, permutation)
#' @return > NSIM number of simulations
#' @return > PADJUST P-values adjust method for permutation tests
#' @return > COND conditional randomization (logical)
#' @return > XY input coordinates 
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
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' data(eco.test)
#' 
#' # ---global analysis---
#' 
#' globaltest <- eco.malecot(eco=eco, method = "global", smax=10,
#'                          size=1000)
#' eco.plotCorrelog(globaltest)    # Significant mean class coancestry classes at   
#'                     # individual level (alpha = 0.05, 
#'                     # out of the red area), 
#'                     # and family-wise P corrected values (red-blue
#'                     # points, indicated in the legend)
#'
#' # ecoslot.SP(globaltest) contains:
#' # - the slope (bhat) and values with confidence intervals
#' #  of the regression reg = kinship ~ ln(distance_between_individuals)
#' #- A Mantel test result for assesing the relation between
#' #  between  kinship and ln(distance_between_individuals)
#' #- A cubic interpolation between the residuals of reg and 
#' #  ln(distance_between_individuals)
#' #- the sp statistic and its confidence interval
#'
#' # ecoslot.OUT(globaltest) contains:
#' # - In permutation case, the values of mean and log-mean distance    
#' #   classes; observed class value; expected + alternative + P value,
#' #   the bootstrap null confidence intervals and 
#' #   jackknife statistics (jackknifed mean, jackknifed SD, and
#' #                         CI for the class statistic)
#'
#' # - In bootstrap case, the values of mean and log-mean distance
#' #   classes;the bootstrap null confidence intervals and 
#' #   jackknife statistics (jackknifed mean, jackknifed SD, and
#' #                         CI for the class statistic)
#' 
#' 
#' # A directional approach based in bearing correlograms, 30 degrees
#' globaltest_30 <- eco.malecot(eco=eco, method = "global", smax=10,
#'                          size=1000, angle = 30)
#' eco.plotCorrelog(globaltest) 
#'
#' #----------------------------------------------------------#
#' # ---local analysis---
#' 
#' 
#' # (using the spatial weights). 
#' 
#' # ---local analysis with k nearest neighbors---
#' 
#' 
#' 
#' localktest <- eco.malecot(eco=eco, method = "local",
#'                          type = "knearest", kmax = 5, 
#'                          adjust = "none")
#' eco.plotLocal(localktest)
#'
#'
#' # ---local analysis with radial distance---
#' 
#' localdtest <- eco.malecot(eco=eco, method = "local",
#'                         type = "radialdist", smax = 3, 
#'                         adjust = "none")
#'                         
#' eco.plotLocal(localdtest)                    # rankplot graphic (see ?"eco.rankplot")
#' 
#'                                          # Significant values
#'                                          # in blue-red scale, 
#'                                          # non significant 
#'                                          # values in yellow
#'
#' eco.plotLocal(localktest, significant = FALSE)    # significant and non
#'                                               # signficant values
#'                                               # in blue-red scale
#'
#' # The slot OUT of localktest (ecoslot.OUT(localktest)) and localdtest 
#' # (ecoslot.OUT(localdtest)) contains:
#' # - the mean distance per individual, observed value of the
#' #   statistic, expected + alternative + P value + null hypotesis
#' #   confidence intervals,  or boostrap confidence intervals in 
#' #   permutation or bootstrap cases, respectively.
#' }
#' 
#' @references
#' 
#' Born C., P. le Roux, C. Spohr, M. McGeoch, B. Van Vuuren. 2012.
#' Plant dispersal in the sub-Antarctic inferred from anisotropic genetic structure.
#' Molecular Ecology 21: 184-194.
#' 
#' Double M., R. Peakall, N. Beck, and Y. Cockburn. 2005. 
#' Dispersal, philopatry, and infidelity: dissecting 
#' local genetic structure in superb fairy-wrens (Malurs cyaneus). 
#' Evolution 59: 625-635.
#' 
#' Kalisz S., J. Nason, F.M. Handazawa, and S. Tonsor. 2001. 
#' Spatial population genetic structure in Trillium grandiflorum: 
#' the roles of dispersal, mating, history, and selection. 
#' Evolution 55: 1560-1568.
#' 
#' Loiselle B., V. Sork, J. Nason, and C. Graham. 1995. 
#' Spatial genetic structure of a tropical understory shrub, 
#' Psychotria officinalis (Rubiaceae). 
#' American Journal of Botany 1420-1425.
#' 
#' Oden, N., 1984. Assessing the significance of a spatial correlogram. 
#' Geographical Analysis, 16: 1-16.
#' 
#' Rosenberg, M. 2000. The bearing correlogram: a new method 
#' of analyzing directional spatial autocorrelation. 
#' Geographical Analysis, 32: 267-278.
#' 
#' Vekemans, X., and O. Hardy. 2004. New insights from fine-scale 
#' spatial genetic structure analyses in plant populations. 
#' Molecular Ecology, 13: 921-935.
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' 
#' @export


# kmax for kmax nearest neigbors in local analysis. Use "fdr" instead
# of "holm" if multiple correction results too extrict.

eco.malecot <- function(eco, 
                        method = c("global", "local"), 
                        kinmatrix =NULL,
                        int = NULL,
                        smin = 0,
                        smax = NULL,
                        nclass = NULL,
                        kmax = NULL,
                        seqvec = NULL,
                        size = NULL,
                        type = c("knearest", "radialdist"),
                        cubic = TRUE,
                        testclass.b = TRUE,
                        testmantel.b = TRUE,
                        jackknife = TRUE,
                        cummulative = FALSE,
                        normLocal = TRUE,
                        nsim = 99, 
                        test = c("permutation", "bootstrap"), 
                        alternative = c("auto","two.sided", 
                                        "greater", "less"),
                        sequential = TRUE, 
                        conditional = c("AUTO", "TRUE", "FALSE"),
                        bin = c("sturges", "FD"),
                        row.sd = FALSE,
                        adjust = "holm",
                        latlon = FALSE,
                        angle = NULL) {
  
  XY <- eco@XY
  
  if(ncol(XY)>2) {
    message("XY with > 2 columns. The first two are taken as X-Y coordinates")
    XY <-XY[,1:2]
  } 
  
  if(latlon == TRUE) {
    XY <- geoXY(XY[,2], XY[,1], unit=1)
  } 
  
  distancia <- dist(XY)
  logdistancia <- log(distancia)
  
  control <- c(!is.null(smax), !is.null(kmax), !is.null(seqvec))
  if(sum(control) == 0) {
    smax <- max(distancia)
  } 
  if(sum(control) > 1) {
    stop("multiple selection of smax, kmax, seqvec; please enter only one parameter")
  }
  conditional <- match.arg(conditional)
  method <- match.arg(method)
  type <- match.arg(type)
  test <- match.arg(test)
  alternative <- match.arg(alternative)
  bin <- match.arg(bin)
  geno <- ecoslot.A(eco)
  loc.fac <- as.numeric(eco@INT@loc.fac)
  nloc <- max(loc.fac)
  nind <- nrow(ecoslot.A(eco))
  crit <- abs(qt(0.975, nloc - 1)) #critical value for CI
  
  #kinship matrix
  if(is.null(kinmatrix)) {
    kin <- int.kin.loiselle(geno, loc.fac)
  } else {
    kin <- as.matrix(kinmatrix)
    diag(kin) <- 0
  }
  
  #tests
  
  if(test == "permutation") {
    replace <- FALSE
  } else {
    replace <- TRUE
  }
  
  #method selection
  if(method == "global") {
    type <- "correlogram"
    
    if(is.null(smax) & is.null(seqvec)) {
      stop("please provide a smax argument or a vector of classes for dist method")
    }
    conditional <- FALSE
    modelmatrix.comp <- eco.lagweight(XY, 
                                      int = int, 
                                      smin = smin,
                                      smax = smax, 
                                      nclass = nclass,
                                      size = size,
                                      seqvec = seqvec,
                                      row.sd = row.sd,
                                      bin = bin,
                                      cummulative = FALSE)
    
    
    if(!is.null(angle)) {
      if(angle < 0  || angle > 180) {
        stop("angle must be a number between 0 and 180")
      }
      modelmatrix.comp <- eco.bearing(modelmatrix.comp, angle)
    }
    
    modelmatrix <- modelmatrix.comp@W
    nbins <- length(modelmatrix)
    
  } else if(method == "local") {
    
    sequential <- FALSE
    
    if(conditional == "AUTO") {
      conditional <- FALSE
    }
    
    if(type == "knearest" & is.null(kmax)) {
      stop("kmax argument must be not-null with local knearest analysis")
    }
    if(type == "radialdist" & is.null(smax)) {
      stop("smax argument must be not-null with local radialdist analysis")
    }
    if(type == "knearest") {
      modelmatrix <- eco.weight(XY = XY, 
                                method = "knearest", 
                                k = kmax,
                                row.sd = row.sd)
      
      
      if(!is.null(angle)) {
        if(angle < 0  || angle > 180) {
          stop("angle must be a number between 0 and 180")
        }
        modelmatrix<- eco.bearing(modelmatrix, angle)
      }
      
      modelmatrix <- modelmatrix@W
    } else if (type == "radialdist") {
      modelmatrix <- eco.weight(XY = XY, 
                                method = "circle", 
                                d1 = 0, 
                                d2 = smax,
                                row.sd = row.sd)
      
      if(!is.null(angle)) {
        if(angle < 0  || angle > 180) {
          stop("angle must be a number between 0 and 180")
        }
        modelmatrix<- eco.bearing(modelmatrix, angle)
      }
      
      modelmatrix <- modelmatrix@W
    }
    nbins <- nind
  }
  
  
  ### function for computing observed values in each case
  
  select_method <- function(kinmat) {
    
    if(method != "local") {
      
      out <- sapply(1:nbins, function(i) {
        if(sum(modelmatrix[[i]]) == 0) {
          stop("no individuals in class", i)
        }
        sum(kinmat * modelmatrix[[i]]) / sum(modelmatrix[[i]])
      })
      
    } else {
      out <- apply((kinmat * modelmatrix), 1, sum) / apply(modelmatrix, 1, sum)
      if(normLocal){
        out <- (out - mean(out, na.rm = TRUE)) / sd(out, na.rm = TRUE)
      }
      out[is.infinite(out)] <- NA             #when there are no connections
      out[is.na(out)] <- NA 
    }
    out
  }
  
  obs <- select_method(kin)
  
  # Here stats the permutation test. The kinship matrix is randomized, 
  #then select_method is called each time
  
  
  if(nsim != 0) {
    
    cat("Performing randomization test...")
    
    
    samp <- 1:nind
    kin.perm <- kin

    # create a matrix with randomized kinship values 
    random.kin <- sapply(1:nsim, function(i) {
      
      #permuted matrix for each repetition
      #conditional case
      if(conditional) {
        for(k in samp) {
          order.z <- sample(samp[-k], replace = replace)
          kin.perm[k,-k] <- kin.perm[k, ][order.z]
        }
        
        #free case
      } else {
        shuffle.kin <- sample(samp, replace = replace)
        kin.perm <- kin[shuffle.kin, shuffle.kin]
      }
      
      # call function for computing observed values, with the randomized kinship matrix
      select_method(kin.perm)
      
    }) # end randomization test
  
}
  
  if(method == "global") {
    meandistance <- modelmatrix.comp@MEAN
    logdist <- modelmatrix.comp@LOGMEAN
    breaks.kin <- modelmatrix.comp@BREAKS
    cardinal <- modelmatrix.comp@CARDINAL
    if(!cummulative) {
      d.max <- round(breaks.kin[-1], 3)
      d.min <- round(breaks.kin[-length(breaks.kin)], 3)
      dist.dat<-paste("d=", d.min, "-", d.max, sep = "")
    } else {
      dist.dat <- paste("d=0-d=", breaks.kin[-1], sep="")
    }
    
  } else if(method == "local") {
    sequential <- FALSE
    distancia <- as.matrix(distancia)
    meandistance <- apply(modelmatrix * distancia, 1, sum) / apply(modelmatrix, 1, sum)
    cardinal  <- apply(modelmatrix, 1, sum)
    logdist <- numeric()
    breaks.kin <- 0
    d.max <- 1
    d.min <- rep(1, nrow(modelmatrix))
    dist.dat<- rownames(XY)
  }
  
  
  if(test == "permutation") {
    tab <- data.frame(matrix(0, nrow = length(dist.dat), ncol= 13))
    rownames(tab) <- dist.dat
    colnames(tab) <- c("d.mean","d.log","obs", "exp","alter", 
                       "p.val", "mean.jack", "sd.jack", "Jack.CI.inf",
                       "Jack.CI.sup", "null.lwr", "null.uppr", 
                       "cardinal")
    tab[, 1] <- meandistance
    tab[, 2] <- logdist
    tab[, 3] <- round(obs, 4) #obs
    tab[, 13] <- cardinal 
    
    if(nsim != 0) {
      
        for(i in 1:nrow(tab)) {
        if(!is.na(obs[i])) {
        ran <-  int.random.test(random.kin[i, ], obs[i],
                                test = "permutation", 
                                alternative = alternative, 
                                nsim = nsim) 
        
          tab[i, 4] <- round(ran[[2]], 4) #exp
          tab[i, 5] <- ran[[3]]           #alter
          tab[i, 6] <- ran[[4]] #p.val
          tab[i, 11] <- round(ran[[5]][1], 4) #null.lwr
          tab[i, 12] <- round(ran[[5]][2], 4) #null.uppr
          
          if(sequential) {
              tab[i, 6] <- (p.adjust(tab[1:i, 6], method= adjust))[i]
            }
            
          } else  {
          tab[i, 4] <- NA
          tab[i, 5] <- NA
          tab[i, 6] <- NA
          tab[i, 11] <- NA
          tab[i, 12] <- NA
          } 
        }
            
         if(!sequential) {
              tab[ , 6] <- p.adjust(tab[ , 6], method = adjust)
        }
            
        tab[, 6] <- round(tab[, 6], 5)
            
    } else {
      
      # if no test, fill with NA
      tab[, 4:12] <- NA
    } 

  } else if(test == "bootstrap") {
    tab <- data.frame(matrix(nrow = length(d.min), ncol=10))
    rownames(tab) <- dist.dat
    colnames(tab) <- c("d.mean","d.log", "obs", "mean.jack", "sd.jack", 
                       "Jack.CI.inf", "Jack.CI.sup", "null.lwr", "null.uppr",  
                       "cardinal")
    tab[, 1] <- round(meandistance, 3)
    tab[, 2] <- round(logdist, 3)
    tab[, 3] <- round(obs, 4) # obs
    tab[, 10] <- cardinal     #cardinal
    
    if(nsim != 0) {
    for(i in 1:nrow(tab)) {
      rand <-  int.random.test(random.kin[i, ],
                               obs[i], 
                               test = "bootstrap", 
                               nsim = nsim)
      if(!is.na(obs[i])) {
        tab[i, 8:9] <- round(rand[[2]], 4) #null lwr, uppr
      }
    }
    
    } else  {
      
    # if no test, fill with NA
    tab[, 8:9] <- NA
    }
  }
  
  
  
  #SD AND SP ANALYIS FOR "global"
  
  if(method == "global") {  
    ##############calculo de sp 
    dist.sp <- as.matrix(distancia)
    
    if(!is.null(angle)) {
      XDIST<- dist(XY[, 1])
      YDIST<- dist(XY[, 2])
      ind_angle <- 180 * atan2(YDIST, XDIST) / pi
      ind_angle <- as.matrix((cos(round(ind_angle) - angle)) ^ 2)
      logdistmat <- log(as.matrix(distancia) * ind_angle)
    } else {
      logdistmat <- as.matrix(logdistancia)
    }

    Fij.sp <- kin
    dmin <- min(breaks.kin)
    dmax <- max(breaks.kin)
    if(dmin == 0) {
      dmin <- exp(-100)
    }
    restricted <- (dist.sp >= dmin & dist.sp <= dmax)
    logdist.sp <- logdistmat[restricted]
    Fij.sp <- Fij.sp[restricted]
    
    modelo <- lm(Fij.sp ~ logdist.sp)
    bhat <- as.numeric(coef(modelo)[2])
    sp.stat <- - bhat /(1 - (tab$obs)[1])
    x.intercept <- exp(1)^(-coef(modelo)[1]/coef(modelo)[2])
    
    ###cubic interpolation
    if(cubic) {
      Fij.detrended <- modelo$residuals
      cubica <- lm(Fij.detrended ~ poly(logdist.sp, degree = 3))
      capture.output(cubica.final <- step(cubica, scope = list(lower = Fij.detrended ~ 1,
                                                               upper = cubica)))
    }
    
    ####permutation test for b. Permutation over locations. 
    #if(testclass.b) {
    #cat("\n")
    #bhat.test <- rep(0, nsim)
    #for(k in 1:nsim) {
    #  samp <- sample(nrow(kin))
    #  kin2 <- kin[samp, samp]
    #  obs.sim <- numeric()
    #  for(i in 1:nbins) {
    #    if(sum(modelmatrix[[i]]) == 0) {
    #      stop("no individuals in class", i)
    #    }
    #    obs.sim[i] <- sum(kin2 * modelmatrix[[i]]) / sum(modelmatrix[[i]])
    #  }
    #  modelo.sim <- lm(kin2[restricted] ~ logdist.sp)
    #  bhat.sim <- as.numeric(coef(modelo.sim)[2])
    #  bhat.test[[k]] <- bhat.sim
    #  cat(paste("\r", "testing slope...computed",
    #            100 * round(k /nsim, 1), "%"))
    #}
    
    #btest <- int.random.test(bhat.test, bhat, nsim)
    #}
    
    #--------------------MANTEL TEST FOR SP-------------------------------------#
    
    if(testmantel.b && nsim != 0) {
      mantelcells <- (row(logdistmat)>col(logdistmat)) & restricted
      logdist.mantel <- logdistmat[mantelcells]
      Fij.mantel <- kin[mantelcells]
      obs <- cor(logdist.mantel, Fij.mantel)
      
      repsim <- numeric()
      N <- nrow(logdistmat)
      repsim <- sapply(1:nsim, function(i) {
        samp <- sample(N)
        temp <- (kin[samp, samp])[mantelcells]
        cor(logdist.mantel, temp)
      })
      
      resmantel <- int.random.test(repsim = repsim, 
                                   obs = obs,
                                   nsim = nsim,
                                   test = "permutation",
                                   alternative = "auto")
      mantel.obs <- resmantel$obs
      mantel.pval <- resmantel$p.val
    } else{
      mantel.obs <- NA
      mantel.pval <- NA
    }
    
    ##################################################################
    if(jackknife) {
      #jackknife over loci
      obs.jack <- matrix(0, nrow = nloc, ncol = nbins)
      
      
      #jackknifing kinship per class-----------#
      cat("\n")
      kin.loci <- list()
      nrepet <- nloc * nbins
      counter <- 1
      for(i in 1: nloc) {
        col.delete <- which(loc.fac == i)
        loc.fac.jack <- as.numeric(as.factor(loc.fac[-col.delete]))  # renumber the factor from 1 to nloc-1
        geno.jack <- geno[, -col.delete]
        kin.jack <- int.kin.loiselle(geno.jack, loc.fac.jack)
        kin.loci[[i]] <- kin.jack
        for(j in 1:nbins) {
          obs.jack[i, j] <- sum(kin.jack * modelmatrix[[j]]) / sum(modelmatrix[[j]])
          cat("\r", paste("jackknife over loci...", 100 * round(counter / (nrepet), 1), "%"))
          counter <- counter + 1
        }
      }
      cat("\n")
      #---------#
      
      ### error bars
      obs.real <- t(replicate(nloc, tab$obs))                                                     #value without jackknife                                                        #mean of jackknife values
      pseudo <- (nloc * obs.real) - ((nloc - 1) * obs.jack)                   #pseudo-value
      theta <- apply(pseudo, 2, mean)
      pseudo.variance <- apply(pseudo, 2, var)
      sd.jack <- sqrt(pseudo.variance / nloc) 
      ####
      #F1 CONFIDENCE INTERVALS
      temp.F1 <- crit * sd.jack[1]
      F1.confint <- drop(cbind(theta[1] - temp.F1,theta[1] + temp.F1))
      ###
      #ALL CONFIDENCE INTERVALS
      temp.FCI <- crit * sd.jack
      FCI <- drop(cbind(theta - temp.FCI, theta + temp.FCI))
      colnames(FCI) <- c("Jack.lwr", "Jack.uppr")
      if(test == "permutation") {
        tab[, 7] <- round(theta, 4)
        tab[, 8] <- round(sd.jack, 4)
        tab[, 9:10] <- round(FCI, 4)
      } else {
        tab[, 4] <- round(theta, 4)
        tab[, 5] <- round(sd.jack, 4)
        tab[, 6:7] <- round(FCI, 4)
      }
      
      #---------------------------------------------------------------------------#
      #JACKKNIFE OF THE SLOPE OVER LOCI, USING INDIVIDUALS 
     bhat.jack <- sapply(1:nloc, function(i) {
        mod.jack <- lm(kin.loci[[i]][restricted] ~ logdist.sp)
        as.numeric(coef(mod.jack)[2])
      })
     
      pseudo.bhat <- (nloc * rep(bhat, nloc)) - ((nloc - 1) * bhat.jack)                   #pseudo-value
      theta.bhat <- mean(pseudo.bhat)
      pseudovar.bhat <- var(pseudo.bhat)
      sd.bhat <- sqrt(pseudovar.bhat / nloc)
      temp <- crit * sd.bhat
      bhat.confint <- drop(cbind(theta.bhat - temp,theta.bhat + temp))
      sp.confint <- - bhat.confint /(1 - (tab$obs)[1])
      sp.confint <- drop(c(sp.confint[2], sp.confint[1]))
      names(sp.confint) <- c("5%", "95%")
      
      bhat <- round(c(bhat, sd.bhat, theta.bhat, bhat.confint[1],
                      bhat.confint[2], x.intercept, 
                      (tab$obs)[1], F1.confint[1], F1.confint[2]), 4)
      
      names(bhat) <- c("bhat", "SD","theta", "CI.5%", "CI.95%", 
                       "X-intercept", "F1", "F1.CI.5%", "F1.CI.95%")
      
      sp.stat <- c(sp.stat, sp.confint[1], sp.confint[2])
      
      names(sp.stat) <- c("sp", "CI.5%", "CI.95%")
      
    }
    
    distance <- data.frame(meandistance, logdist)
    colnames(distance) <- c("dist", "log.dist")
    rownames(distance) <- 1:nrow(distance)
    distance <- t(distance) 
    
    
    out.sp <- list(model = modelo,
                   distance = distance,
                   restricted = round(c(dmin, dmax), 4),
                   bhat = bhat,
                   mantel.obs.b = round(mantel.obs, 4),
                   mantel.pval.b = round(mantel.pval, 4),
                   sp = round(sp.stat, 5), 
                   cubic_model = ifelse(cubic, cubica.final, NA)
    )
    
    
    #######################
    
  }
  
  if(method == "global") {

    
    salida <- new("eco.IBD")
    salida@OUT <- list(tab)
    salida@GLOBALTEST <- ifelse(min(tab$p.val) < 0.05/nrow(tab), "SIGNIFICANT", "NOT-SIGNIFICANT")
    salida@IN <- list(XY = XY, ECOGEN = eco@INT)
    salida@BREAKS <- breaks.kin
    salida@CARDINAL <- cardinal
    salida@NAMES <- colnames(eco@G)
    salida@METHOD <- c("Kinship", type) 
    
    salida@DISTMETHOD <- method
    salida@TEST <- c(test, conditional) 
    salida@NSIM <- nsim
    salida@PADJUST <- paste(adjust, "-sequential:", sequential)
    salida@SP <- out.sp
    salida@ANGLE <- angle
    salida@BEARING <- FALSE
    
  } else if (method == "local") {
    
    salida <- new("eco.lsa")
    if(test == "permutation") {
      tab <- tab[, c(1,3,4,5,6,11,12,13)]
    } else {
      tab <- tab[, c(1,3,8, 9,10)]
    }
    tab <- data.frame(tab, round(aue.rescale(tab$obs, "one.one"), 4))
    colnames(tab)[ncol(tab)] <- "obs.res"
    salida@OUT <- tab
    salida@METHOD <- "local kinship"
    salida@TEST <- test
    salida@NSIM <- nsim
    salida@COND <- ifelse(conditional ==  "TRUE", TRUE, FALSE)
    salida@PADJ <- adjust
    salida@XY <- data.frame(XY)
  }
  
  if(!is.null(angle)) {
    salida@METHOD[1] <- paste0(salida@METHOD[1], " (directional)")
  }
  
  cat("\n")
  cat("done!")
  cat("\n\n")
  
  salida
}
