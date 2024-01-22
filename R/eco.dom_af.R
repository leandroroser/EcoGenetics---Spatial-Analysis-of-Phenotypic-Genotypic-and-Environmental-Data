
#' Compute allele frequencies for dominant data using different methods
#' @param x ecopop or genpop object, or matrix/data.frame with allele frequencies
#' @param method Method used to compute the allelic frequencies. 
#' Can be one of 'zhivor' ( Zhivotovsky 1999, with uniform prior),  
#' 'zhivonu' (Zhivotovsky 1999, with non uniform prior), 
#' 'rawfreq' (square root method).
#' @references 
#' Zhivotovsky, L. A. (1999). Estimating population structure in diploids 
#' with multilocus dominant DNA markers. Molecular Ecology, 8:907-913.
#' @examples
#' \dontrun{
#' data(eco.test)
#' my_ecopop <- ecogen2ecopop(eco, "pop")
#' eco.dom_af(my_ecopop)
#' }
#' @author Juan Vilardi \email{vilardi@@ege.fcen.uba.ar}
#' @export

setGeneric("eco.dom_af", function(x, method = c("zhivor", "zhivonu", "rawfreq")) {
  
  method <- match.arg(method)
  
  # get matrix matr with 'allele counts'
  
  if (inherits(x, "genpop")) {
    matr <- x@tab
  } else if(inherits(x, "ecopop")) {
    if (x@INT@allele_data == "frequency") {
      stop("ecopop object with genetic data as counts needed, 
         but this object has allele frequencies")
    }
    matr <- x@AF
  } else if(inherits(x, "matrix") || inherits(x, "data.frame")) {
    matr <- x
  } else {
    stop("object of invalid class")
  }
  
  
  
  # frsq is at the beginning = matr
  frsq <- matr
  
  ncol_matr <- ncol(matr)
  
  # matrix to save variances
  matrs <- matrix(nrow = nrow(matr), ncol = ncol(matr)/2)
  rownames(matrs) <- rownames(matr)
  
  
  # start computations method zhivor
  
  zhivor <- function(x) {
    
    qestr <- function(m, n) {
      beta(m + 1, n + 1)/beta(m + 0.5, n + 1)
    }
    
    s2 <- function(m, n) {
      beta05 <- beta(m + 0.5, n + 1)
      beta(m + 1.5, n + 1)/beta05 - (beta(m + 1, n + 1)/beta05)^2
    }
    
    for (i in seq(1, ncol(matr) - 1, 2)) {
      for (j in seq_len(nrow(matr))) {
        condition <- frsq[j, i] + frsq[j, i + 1]
        if (condition == 0) {
          matr[j, i] <- matr[j, i + 1] <- matrs[j, (i + 1)/2] <- NA
        } else if (condition == 1) {
          matr[j, i] <- frsq[j, i]
          matr[j, i + 1] <- frsq[j, i + 1]
          matrs[j, (i + 1)/2] <- 0
        } else {
          matr[j, i] <- qestr(frsq[j, i], frsq[j, i + 1])
          matr[j, i + 1] <- 1 - matr[j, i]
          matrs[j, (i + 1)/2] <- s2(frsq[j, i], frsq[j, i + 1])
        }
      }
    }
    
    result <- list(matr, matrs)
    names(result) <- c("freq", "var")
    result
  }
  
  # method zhivonu
  
  zhivonu <- function(x) {
    
    index <- seq(1, ncol_matr - 1, 2)
    
    Rm <- rowSums(matr[, index])/rowSums(matr)
    frsq <- matR <- matr
    
    elem <- matr[, index]
    elem_1 <- matr[, seq(2, ncol_matr, 2)]
    
    matR[, index] <- ifelse(elem == 0, 0, elem/(elem + elem_1))
    matR[, seq(2, ncol_matr, 2)] <- ifelse(elem_1 == 0, 0, elem_1/(elem + elem_1))
    VR <- rowSums(matR[, index]^2 * (elem + elem_1))/rowSums(matr) - Rm^2
    
    top <- (Rm * (1 - Rm)/VR - 1)
    a <- top * Rm
    b <- top * (1 - Rm)
    
    qestrnu <- function(m, n, a, b) beta(m + a + 0.5, n + b)/beta(m + a, n + b)
    
    s2nu <- function(m, n, a, b) {
      beta_sub <- beta(m + a, n + b)
      beta(m + a + 1, n + b)/beta_sub - (beta(m + a + 0.5, n + b)/beta_sub)^2
    }
    
    for (i in index) {
      for (j in seq_len(nrow(matr))) {
        condition <- frsq[j, i] + frsq[j, i + 1]
        if (condition == 0) {
          matr[j, i] <- matr[j, i + 1] <- matrs[j, (i + 1)/2] <- NA
        } else if (condition == 1) {
          matr[j, i] <- frsq[j, i]
          matr[j, i + 1] <- frsq[j, i + 1]
          matrs[j, (i + 1)/2] <- 0
        } else {
          matr[j, i] <- qestrnu(frsq[j, i], frsq[j, i + 1], a[j], b[j])
          matr[j, i + 1] <- 1 - matr[j, i]
          matrs[j, (i + 1)/2] <- s2nu(frsq[j, i], frsq[j, i + 1], a[j], b[j])
        }
      }
    }
    
    result <- list(matr, matrs)
    names(result) <- c("freq", "var")
    result
  }
  
  # method rawfreq
  
  rawfreq <- function(x) {
    
    index <- seq(1, ncol_matr - 1, 2)
    index2 <- seq(2, ncol_matr, 2)
    index3 <- seq_len(ncol(matrs))
    
    for (i in index) {
      for (j in seq_len(nrow(matr))) {
        condition <- matr[j, i] + matr[j, i + 1]
        if (condition == 0) {
          frsq[j, i] <- frsq[j, i + 1] <- NA
        } else if (condition == 1) {
          frsq[j, i] <- matr[j, i]
          frsq[j, i + 1] <- matr[j, i + 1]
        } else {
          frsq[j, i] <- sqrt(matr[j, i]/(matr[j, i] + matr[j, i + 1]))
          frsq[j, i + 1] <- 1 - frsq[j, i]
        }
      }
    }
    
    
    matrs[, index3] <- (1 - frsq[, index]^2)/(4 * (matr[, index] + matr[, index2]))
    LM <- frsq
    numer <- frsq[, index]
    Vx <- frsq[, index]^2*(1-frsq[, index]^2)/(matr[, index] + matr[, index2])
    denom <-1 - Vx/(8*frsq[, index]^4)
    LM[, index] <- numer/denom
    LM[, index2] <- 1 - LM[, index]
    result <- list(frsq, matrs, LM)
    names(result) <- c("freq", "var", "LM")
    return(result)
  }
  
  if (method == "zhivor") {
    zhivor(x)
  } else if (method == "zhivonu") {
    zhivonu(x)
  } else if (method == "rawfreq") {
    rawfreq(x)
  }
})
