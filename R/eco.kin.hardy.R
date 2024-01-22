#' Kinship and relationship estimation for dominant markers
#' @param x ecogen, genind matrix or data.frame. In case of matrix or data frame,
#' tha data consists in a table with individuals in rows and allele counts 
#' in columns.
#' @param fi Assumed Fi
#' @description Kinship and relationship estimation following Hardy (2003).
#' @return List with three slots containing, respectively, the heretability 
#' of each locus, relationship and kinship values
#' @export
#' @author Juan Vilardi \email{vilardi@@ege.fcen.uba.ar}

eco.kin.hardy <- function(x, fi) { 
  
  if(inherits(x, "genind")) {
    mata <- x@tab
  } else if(inherits(x, "ecogen")) {
    mata <- x@A
  } else if(inherits(x, "matrix") || inherits(x, "data.frame")) {
    mata <- x
  }
  
  
  nrow_mata <- nrow(mata)
  ncol_mata <- ncol(mata)
  
  na_mata <- is.na(mata)
  absent <- colSums(na_mata)
  ntot <- nrow_mata - absent
  sumband <- colSums(mata, na.rm = TRUE)
  frtot <- sumband / ntot
  zeta <- 2/(1 + fi)
  denom <- sqrt(fi^2 + 4 * (1 - fi) * (1 - frtot))
  h2 <- (denom + fi) / (denom + 2 - fi) * zeta
  
  relat <- matrix(ncol = nrow_mata, nrow = nrow_mata)
  
  for(i in seq_len(nrow_mata)) {
    for(j in seq(i, nrow_mata)) {
      num <- den <-0
      for(k in seq(2, ncol_mata, 2)) {
        
        cond <- na_mata[i,k] + na_mata[j,k]
        if(cond == 0) {
          h_top <- frtot[k] * (1 - frtot[k]) * h2[k]
          numi <- (mata[i,k] - frtot[k]) * (mata[j,k] - frtot[k]) + h_top / (ntot[k] - 1)
          deni <- h_top
          num <- num + numi 
          den <-den + deni
        }
      }
      relat[i,j] <- relat[j, i] <- num/den
    }
  }
  list(heritab = h2, relationship = relat, kinship = relat / zeta)
}
