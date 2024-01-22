#' random test
#' 
#' @author Leandro Roser \email{leandroroser@@ege.fcen.uba.ar}
#' @keywords internal

# revision date: 2015/07/31. Leandro Roser

int.random.test <- function(repsim = NULL, obs, nsim, 
                            test = c("permutation", "bootstrap"), 
                            alternative = c("auto","two.sided", 
                                            "greater", "less"),
                            adjust = "none",
                            alpha = 0.05) {
  
  test <- match.arg(test)
  alternative <- match.arg(alternative)
  N <- length(obs)
  clase <- class(repsim)
  
  if (length(obs) > 1) {
    multi <- TRUE
    if (any(is.null(dim(repsim)))) {
      repsim <- NULL
    }
  } else {
    multi <- FALSE
    if(length(repsim) == 0) {
      repsim <- NULL
    }
  } 
  
  
  if(is.null(repsim)) {
    nsim <- 0
  }
  
  #-SINGLE TEST----------------------------------------------------------------#
  if(!multi) {
    ##-permutation case-------#
    if(test == "permutation") {
      
      #no simulations or NA observed value
      if (nsim == 0 || is.na(obs)) {
        
        results <- list(obs = obs, 
                        exp = NA, 
                        alter = NA, 
                        p.val =NA, 
                        CI =NA)
        return(results)
      }
      
      #else 
      
      expected <- median(repsim, na.rm = TRUE)
      if(alternative == "auto") {
        alter <- expected - obs
        if(alter > 0) {
          alternative <- "less"
        } else if (alter <= 0) {
          alternative <- "greater"
        }
      }
      
      if(alternative == "greater") {
        howmany <- sum(repsim >= obs, na.rm = TRUE)
        p.val <- (howmany + 1) / (nsim + 1)
        
      } else if(alternative == "less") {
        howmany <- sum(repsim <= obs, na.rm = TRUE)
        p.val <- (howmany + 1) / (nsim + 1)
        
      } else if(alternative == "two.sided") {
        howmany <- sum(abs(repsim) >= abs(obs), na.rm = TRUE)
        p.val <- (howmany + 1) / (nsim + 1)
      }
      if(alternative == "two.sided") {
      confint <- quantile(repsim, probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
      } else  {
      confint <- quantile(repsim, probs = c(alpha, 1-alpha), na.rm = TRUE)
      }
      
      result <- list(obs = obs, 
                     exp = expected, 
                     alter = alternative, 
                     p.val = p.val, 
                     CI = confint)
      
      ##-bootstrap case-------#
    } else if(test == "bootstrap") {
      
      # no simulations or NA observed value
      if(nsim == 0 || is.na(obs)) {
        result <- list(obs = obs, CI = NA)
        return(result)
      } else  {
        result <- quantile(repsim, probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
        result <- list(obs = obs, CI = result)
      }
    }
    
    #-MULTIPLE TESTS---------------------------------------------------------------#
    #variables in columns, repetitions in rows
  } else {
    
    p.val <- numeric()
    expected <- numeric()
    altern <- rep("", ncol(repsim))
    alternative.i <- alternative
    
    ##-permutation case-------#
    if(test == "permutation") {
      
      #no simulations or NA observed value
      if(nsim == 0) {
        
        tab <- data.frame(outer(obs, c(1:4)))
        tab[, 1] <- round(obs, 4)
        tab[, 2] <- NA
        tab[, 3] <- NA
        tab[, 4] <- NA
        tab[, 5:6] <- NA
        
        colnames(tab) <- c("obs", "exp", "alter", "p.val", "lwr", "uppr")
        return(tab)
      }
      
      
      for (i in 1:ncol(repsim)) {
        
        if(!is.na(obs[i])) {
          alternative <- alternative.i
          
          repet <- repsim[, i]
          expected[i] <- median(repet, na.rm = TRUE)
          
          if(is.na(expected[i])) {
            alternative <- "NA"
          }
          
          if(alternative == "auto") {
            alter <- expected[i] - obs[i]
            if(alter > 0) {
              alternative <- "less"
            } else if (alter <= 0) {
              alternative <- "greater"
            }
          }
          
          if(!is.na(alternative)) {
            altern[i] <- alternative
          } else {
            altern[i] <- NA
          }
          
          if(alternative == "greater") {
            howmany <- sum(repet >= obs[i], na.rm = TRUE)
            p.val[i] <- (howmany + 1) / (nsim + 1)
            
          } else if(alternative == "less") {
            howmany <- sum(repet <= obs[i], na.rm = TRUE)
            p.val[i] <- (howmany + 1) / (nsim + 1)
            
          } else if(alternative == "two.sided") {
            howmany <- sum(abs(repet) >= abs(obs[i]), na.rm = TRUE)
            p.val[i] <- (howmany + 1) / (nsim + 1)
            
          } else if(alternative == "NA") {
            p.val[i] <- NA
          }
          
        }  else {
          altern[i] <- NA
          p.val[i] <- NA
          expected[i] <- NA
        }
      }
      
      p.val <- p.adjust(p.val, method = adjust)
      
      if(!is.null(dim(repsim))) {
        if(alternative == "two.sided") {
        qq <- apply(repsim, 2,	quantile, probs = c(alpha/2, 1-alpha/2),  
                    na.rm = TRUE)
        } else  {
        qq <- apply(repsim, 2,	quantile, probs = c(alpha, 1-alpha),  
                      na.rm = TRUE)
        }
      } else {
        if(alternative == "two.sided") {
        qq <- quantile (repsim, probs = c(alpha/2, 1-alpha/2),  na.rm = TRUE)
        } else {
          qq <- quantile (repsim, probs = c(alpha/2, 1-alpha/2),  na.rm = TRUE)  
      }
      }
      
      tab <- data.frame(outer(obs, c(1:4)))
      tab[, 1] <- round(obs, 4)
      tab[, 2] <- round(expected, 4)
      tab[, 3] <- altern
      tab[, 4] <- round(p.val, 4)
      tab[, 5:6] <- round(t(qq), 4)
      
      colnames(tab) <- c("obs", "exp", "alter", "p.val", "lwr", "uppr")
      
      result <- tab
      
      ##-bootstrap case-------#
    } else if(test == "bootstrap") {
      
      #no simulations or NA observed value
      if(nsim == 0) {
        
        tab <- data.frame(outer(obs, c(1:3)))
        tab[, 1] <- round(obs, 4)
        tab[, 2:3] <- NA
        colnames(tab) <- c("obs", "lwr", "uppr")
        return(tab)
      }
      
      if(!is.null(dim(repsim))) {
        qq <- apply(repsim, 2,	quantile, probs = c(alpha/2, 1-alpha/2),  
                    na.rm = TRUE)
      } else {
        qq <- quantile (repsim, probs = c(alpha/2, 1-alpha/2),  na.rm = TRUE)
      }
      
      tab <- data.frame(outer(obs, c(1:3)))
      tab[, 1] <- round(obs, 4)
      tab[, 2:3] <- round(t(qq), 4)
      
      
      colnames(tab) <- c("obs", "lwr", "uppr")
      result <- tab
      
    }
  }
  result
  
}


