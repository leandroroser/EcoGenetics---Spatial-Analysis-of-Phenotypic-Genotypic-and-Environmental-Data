
#' eco.plotCorrelog
#' 
#' @description Plot method for correlograms and variograms. 
#' For examples, see  \code{\link{eco.correlog}} \code{\link{eco.cormantel}}  \code{\link{eco.variogram}}
#' @param x Result of correlogram or variogram analysis
#' @param var Individual variable to plot for multiple analyses with \code{\link{eco.correlog}} 
#' To plot multiple variables in a same plot, use only the argument x (see examples)
#' @param xlabel Label for X axis (default: NULL)
#' @param ylabel Label for Y axis (default: NULL)
#' @param title Title of the plot (default: NULL)
#' @param legend Show legends in ggplot graphs? (default: TRUE)
#' @param background Background color ("grey" or "white")
#' @param errorbar Show error-bars? (default: FALSE)
#' @param intervals Show bootstrap CI in kinship analysis? (default: TRUE)
#' @param significant.M With multiple variables: 
#' show only significant correlograms? (default: FALSE)
#' @param significant.S With single variables and permutation test: 
#' show different colours for significant points? (default: TRUE)
#' @param xlim X axis limits (as vector: c(min, max);  default: NULL)
#' @param ylim Y axis limits (as vector: c(min, max);  default: NULL)
#' @param interactivePlot Show an interactive plot via plotly? (default: TRUE)
#' @param nsim Number of simulations for permutation or bootstrap tests.
#' @param meanplot Show a line with the mean, when the plot is for multiple variables? (default: TRUE)
#' @param randtest Randomization test (one of: "permutation", "bootstrap", "none")
#' @param alpha significance level for P (or P-adjusted) values (Default alpha = 0.05)
#' @param quiet print quietly? Default FALSE
#' 
#' @seealso  \code{\link{eco.correlog}} \code{\link{eco.cormantel}}  \code{\link{eco.variogram}}
#'
#' @author Leandro Roser \email{learoser@@gmail.com}
#' 
#' @export


setGeneric("eco.plotCorrelog", 
           function(x, var = NULL, 
                              xlabel = NULL, 
                              ylabel = NULL, 
                              title = NULL, 
                              legend = TRUE,
                              background = c("grey", "white"),
                              errorbar = FALSE,
                              intervals = TRUE,
                              significant.S = TRUE,
                              significant.M = FALSE,
                              xlim = NULL,
                              ylim = NULL,
                              nsim = 999,
                              interactivePlot = TRUE,
                              meanplot = TRUE,
                              randtest = c("permutation", "bootstrap", "none"),
                              alpha = 0.05,
                              quiet = FALSE) {
  
  op <- options()
  options(max.print = 9999)
  
  if(x@BEARING == TRUE) {
    stop("Use for this object the function eco.plotCorrelogB")
  }
  
  # tricky solution for global binding problems during check. Set the 
  # variables as NULL
  d.mean <- obs <- uppr <- lwr <- max.sd <- min.sd <- NULL
  null.uppr <- null.lwr <- value <- variable <-  NULL
  d.mean5 <- null.lwr2 <- null.uppr2 <- d.mean2 <- obs2 <- NULL 
  d.mean3 <- obs3 <- d.mean4 <- CI.sup <- CI.inf <- obs <- ymin <- obs2 <- NULL
  
  
  if(length(x@OUT) == 1) {
    var2 <- 1
    plot.method <- "uniplot"
  } else if(is.null(var)) {
    plot.method <- "multiplot"
  } else {
    var2 <- which(names(x@OUT) %in% var)
    plot.method <- "uniplot"
  }
  
  randtest <- (x@TEST)[1]
  if(length(randtest) == 0) { 
    randtest <- "none"
  }
  
  if(x@METHOD[1] == "empirical variogram") {
    simObject <- 0
  } else {
    simObject <- x@NSIM
  }
  
  method <- (x@METHOD)[1]
  method <- gsub(" [(].*", "", method)
  method2 <- pmatch(method, c("Mantel test",
                              "Partial Mantel test", 
                              "Moran's I", 
                              "Geary's, C",
                              "bivariate Moran's Ixy",
                              "empirical variogram",
                              "Kinship"
  ))
  if(length(method2) == 0) {
    stop("invalid input to plot")
  }
  
  # set point size in interactive plots
  if(interactivePlot) {
  p.size <- 3
  axis.size <- 9
  title.size <- 13
  } else {
  p.size <- 4
  axis.size <- 10
  title.size <- 14
  }
  # THEME AND LABELS--------------------------------------#
  
  theme <- match.arg(background)
  
  if(theme == "grey") {
    themecol <-  ggplot2::theme_grey()
  } else {
    themecol <- ggplot2::theme_bw()
  }
  
  if(is.null(xlabel)) {
    xlabel <- "Great circle distance"
  } 
  
  if(is.null(ylabel)) {
    ylabel <- method
  }
  
  # legend
  if(legend){
   leyenda <- ggplot2::theme(legend.position = "right")
  } else {
   leyenda <- ggplot2::theme(legend.position = "none")
  }

  
  # UNIPLOT--------------------------------------#
  
  if(plot.method == "uniplot") {
    
    datos <- as.data.frame(x@OUT[[var2]])
    
    # for ggplotly labels:
    if(method == "Kinship") {
      colnames(datos)[c(9,10)]<-  c("CI.inf", "CI.sup")
    }
    
    
    ########## title,  x and y labels ############
    
    if(is.null(title)) {
      title <- names(x@OUT[var2])
    }
    
    
    localenv <- environment()
    
    
    ########## basic plot ############
    
    #trick to solve labels in ggplotly:
    datos$d.mean2 <- datos$d.mean3 <- datos$d.mean4 <- datos$d.mean5 <-  datos$d.mean
    datos$obs2 <- datos$obs3 <-  datos$obs
    if(method == "Kinship") {
      datos$null.uppr2 <- datos$null.uppr
      datos$null.lwr2 <- datos$null.lwr
    }
    
    #ggplotly requires aes within ggplot(...) to plot errorbars
    z <- ggplot2::ggplot(datos, ggplot2::aes(x = d.mean, y = obs), environment = localenv)
    
    
    # THIS IS HERE TO be in the last layer
    if(method == "Kinship" & intervals & simObject != 0) {
      
      z <- z +  ggplot2::geom_path(ggplot2::aes(x = d.mean5, y = null.uppr), 
                                   linetype = 2, 
                                   colour = "red") +  
        ggplot2::geom_path(ggplot2::aes(x = d.mean5, y = null.lwr),
                           linetype = 2, 
                           colour = "red") + 
        ggplot2::geom_ribbon(ggplot2::aes(x = d.mean5, ymin = null.lwr2, 
                                          ymax = null.uppr2), 
                             fill = 90,
                             alpha = 0.05) 
    }
    
    
    z <- z + ggplot2::geom_line(ggplot2::aes(x = d.mean2, y = obs2)) + 
      ggplot2::xlab(xlabel) + 
      ggplot2::ylab(ylabel) + 
      ggplot2::labs(title = title) 
    
    
    ######## background ########
    
    z <- z + themecol
    
    ######## permutation and bootstrap cases ########
    
    #permutationc case
    if(randtest == "permutation") {
      #labeling S and NS points
      if(significant.S && simObject != 0) {
      pval2 <- as.numeric(datos$p.val < alpha)
      if(sum(pval2) == 0) {
        labelp <- "NS"
      } else if (sum(pval2) == length(pval2)) {
        labelp <- paste0("P < ", alpha)
      } else {
        labelp <- c(paste0("P < ", alpha), "NS")
      }
      pval2[pval2 == 0] <- "#F8766D"
      pval2[pval2 == 1] <- "#00B0F6"
      datos$pval2 <- pval2
      } else {
        pval2 <- rep("#F8766D", nrow(datos))
        labelp <- ""
        leyenda <-  ggplot2::theme(legend.position = "none")
      }
      
      puntos <- ggplot2::geom_point(ggplot2::aes(x = d.mean3, y = obs3, colour = pval2), 
                                    size = p.size)
      z <- z + puntos
   
     
    }
    
    # bootstrap case
    if(randtest == "bootstrap")  {
      
      
      
      puntos <- ggplot2::geom_point(ggplot2::aes(x = d.mean3, y = obs3, colour = "blue"), 
                                    size = p.size)
      
      if(simObject != 0){
      intervalo <- ggplot2::geom_ribbon(ggplot2::aes(x = d.mean, ymax = uppr, 
                                                     ymin = lwr), 
                                        fill = "blue",
                                        alpha = 0.2)
      z <- z + puntos + intervalo
      } else {
      z <- z + puntos
      }
      
      leyenda <- ggplot2::theme(legend.position = "none")
    }
    
    if(randtest == "none" & method == "empirical variogram") {
      
      
      z <- z + ggplot2::ylab("Semivariance") + 
        ggplot2::geom_point(ggplot2::aes(x = d.mean3,y = obs3), 
                            colour = "#F8766D", size = p.size)
      
      leyenda <- ggplot2::theme(legend.position = "none")
    }
    
    ########## error bar ############################################
    
    if(errorbar) {
      
      if(method != "Kinship") {
        #approx. SD ~1.96*SD-JACK
        
        datos$sd <- 1.96 * datos$sd.jack
        datos$CI.inf <- datos$mean.jack - datos$sd
        datos$CI.sup <- datos$mean.jack + datos$sd
      }
        
        z <- z + ggplot2::geom_errorbar(ggplot2::aes(x = d.mean4, 
                                                     ymax = CI.sup, 
                                                     ymin= CI.inf,
                                                     linetype = "solid"))
      
        
      }
    
    
    ####### theme configuration (permutation case) ##########################
    # face = "bold" in element_text to make text bold
    z <- z +
        ggplot2::theme(axis.text = ggplot2::element_text(size = axis.size), 
                       axis.title = ggplot2::element_text(size = title.size), 
                       plot.title = ggplot2::element_text(size = title.size, hjust=0.5))
   
    if(randtest == "permutation") {
    z <-  z + ggplot2::scale_colour_discrete(name  ="P value", labels= labelp) 
    }
    
    z <- z + leyenda
    ########## xlim, ylim ############
    
    if(!is.null(xlim)) {
      z <- z + xlim
    }
    if(!is.null(ylim)) {
      z <- z + ylim
    }
    
    if(interactivePlot) {
      # plot with ggplotly. show distance and observed value
      z <- suppressMessages(plotly::ggplotly(z, tooltip = c("d.mean", "CI.inf", "obs", 
                                           "CI.sup", "null.lwr", "null.uppr")))
      
      # this corrects the labels in ggplotly
      for(i in 1:length(z$x$data)) {
        if(z$x$data[[i]]$name =="#00B0F6") {
          z$x$data[[i]]$name <- paste0("P < ", alpha)
        } else if(z$x$data[[i]]$name =="#F8766D") {
          z$x$data[[i]]$name <- "NS"
        }
      }
      
    }
    suppressMessages(print(z))
    ######################
    
  # multiplot case
    
  } else if(plot.method == "multiplot") {
    
    if(is.null(title)) {
      title <- ""
    }
    
    # in case of no simulations, set significant.M == FALSE
    if(simObject == 0) {
      significant.M  <- FALSE
    }
      
    man <- int.multiplot(x, significant = significant.M, 
                         plotit = FALSE, nsim = nsim)

      
      # tricky solution for global binding problems during check. Set the 
      # variables as NULL
     
      datos <- data.frame(man$mean.correlogram, man$correlogram.alleles)
      intervalos <- man$mean.correlogram
      colnames(datos)[1] <- "mean"
      
     
      localenv <- environment()
      
      #PLOT FOR VARIABLES
      
      test.data <- reshape2::melt(datos[, -c(2:4)], id ="mean")
      
      # this trick is to solve label duplications in the tooltip of the plot
      intervalos <- man$mean.correlogram
      intervalos$d.mean2 <- intervalos$d.mean
      intervalos$d.mean3 <- intervalos$d.mean
      intervalos$obs2 <- intervalos$obs
      
      if(meanplot) {
        
        multi.correlog <- ggplot2::ggplot(data = intervalos, ggplot2::aes(x = d.mean, y=obs), environment = localenv)+
          themecol+
          ggplot2::theme(axis.text = ggplot2::element_text(size = axis.size), 
                         axis.title = ggplot2::element_text(size = title.size, hjust=0.5))+
          ggplot2::geom_line(data=test.data, ggplot2::aes(x = mean, y = value, colour = variable), linewidth = 1) + 
          ggplot2::geom_point(ggplot2::aes(x = d.mean2, y = obs2), size = p.size) +
          ggplot2::geom_line(ggplot2::aes(x = d.mean, y = obs), linewidth= 1.8) + 
          ggplot2::geom_errorbar(ggplot2::aes(x = d.mean3, ymax = uppr, ymin= lwr), linewidth=1) + 
          leyenda +
          ggplot2::xlab(xlabel) + 
          ggplot2::ylab(ylabel) + 
          ggplot2::labs(title = title) 
        
        #multi.correlog <-  multi.correlog + 
        #  ggplot2::geom_line(data = intervalos, ggplot2::aes(x = d.mean, y = obs), size= 1.8)+
        #  ggplot2::geom_point(data = intervalos, ggplot2::aes(x = d.mean, y = obs), size = 3)+
        #  ggplot2::geom_errorbar(data = intervalos, ggplot2::aes(x = d.mean, ymax = uppr, ymin= lwr), size=1)
        
        #ggplot2::geom_point(data = intervalos, ggplot2::aes(x = d.mean, y = obs), size = 5)+
        #ggplot2::geom_errorbar(data = intervalos, ggplot2::aes(x = d.mean, ymax = uppr, ymin= lwr), size=1)+
        #ggplot2::geom_line(data = intervalos, ggplot2::aes(x = d.mean, y = obs), size= 1.8) 
        
        if(interactivePlot) {
          multi.correlog <- suppressMessages(plotly::ggplotly((multi.correlog + ggplot2::theme(legend.position = "none")), 
                                                              tooltip = c("d.mean", "variable", "value", "obs", "lwr", "uppr")))
        }
        
      } else  {
        # i am doing this to get the right plot with plotly (there are problems to overlap two plots)
        colnames(test.data)[1] <- "d.mean"
        multi.correlog <- ggplot2::ggplot(data=test.data, environment = localenv)+
          ggplot2::geom_line(ggplot2::aes(x = d.mean, y = value, colour = variable), linewidth = 1)+
          themecol+
          ggplot2::theme(axis.text = ggplot2::element_text(size = axis.size), 
                         axis.title = ggplot2::element_text(size = title.size, hjust=0.5)) + leyenda + 
          ggplot2::ylab(ylabel)+
          ggplot2::xlab(xlabel)+
          ggplot2::labs(title = title)
    
        
        if(interactivePlot) {
          multi.correlog <- suppressMessages(plotly::ggplotly((multi.correlog + ggplot2::theme(legend.position = "none")), 
                                                              tooltip = c("d.mean", "variable", "value")))
        }
      }
      
      
      mean.correlog <- ggplot2::ggplot(data = intervalos, ggplot2::aes(x = d.mean, y=obs))+
        ggplot2::geom_line(ggplot2::aes(x = d.mean, y = obs), linewidth= 1.8, colour="#F8766D")+
        ggplot2::geom_point(size = p.size)+
        ggplot2::geom_errorbar(ggplot2::aes(ymax = uppr, ymin= lwr), linewidth=2) +
        ggplot2::ylab(ylabel)+
        ggplot2::xlab(xlabel)
        
      
      if(interactivePlot) {
        mean.correlog <- suppressMessages(plotly::ggplotly(mean.correlog,
                                          tooltip = c("d.mean", "obs", "lwr", "uppr")))
      }
      
   
    z <- list(mean.correlog = mean.correlog, multi.correlog = multi.correlog)
    

    lapply(z, print)
  
    z$data <- man
    if(!quiet) {
    print(z$data)
    }
    
  }
  options(op)
  suppressMessages(invisible(z))
})



#-------------------------------------------------------------------#
#' int.multiplot method. Graphical processing of multiple correlograms 
#' @rdname int.multiplot
#' @keywords internal

int.multiplot<- function(correlog, 
                         significant = TRUE,
                         ...) {
  
  
  # tricky solution for global binding problems during check. Set the 
  # variables as NULL
  d.mean <- obs <- uppr <- lwr <- max.sd <- min.sd <- null.uppr <- null.lwr <- value <- variable <-  NULL
  
  data <-correlog@IN$Z
  N <- length(correlog@OUT)
  var.names <- colnames(correlog@IN$Z)
  
  if(significant) {
    sign <- numeric()
    j <- 1
    for(i in 1:N) {
      if(any(correlog@OUT[[i]][, 3] < alpha)) {
        sign[j] <- i
        j <- j + 1
      }
    }
    
    if(length(sign) == 0) {
      stop("no significant correlograms")
    }
  } else {
    sign <- 1:N
  }
  
  # manhattan matrix ------------------------------------
  manhattan.correlog <- table(sign,sign)
  secuencia <- matrix(sign, length(sign),length(sign), byrow = TRUE)
  #colnames(manhattan.correlog) <- rownames(manhattan.correlog) <- colnames(data)[sign]
  
  #manhatann matrix construction
  l.sign <- 1:length(sign)
  for(i in l.sign) {
    for(j in l.sign) {
      mat <- mean(abs(correlog@OUT[[sign[i]]][,2]-
                        correlog@OUT[[sign[j]]][,2]))
      manhattan.correlog[i, j] <- mat
    }
  }
  #force class manhattan.correlog = "matrix"
  class(manhattan.correlog) <- NULL
  dimnames(manhattan.correlog) <- list(colnames(data)[sign], colnames(data)[sign])
  #-----------------------------------------------
  
  #mean correlogram--------------------------------------
  mean.correlog <- matrix(0, ncol = length(sign), nrow = length(correlog@CARDINAL))
  l.sign <- 1:length(sign)
  for(j in l.sign) {
    mat <- correlog@OUT[[sign[j]]][,2]
    mean.correlog[, j] <- mat
  }
  colnames(mean.correlog) <- colnames(data)[sign]
  rownames(mean.correlog) <- rownames(correlog@OUT[[1]])
  mean.correlog <- as.data.frame(mean.correlog)
  
  intervalos <- matrix(0, nrow = length(correlog@CARDINAL), ncol = 2)
  
  #ci for mean correlogram
  intervalos <- int.jackknife(t(mean.correlog), mean)
  intervalos <- data.frame(cbind(correlog@OUT[[1]][,1],
                                 intervalos$obs, t(intervalos$CI)))
  colnames(intervalos) <- c("d.mean", "obs","lwr", "uppr")
  rownames(intervalos) <- rownames(correlog@OUT[[1]])
  intervalos <- as.data.frame(intervalos)
  
  
  salida <- list(correlogram.alleles = mean.correlog,
                 manhattan.correlog = manhattan.correlog,
                 mean.correlogram = intervalos,
                 method = correlog@METHOD,
                 significant.var.names = colnames(data)[sign],
                 significant.var.number = sign)
  
  class(salida) <- "int.multiplot"
  
  invisible(salida)
  
}


#' eco.plotCorrelogB
#' 
#' @description Plot method for bearing correlograms
#' For examples, see  \code{\link{eco.correlog}}. It constructs an angular correlogram
#' for each distance class taken as fixed.
#' @param x Result of correlogram  analysis, with output using angles as independent 
#' variables for fixed distances (instead of distances as independent variables)
#' @param var Individual variable to plot; var is a number between 1 and the number of 
#' distance classes indicating the corresponding class (for example, with 5 distance classes,
#' the number 3 indicates the third)
#' To plot multiple variables in a same plot, use only the argument x (see examples)
#' @param xlabel Label for X axis (default: NULL)
#' @param ylabel Label for Y axis (default: NULL)
#' @param title Title of the plot (default: NULL)
#' @param legend Show legends in ggplot graphs? (default: TRUE)
#' @param background Background color ("grey" or "white")
#' @param significant.S With single variables and permutation test: 
#' show different colours for significant points? (default: TRUE)
#' @param xlim X axis limits (as vector: c(min, max);  default: NULL)
#' @param ylim Y axis limits (as vector: c(min, max);  default: NULL)
#' @param interactivePlot Show an interactive plot via plotly? (default: TRUE)
#' @param alpha significance level for P (or P-adjusted) values (Default alpha = 0.05)
#' @seealso  \code{\link{eco.correlog}} 
#'
#' @author Leandro Roser \email{learoser@@gmail.com}
#' 
#' @export 

setGeneric("eco.plotCorrelogB", 
          function(x, var = NULL, 
                   xlabel = NULL, 
                   ylabel = NULL, 
                   title = NULL, 
                   legend = TRUE,
                   background = c("grey", "white"),
                   significant.S = TRUE,
                   xlim = NULL,
                   ylim = NULL,
                   interactivePlot = TRUE,
                   alpha = 0.05) {
            
            op <- options()
            options(max.print = 9999)
            
            obs <- obs2 <- obs3 <- angle <- angle2 <- angle3 <- NULL
            
            if(length(x@OUT) == 1) {
              var2 <- 1
              plot.method <- "uniplot"
            } else {
              if(is.null(var)) {
                plot.method <- "multiplot"
              } else {
                if(!is.numeric(var)){
                  stop("var must be a number between 1 and number of classes")
                }
                var2 <- var
                plot.method <- "uniplot"
              }
            }
            
            randtest <- (x@TEST)[1]
            if(length(randtest) == 0) { 
              randtest <- "none"
            }
            
            simObject <- x@NSIM
            
            
            method <- (x@METHOD)[1]
            method <- gsub(" [(].*", "", method)
            
            
            # set point size in interactive plots
            if(interactivePlot) {
              p.size <- 3
              axis.size <- 9
              title.size <- 13
            } else {
              p.size <- 4
              axis.size <- 10
              title.size <- 14
            }
            # THEME AND LABELS--------------------------------------#
            
            theme <- match.arg(background)
            
            if(theme == "grey") {
              themecol <-  ggplot2::theme_grey()
            } else {
              themecol <- ggplot2::theme_bw()
            }
            
            if(is.null(xlabel)) {
              xlabel <- "Degrees N of Due E"
            } 
            
            if(is.null(ylabel)) {
              ylabel <- method
            }
            
            # legend
            if(legend){
              leyenda <- ggplot2::theme(legend.position = "right")
            } else {
              leyenda <- ggplot2::theme(legend.position = "none")
            }
            
            
            # UNIPLOT--------------------------------------#
            
            if(plot.method == "uniplot") {
              
              datos <- as.data.frame(x@OUT[[var2]])
              datos <- data.frame(distance = rep(names(x@OUT[var2]), nrow(datos)), datos)
              colnames(datos)[2] <- "angle"
              
              ########## title,  x and y labels ############
              
              if(is.null(title)) {
                title <- names(x@OUT[var2])
              }
              
              localenv <- environment()
              
              
              ########## basic plot ############
              
              #trick to solve labels in ggplotly:
              datos$angle5 <- datos$angle4 <- datos$angle3 <- datos$angle2 <-  datos$angle
              datos$obs2 <- datos$obs3 <-  datos$obs
              
              #ggplotly requires aes within ggplot(...) to plot errorbars
              
              z <- ggplot2::ggplot(datos, ggplot2::aes(x = angle, y = obs), environment = localenv) + 
                ggplot2::geom_line(ggplot2::aes(x = angle2, y = obs2)) + 
                ggplot2::xlab(xlabel) + 
                ggplot2::ylab(ylabel) + 
                ggplot2::labs(title = title)  +
                themecol
              
                #labeling S and NS points
                if(significant.S && simObject != 0) {
                  pval2 <- as.numeric(datos$p.val < alpha)
                  if(sum(pval2) == 0) {
                    labelp <- "NS"
                  } else if (sum(pval2) == length(pval2)) {
                    labelp <- paste0("P < ", alpha)
                  } else {
                    labelp <- c(paste0("P < ", alpha), "NS")
                  }
                  pval2[pval2 == 0] <- "#F8766D"
                  pval2[pval2 == 1] <- "#00B0F6"
                  datos$pval2 <- pval2
                } else {
                  pval2 <- rep("#F8766D", nrow(datos))
                  labelp <- ""
                  leyenda <-  ggplot2::theme(legend.position = "none")
                }
                
                puntos <- ggplot2::geom_point(ggplot2::aes(x = angle3, y = obs3, colour = pval2), 
                                              size = p.size)
                z <- z + puntos
                
   

              ####### theme configuration (permutation case) ##########################
              # face = "bold" in element_text to make text bold
              z <- z + ggplot2::theme(axis.text = ggplot2::element_text(size = axis.size), 
                                      axis.title = ggplot2::element_text(size = title.size), 
                                      plot.title = ggplot2::element_text(size = title.size, hjust=0.5))
              
              if(randtest == "permutation") {
                z <-  z + ggplot2::scale_colour_discrete(name  ="P value", labels= labelp) 
              }
              
              z <- z + leyenda
              ########## xlim, ylim ############
              
              if(!is.null(xlim)) {
                z <- z + xlim
              }
              if(!is.null(ylim)) {
                z <- z + ylim
              }
              
              if(interactivePlot) {
                # plot with ggplotly. show distance and observed value
                z <- suppressMessages(plotly::ggplotly(z, tooltip = c("angle", "obs")))
                
                # this corrects the labels in ggplotly
                for(i in 1:length(z$x$data)) {
                  if(z$x$data[[i]]$name =="#00B0F6") {
                    z$x$data[[i]]$name <- paste0("P < ", alpha)
                  } else if(z$x$data[[i]]$name =="#F8766D") {
                    z$x$data[[i]]$name <- "NS"
                  }
                }
                
              }
              
              ######################
              
              # multiplot case
              
            } else if(plot.method == "multiplot") {
              
              if(is.null(title)) {
                title <- "Bearing Correlogram"
              }
              
              # tricky solution for global binding problems during check. Set the 
              # variables as NULL
              
              datos <- do.call("cbind", lapply(x@OUT, function(y) y$obs))
              datos <- cbind(x@OUT[[1]][, 1], datos)
              colnames(datos)[1] <- "angle"
              
              localenv <- environment()
              
              #PLOT FOR VARIABLES
              rownames(datos) <- datos[, 1]
              test.data <- reshape2::melt(datos[, -1])
              colnames(test.data) <- c("angle", "dist", "obs")
              # this trick is to solve label duplications in the tooltip of the plot
              
              # i am doing this to get the right plot with plotly (there are problems to overlap two plots)
              z <- ggplot2::ggplot(data=test.data, environment = localenv)+
                ggplot2::geom_line(ggplot2::aes(x = angle, y = obs, colour = dist), linewidth = 1)+
                themecol+
                ggplot2::theme(axis.text = ggplot2::element_text(size = axis.size), 
                               axis.title = ggplot2::element_text(size = title.size, hjust=0.5)) + leyenda + 
                ggplot2::ylab(ylabel)+
                ggplot2::xlab(xlabel)+
                ggplot2::labs(title = title)
              
              
              if(interactivePlot && !legend) {
                z <- suppressMessages(plotly::ggplotly((z + ggplot2::theme(legend.position = "none")),
                                      tooltip = c("angle", "dist", "obs")))
                z[[1]]$layout$title <- title
                } else if(interactivePlot && legend) {
                z <- suppressMessages(plotly::ggplotly(z, tooltip = c("angle", "dist", "obs")))
                z[[1]]$layout$title <- title
              }
              
            }
            suppressMessages(z)
          })
