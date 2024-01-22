
#-------------------------------------------------------------------#
#' eco.plotLocal
#' @param x Result of eco.lsa analysis
#' @param interactivePlot Show an interactive plot via plotly? (default: TRUE)
#' @param multi for multivariable plot, use d3heatmap or ggplot2? (Default: d3heatmap).
#' In d3heatmap, NA values are set to 0.
#' @param significant Show all non significant points with a same colour?
#' @param alpha significance (alpha) for P (or P-adjusted) values (Default: 0.05)
#' @param rescaled rescale statistics between -1 and 1? (Default: FALSE)
#' @param limits When multiple variables are used, values used  as limits for computing the gradient for the plot
#' @param title title of the plot
#' @param z.name name of the variables axis in multivariable plot (using ggplot2 like plots)
#' @param grp groups for multivariable plot (using ggplot2 like plots)
#' @param vertical vertical multivariable plot? (Default: true)
#' @param legend Show legend?
#' @param n number of plot per screen in multivariable plots as list
#' @param nrow number of rows in multivariable plots as lists
#' @param byrow plot by row in multivariable plots by list
#' @param ... additional arguments passed to eco.rankplot, eco.forestplot, 
#' eco.resterplot o grf.seqmultiplot, depending of the selected plot
#' @description 
#' 
#' For examples see  \code{\link{eco.lsa}} 
#' 
#' SINGLE VARIABLES:
#' 
#' Using permutation test: The function calls eco.rasterplot, 
#' who generates a plot for a numeric or
#' factor variable.
#' The X and Y axes in the plot correspond 
#' to the rank of the X and Y coordinates, respectively. 
#' Additional parameters can be passed to eco.rankplot.
#' 
#' Using bootstrap test: The function calls eco.forestplot, 
#' who computes a forest plot 
#' for the confidence interval of each individual of the
#' input data (as row number)
#' and the corresponding observed value of the statistic.
#' Additional parameters can be passed to eco.forestplot.
#' 
#' 
#' MULTIPLE VARIABLES: 
#' multiple output format results. "list" for object with a list 
#' of individual test for each variable, or "matrix" for results as matrices
#' of multiples variables.
#' 
#' For results as matrices (option multi = "matrix" in eco.lsa): 
#' The function class eco.rasterplot, who generates a multivariate plot for 
#' a data matrix (raster). Additional parameters can be passed to eco.rasterplot.
#' The resterplot graph is a flexible tools for multiple data sources 
#' (environmental, genetic, phenotypic, etc.).
#' 
#' For results as list (option multi = "list" in eco.lsa):
#' The function generates plots for individual variables calling eco.rankplot.
#' Additional parameters can be passed to eco.rankplot.
#' 
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export

eco.plotLocal <- function(x,
                          interactivePlot = TRUE,
                          multi = c("ggplot", "d3heatmap"),
                          significant = TRUE,
                          alpha = 0.05,
                          rescaled = FALSE,
                          limits = NULL,
                          title = NULL,
                          z.name = NULL,
                          grp =  NULL,
                          vertical = TRUE,
                          legend = TRUE,
                          n = 4,
                          nrow = 2, 
                          byrow = TRUE,
                          ...) {
  
  multi <- match.arg(multi)
  if(inherits(x, "eco.lsa")) {
    if(x@TEST == "permutation" || x@NSIM == 0) {
      out <- eco.rankplot(x, rescaled = rescaled, 
                          interactivePlot = interactivePlot, ...)
    } else if(x@TEST == "bootstrap") {
      out <- eco.forestplot(x, rescaled = rescaled, 
                            interactivePlot = interactivePlot, ...)
    }
    
  } else if(inherits(x, "eco.multilsa")) {
  
   if(multi == "d3heatmap" && interactivePlot) {
   stop("Sorry, the use of d3heatmap has been deprecated")
     
   # if significant, set to 0 when no SA has been found
   if(significant) {
   modelmat <- x@PVAL < alpha
   mode(modelmat) <- "integer"
   mymat <- x@OBS.RES * modelmat
   } else {
   mymat <- x@OBS.RES 
   }
   out <- d3heatmap::d3heatmap(mymat)
   
   } else {
   out <- eco.rasterplot(x = x, 
                   grp = grp,
                   rescaled = rescaled,
                   limits = limits,
                   title = title,
                   z.name = z.name,
                   vertical = vertical,
                   significant = significant,
                   alpha = alpha,
                   interactivePlot = interactivePlot,
                   ...)
   }
    
    
  } else if(inherits(x, "eco.listlsa")) {
    
    
    if(legend) {
      all.plots <- suppressMessages(lapply(x, function(x) eco.rankplot(x, significant = significant, rescaled = rescaled,  interactivePlot = FALSE)))
    } else {
      all.plots <- suppressMessages(lapply(x, function(x) {
        u <- eco.rankplot(x, significant = significant, rescaled = rescaled, interactivePlot = FALSE)+ 
          ggplot2::theme(legend.position="none")
      })
      )
    }

    out <- grf.seqmultiplot(all.plots, n, nrow, byrow = byrow, ...)
    
  }
  
  out
}


