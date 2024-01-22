#' Spatial weights
#' 
#' @description Spatial weights for individuals (nodes) with coordinates XY
#' @param XY Matrix/data frame with projected coordinates.
#' @param method Method of spatial weight matrix: "circle", "knearest", "inverse", 
#' "circle.inverse", "exponential", "circle.exponential".
#' @param W Custom weight matrix, with rownames and colnames identical to the XY data frame with coordinates
#' @param k Number of neighbors for nearest neighbor distance. When equidistant
#' neighbors are present, the program select them randomly.
#' @param d1 Minimum distance for circle matrices.
#' @param d2 Maximum distance for circle matrices.
#' @param p Power for inverse distance. Default = 1.
#' @param alpha Alpha value for exponential distance. Default = 1.
#' @param dist.method Method used for computing distances passed to  \code{\link[stats]{dist}}. Default = euclidean.
#' @param row.sd Logical. Should be row standardized the matrix? Default FALSE 
#' (binary weights).
#' @param max.sd Logical. Should be divided each weight by the maximum of the matrix?
#'  Default FALSE (binary weights).
#' @param self Should be the individuals self-included in circle or knearest
#' weights? Defalut FALSE.
#' @param latlon Are the coordinates in decimal degrees format? Defalut FALSE. If TRUE,
#' the coordinates must be in a matrix/data frame with the longitude in the first
#' column and latitude in the second. The position is projected onto a plane in
#' meters with the function \code{\link{geoXY}}.
#' @param ties ties handling method for "knearest" method: "unique" (default) for counting the ties 
#'  as an unique neighbor (i.e),
#'  "min" for counting all the ties in a given category but each is counted as a neighbor,
#'  "random" for choosing at random a neighbor,  
#'  "ring" for ring of neighbors, 
#'  "first" for sequential k values for each neighbor. 
#' @details This program computes a weights matrix (square matrix with individuals
#' in rows and columns, and weights wij in cells (i and j, individuals)) 
#' under the following available methodologies: 
#'   
#' - circle: all the connection between individuals i and j, included 
#' in a distance radius, higher than d1 and lower than d2, with center in 
#' the individual i, have a value of 1 for binary weights.
#' This distance requires the parameters d1 and d2 (default d1 = 0).
#'  
#' - knearest: the connections between an individual and its 
#' nearest neighbors of each individual i have a value of 1  for binary weights.
#' This distance requires the parameter k.
#' 
#' - inverse: inverse distance with exponent p (distance = 1/dij^p, with
#' dij the distance between individuals i and j).
#' This distance requires the parameter p (default p = 1).
#'   
#' - circle inverse: combination of "circle" and "inverse". 
#'  It is the matrix obtained by multiplying each element in a "circle" 
#'  binary matrix, and an "inverse" matrix.
#'  This distance requires the parameters p, d1 and d2 (default p = 1, d1 = 0).
#'  
#'  - exponential: inverse exponential distance with parameter alpha
#'   (distance = 1/e^(alpha *dij), with dij the distance between individuals i and j).
#'   This distance requires the parameter alpha (default alpha = 1).
#'  
#' - circle exponential: combination of "circle" and "exponential". 
#'  It is the matrix obtained by multiplying each element in a "circle" 
#'  binary matrix, and an "exponential" matrix.
#'  This distance requires the parameters alpha, d1 and d2 (default alpha = 1, d1 = 0).
#'  
#'  In addition to these methods, a spatial weight object can be created
#'  assigning a custom W matrix ("W" argument). In this case, the "method"
#'  is argument automatically set by the program to "custom" (see te example).
#'  
#'  ----------------------------------
#'  
#'  In row standardization, each weight wij for the individual i, is divided by the
#'  sum of the row weights (i.e., wij / sum(wij), where sum(wij) is computed over an
#'  individual i and all individuals j). 
#'  
#'  When self is TRUE, the connection j = i is also included.
#'  
#'  ----------------------------------
#'  
#'  PLOTS FOR ECO.WEIGHT OBJECTS:
#'  
#'  A plot method is availble (function "eco.plotWeight") showing static or interactive plots,
#'  In the case of using the function eco.plotWeight for the argument type="simple", 
#'  the connections are shown in two plots: an X-Y graph, 
#'  with the individuals as points, representing the original coordinates, and in a plot
#'  with coordinates transformed as ranks (i.e., each coordinate takes an ordered
#'  value from 1 to the number of individuals).
#'  The other static method (type="igraph") uses the igraph package to generate 
#'  a visual attractive graph (force network).
#'  Two interactive methods are available: type = "network", to plot an interactive force network,
#'  and type = "edgebundle" to plot a circular network.
#'  For the cases type = "inverse" or type = "exponential", the program generates a 
#'  plot of weights values vs distance 
#'  See the examples below.
#'  
#' @return An object of class eco.weight with the following slots:
#' @return > W weights matrix
#' @return > XY input coordinates
#' @return > METHOD weights construction method
#' @return > PAR parameters used for the construction of weights
#' @return > PAR.VAL values of the parameters used for the construction of weights
#' @return > ROW.SD row standardization (logical)
#' @return > SELF data self-included (logical)
#' @return > NONZERO percentage of non-zero connections
#' @return > NONZEROIND percentage of individuals
#' with non-zero connections 
#' @return > AVERAGE average number of connection per individual
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
#' data(eco3)
#' 
#' # 1)  "circle" method
#' 
#' con <- eco.weight(eco3[["XY"]], method = "circle", d1 = 0, d2 = 500)
#' 
#' 
#' #---- Different plot styles for the graph ----#
#' 
#' # simple
#' eco.plotWeight(con, type = "simple") 
#'             
#' # igraph
#' eco.plotWeight(con, type = "igraph", group = eco3[["S"]]$structure)
#' 
#' # network (interactive)
#' ## click in a node to see the label
#' eco.plotWeight(con, type = "network", bounded = TRUE, group = eco3[["S"]]$structure)
#' 
#' # edgebundle (interactive)
#' ## in the following plot, the assignment a group factor, 
#' ## generates clustered nodes.
#' ## hover over the nodes to see the individual connections
#' eco.plotWeight(con, type = "edgebundle", fontSize=8, group = eco3[["S"]]$structure)
#' 
#' 
#' # 2) "knearest" method
#'
#' con <- eco.weight(eco3[["XY"]], method = "knearest", k = 10)
#' eco.plotWeight(con) 
#' eco.plotWeight(con, type = "network", bounded = TRUE, group = eco3[["S"]]$structure)
#' 
#' # 3)  "inverse" method
#' ## scale dependent. In the example, the original coordinates (in km) are converted into m
#' con <- eco.weight(eco3[["XY"]]/1000, method = "inverse", max.sd = TRUE, p = 0.1)
#' con
#' eco.plotWeight(con)
#' 
#' # 4) "circle.inverse" method
#' con <- eco.weight(eco3[["XY"]], method = "circle.inverse", d2 = 1000)
#' con
#' eco.plotWeight(con)
#' 
#' # 5) "exponential" method
#' ## scale dependent. In the example, the original coordinates (in km) are converted into m
#' con <- eco.weight(eco3[["XY"]]/1000, method = "exponential", max.sd = TRUE, alpha = 0.1)
#' eco.plotWeight(con)
#' 
#' # 6) "circle.exponential" method
#' con <- eco.weight(eco3[["XY"]], method = "circle.exponential", d2 = 2000)
#' con
#' eco.plotWeight(con)
#' 
#' 
#' # 7) CUSTOM WEIGHT MATRIX
#' 
#' ## An eco.weight object can be created with a custom W matrix. In this case,
#' ## the rows and the columns of W (weight matrix) must have names, 
#' ## that must coincide (also in order) with the name of the XY (position) matrix.
#' 
#' require(igraph)
#' ## this example generates a network with the package igraph
#' tr <- make_tree(40, children = 3, mode = "undirected")
#' plot(tr, vertex.size = 10, vertex.label = NA) 
#'
#' ## conversion from igraph to weight matrix 
#' weights <- as.matrix(as_adj(tr))
#' 
#' ## weight matrix requires named rows and columns
#' myNames <- 1:nrow(weights)
#' rownames(weights) <- colnames(weights) <-  myNames
#'
#' ## extract coordinates from the igraph object 
#' coord <- layout.auto(tr)
#' rownames(coord) <- myNames
#' plot(layout.auto(tr))
#'
#' ## custom weight object
#' customw <- eco.weight(XY = coord, W = weights)
#' 
#' ## simple plot of the object
#' eco.plotWeight(customw, type = "simple")
#'
#' ## create a vector with groups to have coloured plots
#' myColors <- c(rep(1,13), rep(2, 9), rep(3, 9), rep(4, 9))
#' 
#' eco.plotWeight(customw, type = "igraph",group = myColors)
#' 
#' ## in the following plot, the argument bounded is set to FALSE, 
#' ## but if you have many groups, it probably should be set to TRUE.
#' # click in a node to see the label
#' eco.plotWeight(customw,type = "network", bounded = FALSE, group = myColors)
#' 
#' ## in the following plot, the assignment a group factor, 
#' # generates clustered nodes.
#' # hover over the name of the nodes to see the individual connections
#' eco.plotWeight(customw,  type = "edgebundle", group = myColors)
#' 
#' 
#' #### CONVERSION FROM LISTW OBJECTS #####
#' require(adegenet)
#' # Delaunay triangulation
#' temp <-chooseCN(eco3[["XY"]], type = 1, result.type = "listw", plot.nb = FALSE)
#' con <- eco.listw2ew(temp)
#' eco.plotWeight(con, "network", bounded = TRUE, group = eco3[["S"]]$structure)
#' 
#' 
#' #-----------------------
#' # ACCESSORS USE EXAMPLE
#' #-----------------------
#' 
#' # the slots are accessed with the generic format 
#' # (ecoslot. + name of the slot + name of the object). 
#' # See help("EcoGenetics accessors")
#' 
#' ecoslot.METHOD(con)        # slot METHOD
#' ecoslot.PAR(con)           # slot PAR
#' ecoslot.PAR.VAL(con)       # slot PAR.VAL
#' 
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export

setGeneric("eco.weight", function(XY,
                                  method = c("circle", "knearest", "inverse",  
                                             "circle.inverse", "exponential", 
                                             "circle.exponential"), W = NULL,
                                  d1 = 0, d2 = NULL,  k = NULL,  p = 1, alpha = 1, 
                                  dist.method = "euclidean",
                                  row.sd = FALSE, max.sd = FALSE,  
                                  self = FALSE, latlon = FALSE,
                                  ties = c("unique", "min", "random", "ring", "first")) {
  
  method <- match.arg(method)
  #dist.method <- match.arg(dist.method)
  ties <- match.arg(ties)

  #// custom matrix W configuration
  if(!is.null(W)) {
    if(!is.null(method)) {
      message("Custom W matrix detected. Method argument set as 'customW'\n")
      method <- "customW"
    }
    if(is.null(rownames(XY)) || is.null(rownames(W)) || is.null(colnames(W))) {
      stop("The use of a W custom matrix requires non null rownames and colnames, which must
           coincide between them and with the rownames of the XY matrix")
    } else {
      nombresXY <- rownames(XY)
      nombresRW <- rownames(W)
      nombresCR <- colnames(W)
      if(!all(nombresXY == nombresRW) || !all(nombresXY == nombresCR) || !all(nombresRW == nombresCR)) {
        stop("The use of a W custom matrix requires non null rownames and colnames, which must
           coincide with the rownames of the XY matrix")
      }
    }
    # check positive 
      
    
      if(!all(W >= 0)) {
        stop("The values of W must all be >= 0, some negative values were found")
      }
    # symmetric if x  = xt
      #if (!all(W == t(W))) {  
      #  stop("Non symmetric W matrix detected")
      #}
    y <- W
    param <- NULL
    param.values <- NULL
    
    
  } # end W configuration //
    
    # colnames and rownames must coincide with XY
    
  if(row.sd == TRUE && max.sd == TRUE) {
    stop("It must be selected one standardization argument of <row.sd> or <max.sd>, or none")
  }
    
    
  # // distance configuration
  if(latlon) {
    XY <- geoXY(XY[,2], XY[,1], unit=1)
    }
  
  distancia <- as.matrix(dist(XY, method = dist.method))

  #####
  
  if(method == "inverse") {
    y <- distancia
    y <- 1 / (y ^ p)
    diag(y) <- 0
    
    param <- "p"
    param.values <- p
    
  } else if(method == "circle") {
    
    if(is.null(d2)) {
      stop("A d2 argument must be given")
    }
    
    if(d1 > max(distancia ) || d2 < min(distancia) || d1 > d2) {
      stop("d1 > maximum distance between points, d2 < minimum distance between points or d1 > d2")
    }
    
    temp <- which((distancia <= d2) & (distancia > d1))
    y <- distancia
    y <- y - distancia
    y[temp] <- 1
    if(self) {
      diag(y) <- 1
    }
    
    param <- c("d1", "d2")
    param.values <- c(d1, d2)
    
  } else if(method == "circle.inverse") {
    if(is.null(d2)) {
      stop("The argument d2 is missing")
    }
    
    if(d1 > max(distancia ) || d2 < min(distancia) || d1 > d2) {
      stop("d1 > maximum distance between points, d2 < minimum distance between points or d1 > d2")
    }
    
    temp <- which((distancia <= d2) & (distancia >= d1))
    dummy <- distancia - distancia
    dummy[temp] <- 1
    y <- 1 / (distancia ^ p)
    y <- dummy * y
    diag(y) <- 0
    
    param <- c("d1", "d2", "p")
    param.values <- c(d1, d2, p)
    
  } else if(method == "exponential") {
    y <- 1 / exp(alpha * distancia)
    diag(y) <- 0
    param <- "alpha"
    param.values <- alpha
    
  } else if(method == "circle.exponential") {
    
    if(is.null(d2)) {
      stop("The argument d2 is missing")
    }
    
    if(d1 > max(distancia ) || d2 < min(distancia) || d1 > d2) {
      stop("d1 > maximum distance between points, d2 < minimum distance between points or d1 > d2")
    }
    
    
    temp <- which((distancia <= d2) & (distancia > d1))
    dummy <- distancia - distancia
    dummy[temp] <- 1
    y <- 1 / exp(alpha * distancia)
    y <- dummy * y
    diag(y) <- 0
    
    param <- c("d1", "d2", "alpha")
    param.values <- c(d1, d2, alpha)
    
  } else if(method == "knearest") {
  
 ties.method <- ties
 
 if(ties.method == "unique" || ties.method ==  "ring") {
   ties.method <- "min"
 }

    if(is.null(k)) {
      stop("The argument k is missing")
    }
    
 y <- t(apply(distancia, 1, 
              function(x) {
                rr <- rank(x[x != 0], ties.method = ties.method)
                if(ties == "unique" || ties ==  "ring") {
                  # create consecutive numbers for allowing multiple neighbors for a given k
                  rr <- as.numeric(as.factor(rr))
                }
                x[x != 0] <- rr
                
                return(x)
              }))
    
    if(ties!= "ring") {
    y <- t(apply(y, 1, function(x) as.numeric(x <= k & x != 0)))
    } else {
    # ring of neighbors
    y <- t(apply(y, 1, function(x) as.numeric(x == k & x != 0)))
    }
    
    if(self) {
      diag(y) <- 1
    }
    
    param <- "k"
    param.values <- k
    
  } 
  
  #row standardization
  if(row.sd) {
    y <- y/apply(y, 1, sum, na.rm = TRUE)
    y[is.na(y)] <- 0
  } 
  
  if(max.sd) {
    y <- y/max(y,na.rm = TRUE)
  }
  
  #output construction
  out <- new("eco.weight")
  
  if(method == "knearest") {
    method <- paste(method, "-", ties.method)
  }
  out@METHOD <- method
  out@ROW.SD <- row.sd
  if(method == "circle" | method =="knearest") {
    out@SELF <- self
  } else {
    out@SELF <- NA
  }
  out@W <- y
  out@XY <- data.frame(XY)
  
  y2 <- y
  diag(y2) <- 0
  out@CONNECTED <- which(apply(y, 1, sum, na.rm = TRUE) != 0)
  out@NONZERO <- round(100* sum(y2 != 0) / (nrow(y2)^2 - nrow(y2)), 1)
  out@NONZEROIND <- round(100 * sum(apply(y2, 1, sum) != 0) / nrow(y2), 1)
  out@AVG <- round(sum(apply(y2, 1, function(x)sum (x != 0)))  / nrow(y2), 1)
  out@PAR <- param
  out@PAR.VAL <- param.values
  avgdist <- y * distancia
  avgdist <- mean(avgdist[avgdist != 0])
  out@AVG.DIST <- round(avgdist, 3)
  out@ANGLE <- NULL
  
  out
  
})
