################################################################################
# AUXILIARY FUNCTIONS
################################################################################

#------------------------------------------------------------------------------#
# Union of classes
setClassUnion("any_vector",
              c("character", "numeric", "integer", "factor"))
setClassUnion("dataframeORmatrix",
							c("data.frame", "matrix"))
setClassUnion("matrixORnull",
              c("matrix", "NULL"))
setClassUnion("characterORnull",
							c("character", "NULL"))
setClassUnion("characterORlogical",
              c("character", "logical"))
setClassUnion("characterORmissing",
              c("character", "missing"))
setClassUnion("listORnull",
              c("list","NULL"))
setClassUnion("factorORnull",
              c("factor","NULL"))
setClassUnion("callORnull",
              c("call","NULL"))
setClassUnion("intORnumeric",
              c("integer","numeric","NULL"))
setClassUnion("intORmissing",
              c("integer","missing","NULL"))
setClassUnion("intORnull",
              c("integer","NULL"))
setClassUnion("numericORnull",
              c("numeric","NULL"))
setClassUnion("numericORmissing",
              c("numeric","missing"))
setClassUnion("logicalORmissing",
              c("logical","missing","NULL"))


#------------------------------------------------------------------------------#
#' Scaling a data frame or matrix to [0, 1] or [-1, 1] ranges
#' 
#' @param dfm Data frame, matrix or vector to scale.
#' @param method Scaling method: "zero.one" for scaling to [0, 1], "one-one" for scaling to [-1,1].
#' @description This program scales each column of a data frame or a matrix 
#' to [0,1] range, computing ((X)\emph{ij} - (Xmin)\emph{i}) / range((X)\emph{i}) 
#' for each individual \emph{j} of the variable \emph{i} or to [-1,1] range 
#' computing  2 *((X)\emph{ij} - (Xmin)\emph{i}) / range((X)\emph{i}) -1
#' @examples
#' 
#' \dontrun{
#' 
#' data(eco.test)
#' require(adegenet)
#' pc <- dudi.pca(eco[["P"]], scannf = FALSE, nf = 3)
#' pc <- pc$li
#' pc <- aue.rescale(pc)
#' plot(eco[["XY"]][, 1], eco[["XY"]][, 2], col = rgb(pc), pch = 16, 
#' cex = 1.5, xlab ="X", ylab= "Y")
#' 
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com} 
#' @export

aue.rescale  <- function(dfm, method =c("zero.one", "one.one")) {
  method <- match.arg(method)
  dfm <- as.data.frame(dfm)
  
  col <- apply(dfm, 2, function(X) { 
    if(method == "zero.one") {
    (X - min(X, na.rm = TRUE)) / diff(range(X, na.rm = TRUE))
    } else if(method == "one.one") {
    (2 * (X - min(X, na.rm = TRUE)) / diff(range(X, na.rm = TRUE))) - 1
    }
  })
  return(col)
}

#' Conversion from listw to ecoweight
#' @param X A listw object
#' @author Leandro Roser \email{learoser@@gmail.com} 
#' @export

eco.listw2ew <- function(X) {
  out <- int.check.con(X)
  xy <- attributes(X)$xy
  if(is.null(rownames(xy))) {
    message("coordinates without row names. The names were automatically set")
  rownames(xy) <- 1:nrow(xy)
    }
  rownames(out) <- colnames(out) <- rownames(xy)
  eco.weight(XY = xy, W = out)
}

#' Phenotypic similarity for vector, matrix or data frame acoording to Ritland (1996)
#' @param X Data frame, matrix or vector. If X is not a vector, the program
#' returns a list of matrices.
#' @author Leandro Roser \email{learoser@@gmail.com} 
#' @export

aue.phenosimil <- function(X) {
  
  ps<- function(y) {
    y.norm <- y - mean(y)
    y.norm <- outer(y.norm, y.norm, "*")
    y.norm <- y.norm / var(y)
    y.norm
  }
  
  if(is.matrix(X) || is.data.frame(X)) {
    out <- lapply(1:ncol(X), function(i) ps(X[, i]))
    names(out) <- colnames(X)
  } else if(is.vector(X)) {
    out <- ps(X)
  }
  out
}

#------------------------------------------------------------------------------#
#' Detection of metacharacters
#' @param X character string
#' @author Leandro Roser \email{learoser@@gmail.com} 
#' @keywords internal

is.meta <- function(X) {
  meta <- c("\\.", "\\\\", "\\|", "\\[", "\\]", "\\{", "\\}", 
            "\\(", "\\)", "\\^", "\\*", "\\?", "\\+", "\\$")
  any(meta %in% paste("\\", X, sep = ""))
}


#------------------------------------------------------------------------------#
#' Metachacter to character
#' @param X character string
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal

meta2char <- function(X) {
  if(!is.character(X)) {
  X <- deparse(substitute(X))
  }
  if(is.meta(X)) {
    X <- paste("\\", X, sep = "")
  }
  X
}


#------------------------------------------------------------------------------#
#' Remove spaces in a line of text
#' @param X character string
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal

aue.formatLine <- function(X) {
gsub("[[:space:]]+|[[:blank:]]+", " ", X)
}

#------------------------------------------------------------------------------#
#' Ordering the content of cells in a matrix. Ordering alleles in a genetic matrix.
#' @description This program takes a matrix and orders
#' the content of each cell. It was specially designed 
#' for genetic data, but can be used with any data 
#' that can be rearranged by the function \code{\link{order}}.
#' The arguments ploidy and ncode determine the mode of
#' ordering the data. 
#' The cells corresponding to each individual \emph{i} and 
#' loci \emph{j} are ordered in ascending order in default option
#' (it can be passed decreasing = TRUE as argument, if descending order is desired). 
#' For example, a locus with ploidy = 2 and ncod =1,  coded as 51, 
#' for an individual, will be recoded as 15. A locus with ploidy = 3
#' and coded as 143645453, will be recoded as 143453645 (alleles 143, 454 and 645).
#' 
#' 
#' @param X Any matrix with content to order.
#' @param ncod Number of digits coding each allele
#'  (e.g., 1: x, 2: xx, 3: xxx, etc.). If NULL, ncode will we 
#'  obtained from the ploidy and the maximum number of characters
#'  in the data cells.
#' @param ploidy Ploidy of the data.
#' @param sep.loc Character string separating alleles.
#' @param chk.plocod  Defalult TRUE. The function checks coherence 
#' in ploidy and number of digits coding alleles.
#' @param ... Additional arguments passed to \code{\link{order}}
#' @examples
#' 
#' \dontrun{
#' 
#' # Example 1----------------------
#' 
#' geno <- c(12, 52, 62, 45, 54, 21)
#' geno <- matrix(geno, 3, 2)
#' 
#' # ordering the data
#' aue.sort(geno, ploidy = 2)
#' 
#' # decreasing sort order
#' aue.sort(geno, ploidy = 2, decreasing = TRUE)
#' 
#' 
#' # Example 2----------------------
#' 
#' geno2 <- c(123456, 524556, 629359, 459459, 543950, 219405)
#' geno2 <- matrix(geno2, 3, 2)
#' 
#' # ordering the data as diploid
#' aue.sort(geno2, ploidy = 2)  # the data is ordered using blocks of 3 characters
#' 
#' # ordering the data as triploid
#' aue.sort(geno2, ploidy = 3)  # the data is ordered using blocks of 2 characters
#' 
#' # error: the ploidy and the number of characters are not congruent
#' aue.sort(geno2, ploidy = 5) 
#' 
#' # error: the ploidy and the number of characters are not congruent
#' aue.sort(geno2, ploidy = 5)
#' 
#' 
#' # Example 3----------------------
#' 
#' # any character data
#' generic <- c("aldk", "kdbf", "ndnd", "ndkd")
#' generic <- matrix(generic, 2, 2)
#' aue.sort(generic, ploidy = 2) 
#' aue.sort(generic, ploidy = 4)
#' 
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export

aue.sort <- function(X, ncod = NULL, ploidy = 2, 
                     sep.loc = "", chk.plocod = TRUE, ...)  {
  
  
  if(ploidy %% 1 != 0) {
    stop("ploidy argument must be non-fractional")
  }
    
  X <- as.matrix(X)
  nind <- nrow(X)
  nloc <- ncol(X)
  ploidy
  
  #if(check) {
  #X <- int.check.colnames(X)
  #X <- int.check.rownames(X)
  #}
  
  lista <- int.loc2listal(X, ncod = ncod, ploidy = ploidy,
                          sep.in = sep.loc, chk.plocod = chk.plocod,
                          chk.names = FALSE)
  
  # creating a list with individual locus 
  # and a list with ordered alleled by locus
 
  lista.orden <- lapply(lista, function(x) t(apply(x, 1, function(u) order(u, ...))))

  # ordering alleles
  for(i in 1:ncol(X)) {
    for(j in 1:nrow(X)) {
      ordlist <- lista.orden[[i]][j, ]
      lista[[i]][j, ] <- lista[[i]][j, ordlist]
    }
    lista[[i]] <- apply(lista[[i]], 1, function(x) paste(x, collapse = sep.loc))
  }
  
  # creating the output
  out <- do.call(cbind, lista)
  
  #replacing multiples "NA" (NANANA..) by NA
  out <- gsub("(NA)+", NA, out)
  
  rownames(out) <- rownames(X)
  colnames(out) <- colnames(X)
  
  out
  
  }


#------------------------------------------------------------------------------#
#' Identification of polymorphic loci
#' @param X allelic frequencies matrix
#' @param poly.level polymorphism threshold
#' @keywords internal

aue.is.poly <- function(X, poly.level) {
  
  if(poly.level < 0 || poly.level > 100) {
    stop("poly.level must be a number between 0 and 100")
  }
  
  temp <- (100 * apply(X, 2, mean, na.rm = TRUE)) 
  (temp >= poly.level) & (100 - temp >= poly.level) 
  
}
   

#------------------------------------------------------------------------------#
#' Remotion of non polymorphic loci
#' @param X allelic frequencies matrix
#' @param poly.level polymorphism threshold
#' @keywords internal

aue.rm.nonpoly <- function(X, poly.level) {
  
  isPoly <- aue.is.poly(X, poly.level)
  out <- X[,  isPoly,  drop = FALSE]
  out
}


#------------------------------------------------------------------------------#
#' Creation of a sequence of numbers in matrix or list format, for indexing.
#' @param from start of sequence
#' @param to end of sequence
#' @param by interval between elements
#' @param out.output format ("matrix2 or "list")
#' @author Leandro Roser \email{learoser@@gmail.com} 
#' @keywords internal

aue.seqlist <- function(from, to, by, out.format = c("matrix", "list")) {
  
  out.format <- match.arg(out.format)
  
  temp <- list()
  j <- 1
  k <- from
  for(i in seq(from = from + by - 1, to = to, by = by)) {
    temp[[j]] <- k:i
    j <- j+1
    k <- i+1
  }
  if(out.format == "matrix") {
    temp <- sapply(temp, function(x) x)
    if(is.null(nrow(temp))){
      temp <- matrix(temp, ncol = length(temp))
    }
  }
  
  temp
}


#------------------------------------------------------------------------------#
#' Remove spaces and tabs at the begining and the end of each element of charvec
#' @param charvec character vector
#' @keywords internal

aue.rmspaces <- function(charvec){
  charvec <- gsub("^([[:blank:]]*)([[:space:]]*)","",charvec)
  charvec <- gsub("([[:blank:]]*)([[:space:]]*)$","",charvec)
  return(charvec)
}


#------------------------------------------------------------------------------#
#' Generation of generic labels of constant length
#' @param base a character string
#' @param n number of labels
#' @keywords internal

aue.genlab <- function(base, n) {
  f1 <- function(cha, n){
    if(nchar(cha) < n){
      cha <- paste("0", cha, sep="")
      return(f1(cha, n))
    } else {return(cha)}
  }
  w <- as.character(1:n)
  max0 <- max(nchar(w))
  w <- sapply(w, function(cha) f1(cha, max0))
  return(paste(base, w, sep = ""))
}


#------------------------------------------------------------------------------#
#' Allelic frequencies 
#' @param eco Object of class ecogen.
#' @param grp Column in the slot S for summary by group.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal

aue.fqal <- function(eco, grp = NULL) {
  if(!is.null(grp)) {
    cual <- which(colnames(eco@S) == grp)
    grp.num <- as.numeric(levels(eco@S[,cual]))[eco@S[,cual]] 
    nfact <- max(grp.num)
  } else {
    dummy <- rep(1, nrow(eco@G))
    eco@S <- data.frame(dummy)
    cual <- which(colnames(eco@S) =="dummy")
    grp.num <- as.numeric(levels(as.factor(eco@S[,cual])))[eco@S[,cual]] 
    nfact <- 1
  }
  
  out <- list()
  for(i in 1:nfact) {
    eco2 <- eco[which(eco@S[,cual] == i)]
    clases<- as.numeric(eco2@INT@loc.fac)
    if(eco@INT@type == "codominant") {
    tabla <- eco2@A
    } else {
      tabla <- eco2@G
    }
    tabla <- 2 * tabla
    frecuencia <- apply(tabla, 2, sum)
    alelos.locus <- tapply(frecuencia, clases, sum)
    for( j in 1:length(clases)) {
      temp <- clases[j]
      clases[j] <- alelos.locus[temp]
    }
    frecuencia <- frecuencia / clases
  }
  
  round(frecuencia, 4)
}

#' EcoGenetics slot standard notation. Returns an accessor to the slot <ecoslot>
#' of the object <X>
#' @param ecoslot Slot of EcoGenetics object
#' @param X object name
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal

aue.access <- function(ecoslot, X) {
  
  if(inherits(X, "name")) {
    class(X) <- "character"
  }
  
  if(inherits(ecoslot, "name")) {
    class(ecoslot) <- "character"
  }
  
  if(!is.character(X)) {
    X <- deparse(substitute(X))
  }
  if(!is.character(ecoslot)) {
    ecoslot <- deparse(substitute(ecoslot))
  }
  paste("ecoslot.",ecoslot,"(",X,")", sep = "")
}

#' printaccess
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal
#' 
# slot access message

.printaccess <- function() {
  cat("--------------------------------------------------------------------------\n")
  cat(" Access to slots:",
      "<ecoslot.> + <name of the slot> + <(name of the object)>","\n",
      "See: help(\"EcoGenetics accessors\")")
}



################################################################################
## GRAPHICAL FUNCTONS-----------------------------------------------------------
################################################################################


### Graphical elements

#------------------------------------------------------------------------------#
#' Circle perimeter
#' @param mat Input raster matrix.
#' @param r Radius of the circle in pixels.
#' @param x0 Circle center- x position.
#' @param y0 Circle center- y position.
#' @param smooth Smoothing factor.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal

aue.circle <- function(mat, r, x0, y0, smooth = 100) { 
	
	for(theta in seq(0, 2*pi, pi/smooth)) {
		xd <- round(r*cos(theta)) 
		yd <- round(r*sin(theta)) 
		mat[x0 + xd, y0 + yd] <- 1
	}
  as.matrix(mat)
}


#------------------------------------------------------------------------------#
#' Solid circle
#' @param mat Input raster matrix.
#' @param r Radius of the circle in pixels.
#' @param x0 Circle center- x position.
#' @param y0 Circle center- y position. 
#' @author Leandro Roser \email{learoser@@gmail.com} 
#' @keywords internal

aue.point <- function(mat, r, x0, y0) { 
	xmat <- col(mat)
	ymat <- row(mat)
	mat2 <- mat - mat
	mat2[sqrt((xmat - x0)^2 + (ymat -y0)^2) <= r] <- 1
	as.matrix(mat2)
}

#------------------------------------------------------------------------------#
#' Solid ellipse
#' @param mat Input raster matrix.
#' @param r Ellipse expansion factor. Default = 1
#' @param a x radius (semimajor axis)
#' @param b y radius (seminimor axis)
#' @param x0 Ellipse center- x position.
#' @param y0 Ellipse center- y position. 
#' @param theta rotation angle in degrees.
#' @author Leandro Roser \email{learoser@@gmail.com} 
#' @keywords internal

aue.ellipse <- function(mat, a = 1, b = 1, x0, y0, theta = 0) { 
  
  x.mat <- col(mat)
  y.mat <- row(mat)
  #degrees to radians
  theta = theta * pi / 180
  #rotate axis
  x.rot <- x.mat*cos(theta) + y.mat*sin(theta)
  y.rot <- -x.mat*sin(theta) + y.mat*cos(theta)
  x0.rot <- x0*cos(theta) + y0*sin(theta)
  y0.rot <- -x0*sin(theta) + y0*cos(theta)
  #create image
  mat2 <- mat - mat
  mat2[sqrt((x.rot - x0.rot)^2/a^2 + (y.rot -y0.rot)^2/b^2) <= 1] <- 1
  as.matrix(mat2)
}


#------------------------------------------------------------------------------#
#' Solid square
#' @param mat Input raster matrix.
#' @param d Radius of the square in pixels.
#' @param x0 Square center in the x direction.
#' @param y0 Square center in the y direction.
#' @param theta rotation angle in degrees.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal
 
aue.square <- function(mat, d, x0, y0, theta = 0) {
	
	x.mat <- col(mat)
	y.mat <- row(mat)
	#degrees to radians
	theta = theta * pi / 180
	#rotate axis
	x.rot <- x.mat*cos(theta) + y.mat*sin(theta)
	y.rot <- -x.mat*sin(theta) + y.mat*cos(theta)
	x0.rot <- x0*cos(theta) + y0*sin(theta)
	y0.rot <- -x0*sin(theta) + y0*cos(theta)
	
	mat2 <- mat - mat
	mat2[pmax(abs(x.rot-x0.rot), abs(y.rot -y0.rot)) <= d] <- 1
	as.matrix(mat2)
}



#------------------------------------------------------------------------------#
#' Radial distance to a point.
#' @param mat Input raster matrix.
#' @param x0 Circle center- x position.
#' @param y0 Circle center- y position.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal

aue.circle.w <- function(mat, x0, y0) { 
  xmat <- col(mat)
  ymat <- row(mat)
  mat2 <- mat - mat
  mat2 <- sqrt((xmat - x0)^2 + (ymat -y0)^2)
  as.matrix(mat2)
}


#------------------------------------------------------------------------------#
#' Transforming a raster into a data frame with cartesian coordinates
#' 
#' @description This function returns a data frame with the column number (x),
#' row number (y) and cell value (z) of each pixel in a raster.
#' @param mat Input raster matrix.
#' @param origin Origin of the reference for the coordinates. Default: upperleft. 
#' @param out output format: "data.frame" (default) or "matrix".
#' @examples
#' 
#' \dontrun{
#' 
#' ras <- matrix(eco[["P"]][,1],15,15)
#' image(ras)
#' ras.row <- aue.image2df(ras)
#' ras.row
#' image(matrix(ras.row[,3], 15, 15))
#' 
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export

aue.image2df <- function(mat, origin = c("upperleft", "lowerleft"), out = c("data.frame", "matrix")) {
  
  if(!inherits(mat, "matrix")) {
  if(inherits(mat, "data.frame")) {
  mat <- as.matrix(mat)
  } else {
    stop("the input must be of class <matrix> or <data.frame>")
  }
  }
  origin <- match.arg(origin)
  out <- match.arg(out)
  xdim <- ncol(mat)
  ydim <- nrow(mat)
  
  if(origin == "lowerleft") {
  mat <- aue.rotate(mat, c(4,3,2,1))
  }
  
  mat.twocol <- expand.grid(1:ydim, 1:xdim)[2:1]
  
  mat.twocol <- cbind(mat.twocol, as.vector(mat))
  colnames(mat.twocol) <- c("x", "y", "z")
  # faster version. Leandro Roser, july 2015
  
  if(out == "data.frame") {
    mat.twocol <- data.frame(mat.twocol)
  }
  attr(mat.twocol, "origin") <- origin
  
  mat.twocol
  
}

#------------------------------------------------------------------------------#
#' Transforming a data frame into a raster
#' 
#' @description This is the inverse function of aue.image2df
#' @param x output of aue.image2df
#' @param origin Origin of the reference for the coordinates. Default: upperleft.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export

aue.df2image <- function(x, origin = c("upperleft", "lowerleft")) {
  
  out <- matrix(x[,3], ncol = max(x[,1]))
  
  if(!is.null(attr(x, "origin"))) {
    if(attr(x, "origin") == "lowerleft") {
      out <- aue.rotate(out, c(4,3,2,1))
    }
  }

  out
  
}


#------------------------------------------------------------------------------#
#' Transforming an adyacency matrix into a local weight matrix for a point with coordinates (x0,y0)
#' 
#' @param x input graph
#' @param x0 point row
#' @param y0 point column
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal

aue.ad2wg <- function(x, x0, y0) {
  
  temp <- aue.image2df((x))
  
  #find (x0,y0) position
  x <- x-x
  x[x0,y0] <- 1
  lugar <- which(x == 1)
  
  # create output
  nind <- nrow(temp)
  out <- matrix(0, nind, nind)
  out[lugar,] <- temp[,3]
  out
}

#------------------------------------------------------------------------------#
#' Rotation of a matrix
#' 
#' @description The program flips the matrix in the desired direction, given as a vector with four 
#' coordinates indicating the matrix corners (1 = upper left, 2= upper right, 3 = lower right, 4 = lower left).
#' The position of the input matrix is c(1,2,3,4). For example, a transposition is c(1,4,3,2).
#' @param x input matrix
#' @param direction vector with four coordinates indicating the position of the matrix angles. 
#' 1 = upper left, 2= upper right, 3 = lower right, 4 = lower left. 
#' 
#' @param y0 point column
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal
#' 

aue.rotate <- function(x, direction = c(1,2,3,4)) {
  direction <- deparse(substitute(direction))
  direction <- gsub(" ", replacement = "", direction)
  if(direction == "c(1,2,3,4)") {
    out <- x
  } else if(direction == "c(1,4,3,2)") {
    out <- t(x)
  } else if(direction == "c(4,3,2,1)") {
    out <- x[nrow(x):1,]
  } else if(direction == "c(2,1,4,3)") {
    out <- x[, ncol(x):1]
  } else if(direction == "c(3,4,1,2)") {
    out <- x[nrow(x):1,ncol(x):1]
  } else if(direction == "c(4,1,2,3)") {
    out <- t(x[nrow(x):1,])
  } else if(direction == "c(2,3,4,1)") {
    out <- t(x[, ncol(x):1])
  } else if(direction == "c(3,2,1,4)") {
    out <- t(x[nrow(x):1, ncol(x):1])
  } else if(direction == "c(3,2,1,4)") {
    out <- t(x[nrow(x):1, ncol(x):1])
  } else {
    stop("invalid rotation")
  }
  out
}


#' Weight matrices based on different geometries
#' 
#' @description Spatial weights for individuals with XY coordinates
#' @param XY Matrix/data frame with projected coordinates.
#' @param geometry Gemetry for spatial weight matrix: "circle", "ellipse", "square", 
#' "diamond"
#' @param a x radius (semimajor axis)
#' @param b y radius (seminimor axis)
#' @param d figure distance.



#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords internal

aue.geom.dist <- function(XY, geometry = c("ellipse", "square"),  d = 1,
                          a = 1, b = 1, row.sd = FALSE, theta = 0) {
  
  X <- XY[,1]
  Y <- XY[,2]
  
  #degrees to radians
  theta <- theta * pi / 180
  #rotate axis
  X.rot <- X*cos(theta) + Y*sin(theta)
  Y.rot <- -X*sin(theta) + Y*cos(theta)
  
  #create matrices#
  geometry <- match.arg(geometry)
  rowmat <- nrow(XY)
  x0 <- matrix(X.rot, rowmat, rowmat)
  y0 <- matrix(Y.rot, rowmat, rowmat)
  x <- t(x0)
  y <- t(y0)
  
  if(geometry == "ellipse") {
    Z <- sqrt((x - x0)^2/a^2  + (y -y0)^2/b^2) 
    Z[Z <= d] <- 1
    Z[Z > d] <- 0
    
  } else if(geometry == "square") {
    Z <- pmax(abs(x-x0), abs(y -y0)) 
    Z[Z <= d] <- 1
    Z[Z > d] <- 0
    
  }
  
  if(row.sd) {
    Zsum <- apply(Z, 1, sum)
    Z <- Z/Zsum
  }
  
  Z
}
 
#' eco.correlog  output to degrees list 
#' @param x eco.correlog object
#' @description convert a eco.correlog  output into a list for degrees 
#' @keywords internal

int.corvarToDeg <- function(x, angle) {
  outlist <- list()
  mynames <- colnames(x[[1]])
  mynames[1] <- "angle"
  rnames <- rownames(x[[1]])
  
  for(i in seq_len(nrow(x[[1]]))) {
    temp <- lapply(x, function(y) y[i, ])
    temp <- do.call("rbind", temp)
    outlist[[i]] <- as.data.frame(temp)
    outlist[[i]][, 1] <- angle
  }
  names(outlist) <- rnames
  
  outlist
}

#' Angles for an XY coordinates matrix
#' @param  XY XY matrix with projected coordinates
#' @param maxpi angles bounded between 0 - pi?
#' @param deg angles in decimal degrees?
#' @param latlon Are the coordinates in decimal degrees format? Defalut FALSE. If TRUE,
#' the coordinates must be in a matrix/data frame with the longitude in the first
#' column and latitude in the second. The position is projected onto a plane in
#' meters with the function \code{\link[SoDA]{geoXY}}.
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords export


aue.dataAngle <- function(XY, maxpi = FALSE, deg = FALSE, latlon = FALSE) {
  
  if(latlon == TRUE) {
    XY <- SoDA::geoXY(XY[,2], XY[,1], unit=1)
  } 
  
  X <- XY[, 1]
  Y <- XY[, 2]
  
  TWOPI <- 2 * pi
  
  # distance in X and Y axes
  x_dist <- outer(X, X, function(x, y) x - y)
  y_dist <- outer(Y, Y, function(x, y) x - y)
  
  angle <- atan2(y_dist, x_dist)
  
  # correction for negative angles and angles > 2 * pi
  angle[angle < 0]  <- angle[angle < 0] + TWOPI
  angle[angle > TWOPI]  <- angle[angle > TWOPI] - TWOPI
  
  #angles bounded from 0 to 180
  if(maxpi) {
  angle[angle > pi] <- angle[angle > pi] - pi
  }
  
  if(deg) {
  angle <- angle * 180 / pi
  }
  angle
}


#' Split categorical variable into levels,  using a second factor (hierarchy) to aggregate the data
#' @param  X factor
#' @param hier hierarchy
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords export


aue.split_categorical <- function(X, hier) {
  out <-  as.data.frame.matrix(table(hier, X[, 1, drop = TRUE]))
  colnames(out) <- paste(aue.rmspaces(colnames(X)), colnames(out), sep  = ".")
  out
}



#' Obtain the classes for each column of a data frame 
#' @param  X factor
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @keywords export

aue.check_class <- function(X) {
  data.frame(lapply(X, "class"))
}

#' Generate aggregated dataframe
#' @param  X data frame
#' @param  hier hierarchy 
#' @param  fun  function
#' @param factor_to_counts split factor into counts for each level?
#' @param ... additional parameters passed to fun 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export

aue.aggregated_df <- function(X, hier, fun, factor_to_counts = FALSE, ...) {
  if(nrow(X) == 0)
  {
    out <- data.frame(matrix(nrow = 0, ncol = 0))
    return(out)
  }
  if(factor_to_counts) {
  aggregator_function <- function(x, ...) {
    if(inherits(x[,1], "factor")) {
      return(aue.split_categorical(x[, 1, drop = FALSE], hier))
    } else {
      out <- data.frame(tapply(x[, 1, drop = TRUE], hier, function(y) fun(y, ...)), 
                        stringsAsFactors = FALSE)
      colnames(out) <- colnames(x)
      return(out)
    }
  }
} else {
  aggregator_function <- function(x, ...) {
    out <- data.frame(tapply(x[, 1, drop = TRUE], hier, function(y) fun(y, ...)), 
                      stringsAsFactors = FALSE)
    colnames(out) <- colnames(x)
    return(out)
  }
}
  temp <- list()
  for(i in 1:ncol(X)) {
    temp[[i]] <- aggregator_function(X[i])
  }

  do.call("cbind", temp)
}


#' Converion from dummy allele matrix to frequencies
#' @param df data frame with alleles coded in dummy format
#' @param loc_fac factor with the locus of each allele
#' @export

aue.dummy2af <- function(df, loc_fac) {
out <- apply(df, 1, function(x) tapply(x, loc_fac, function(y) y / sum(y, na.rm =  TRUE)))
out <- lapply(out, function(x) unlist(unname(x)))
do.call("rbind", out)
}

#' Detect operative system
#' @keywords internal

aue.get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}


#---------------------------------------------------------------------------------------------


#' EcoGenetic devel site
#' @description The function opens the EcoGenetics-devel web site:
#' https://github.com/leandroroser/EcoGenetics-devel
#' @export
ecogenetics_devel <- function(){
  cat("Opening link: https://leandroroser.github.io/EcoGenetics-devel \n")
  browseURL("https://github.com/leandroroser/EcoGenetics-devel")
}

#' EcoGenetic tutorial site
#' @description The function opens the EcoGenetics tutorial web site: 
#' https://leandroroser.github.io/EcoGenetics-Tutorial
#' @export
ecogenetics_tutorial <- function(){
  cat("Opening link: https://leandroroser.github.io/EcoGenetics-Tutorial \n")
  browseURL("https://leandroroser.github.io/EcoGenetics-Tutorial/")
}



