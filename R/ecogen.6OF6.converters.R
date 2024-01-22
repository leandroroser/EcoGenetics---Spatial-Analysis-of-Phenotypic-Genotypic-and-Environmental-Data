
#' Creating input data for Geneland with an ecogen object
#' 
#' @description This function creates four data frames (XY.txt, NAMES.txt, P.txt, G.txt) 
#' in the indicated directory (default: working directory), which can be loadedin Geneland.
#' @param eco Object of class "ecogen"
#' @param dir output path. Default = "" (current directory).
#' @param ncod Number of digits coding each allele
#'  (e.g., 1: x, 2: xx, 3: xxx, etc.).
#' @param ploidy Ploidy of the data.
#' @param to_numeric Recode the genetic data into numeric format? If TRUE, 
#' the functions performs the correction via \code{\link{eco.format}}.
#' Additional formatting parameters can be passed to this function.
#' @param recode Recode mode when to_numeric = TRUE: "all" for recoding
#' the data considering all the individuals values at once (e.g., protein data), 
#' "column" for recoding the values by column (e.g., microsatellite data), "paired" 
#' for passing the values of allelic states and corresponding replacement values, using 
#' the replace_in and replace_out arguments (e.g. replace_in = c("A", "T", "C", "G"),
#' replace_out = c(1,2,3,4)).
#' @param replace_in vector with states of the data matrix to be replaced, when recode = "paired".
#' This argument must be used in conjunction with the argument "replace_out".
#' @param replace_out vector with states of the data matrix used for replacement, when recode = "paired".
#' This argument must be used in conjunction with the argument "replace_in".
#' @param nout Number of digits in the output when to_numeric = TRUE.
#' @param ... Additional parameters passed to \code{\link{eco.format} when to_numeric = TRUE}
#' @return XY.txt Matrix with coordinates.
#' @return NAMES.txt Matrix with row names.
#' @return P.txt Matrix with phenotypic data.
#' @return G.txt Matrix with genotypic data.
#' @examples
#' 
#' \dontrun{
#' 
#' data(eco.test)
#' ecogen2geneland(eco, dir = "", ncod=1)
#' 
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export


setGeneric("ecogen2geneland", 
           function(eco, dir = "", ncod = NULL, ploidy = 2,  
                    to_numeric = FALSE, nout = 3, 
                    recode = c("all", "column", "paired"),
                    replace_in = NULL,
                    replace_out =NULL, ...) {
             
             recode <- match.arg(recode)
            
              if(dir != "") {
                #add "/" to the end if path is "xxx/xxx"
               if(!grep("/$", dir)) {
               dir <- paste0(dir, "/")
               }
              }
             
             # check numeric format in G
             G_temp <- int.check.to_numeric(eco@G, to_numeric = to_numeric, 
                                            nout = nout, recode = recode, 
                                            ploidy = eco@INT@ploidy,
                                            ncod = eco@INT@ncod,
                                            ...)
             
             write.table(int.loc2al(G_temp,  ncod = ncod,  ploidy = ploidy), paste0(dir, "G.txt"),
                         quote = FALSE, row.names = FALSE, col.names = FALSE)
             
             write.table(eco@XY, paste0(dir, "XY.txt"), quote = FALSE,
                         row.names = FALSE, col.names = FALSE)
             
             write.table(rownames(eco@XY), paste0(dir, "NAMES.txt"), quote = FALSE, 
                         row.names = FALSE, col.names = FALSE)
             
             write.table(eco@P, paste0(dir, "P.txt"), quote = FALSE, row.names = FALSE, 
                         col.names = FALSE)
             
    
            
              if(dir == "") {
               return(paste0("Files written to: ", getwd()))
             } else {
               return(paste0("Files written to: ", gsub("/$", "", dir)))
             }
             
           })


#' Exporting an ecogen genetic data frame into Genepop format
#' 
#' @description This function converts the genetic 
#' data of an ecogen object into a Genepop input file. 
#' @param eco Object of class "ecogen".
#' @param dir output path. Default = "" (current directory).
#' @param outName The name of the output file.
#' @param grp The name of the S slot column with groups in which the sample
#' must be divided (e.g., populations). If groups are not given (grp = NULL),
#' all individuals will be assigned to a single one.
#' @param nout Number of digits in the output file.
#' @param sep Character separating alleles.
#' @param recode Recode mode: "none" for no recoding (defalut), "all" for recoding
#' the data considering all the individuals values at once (e.g., protein data), 
#' "column" for recoding the values by column (e.g., microsatellite data), "paired" 
#' for passing the values of allelic states and corresponding replacement values, using 
#' the replace_in and replace_out arguments (e.g. replace_in = c("A", "T", "C", "G"),
#' replace_out = c(1,2,3,4)).
#' @param replace_in vector with states of the data matrix to be replaced, when recode = "paired".
#' This argument must be used in conjunction with the argument "replace_out".
#' @param replace_out vector with states of the data matrix used for replacement, when recode = "paired".
#' This argument must be used in conjunction with the argument "replace_in".
#' @param ... Additional parameters passed to \code{\link{eco.format}}
#' @return A Genepop file in the working directory.
#' @examples 
#' 
#' \dontrun{
#' 
#' data(eco.test)
#' ecogen2genepop(eco, dir = "", outName = "infile.genepop.txt", grp = "pop")
#' # an output file "infile.genepop.txt" is generated in the working directory
#' 
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export


setGeneric("ecogen2genepop", 
           function(eco, dir = "", outName = "infile.genepop.txt", 
                    grp = NULL, nout = 3, sep = "",   
                    recode = c("none", "all", "column", "paired"),
                    replace_in = NULL,
                    replace_out =NULL,
                    ...) {
             
             if(dir != "") {
               #add "/" to the end if path is "xxx/xxx"
               if(!grep("/$", dir)) {
                 dir <- paste0(dir, "/")
               }
             }
             
             nout <- ceiling(nout)
             nout <- c(1,2,3)[c(1,2,3) %in% nout]
             if(length(nout) != 1)  {
               stop("nout must be 1, 2 or 3")
             }

             #check group consistency
             structures <- int.check.group(eco@S, grp = grp, exp.l = nrow(eco@G))
             structures <- as.factor(as.numeric(structures)) #recoding levels
             
             
             X <- eco.format(eco@G, ncod = eco@INT@ncod, 
                             nout = nout,
                             ploidy = eco@INT@ploidy,
                             fill.mode = "first",
                             recode = recode,
                             ...)
             
             X <- cbind(rep(0, nrow(X)), X)
             X[, 1] <- paste(rownames(X), ",")
             
             lista <- list()
             grp <- rep(" ", ncol(eco@G) + 1)
             grp <- as.matrix(t(grp))
             grp[1] <- "POP"
             matriz <- matrix(nrow = 0, ncol = ncol(eco@G)+1)
             for(i in seq_along(levels(structures))) {
               lista[[i]] <- X[structures == i, ]
               lista[[i]] <- rbind(grp, lista[[i]])
               matriz <- rbind(matriz, lista[[i]])
             }
             matriz <- rbind(matriz, rep("", ncol(matriz)))
             
             matriz[, 1] <- as.character(matriz[, 1])
             nombres <- rep("", (ncol(X)) ^ 2)
             nombres <- as.data.frame(matrix(nombres, ncol(X), ncol(X)))
             nombres[, 1] <- c("Data exported from EcoGenetics", colnames(X[, -1]))
             colnames(nombres) <- colnames(matriz)
             matriz <- rbind(as.matrix(nombres), matriz) 
             
             out <- ifelse(dir == "", paste0(getwd(), "/", outName), paste0(dir, outName))
             
             write.table(matriz, file = out, row.names = FALSE,
                         col.names = FALSE, quote = FALSE)
             
             print(paste0("File written to: ", out))
             
           })



#' Importing a Genepop file
#' 
#' @description This function converts a Genepop file into an object 
#' with a genetic matrix (G) and a structures matrix (S).
#' 
#' @param genefile Genepop file.
#' 
#' @return A list with the objects G (genetic matrix) and S (structures matrix).
#'
#' @examples 
#' \dontrun{
#' # ingpop, file with Genepop format in the folder "/extdata" of the package
#' 
#' ecopath <- paste(path.package("EcoGenetics"), "/extdata/ingpop", sep = "")
#' ingpop <- genepop2ecogen(ecopath)
#' ingpop
#' }
#' 
#' @export
#' @author Leandro Roser \email{learoser@@gmail.com}
#  Code adapted from Emiel van Loon and Scott Davis


setGeneric("genepop2ecogen", 
           function(genefile = NULL) {
             
             # read the file into memory
             con <- file(description = genefile, open = "rb")
             lines <- readLines(con)
             close(con)
             fileLength = length(lines)
             
             # checking that the file ends with a newline
             endline <- lines[fileLength]
             endline <- strsplit(endline, " ")[[1]]
             if(any(endline != "")) {
               stop("the file does not end with a newline")
             }
             
             #pop locations ignoring first 'title' line
             popLocations <- grep("POP", lines[2:fileLength], ignore.case=TRUE)
             popLocations <- popLocations + 1 
             
             # get title from first line
             # title <- lines[1]
             
             #get Loci Column names
             lociNames <- lines[2:(popLocations[1]-1)]
             lociNames <- gsub("\t","",lociNames)
             
             # number of individuals and populations
             npop <- length(popLocations)
             initial <- popLocations[1] 
             end <- length(lines)
             rownumber <- end - initial - npop
             
             nombres.loci <- unlist(strsplit( lociNames, '[[:space:]]+'))
             colnumber <- length(nombres.loci)
             
             out <- matrix(0, ncol = colnumber, nrow = rownumber)
             estructuras <- rep(0, rownumber)
             nombres <- character()
             
             #parsing of data between pops into genepop
             counter <- 1
             
             for(i in 1:npop) {
               beginLine <- popLocations[i] + 1
               endLine <- 0
               
               if( i == npop) {  
                 endLine <- length(lines) - 1
                 
               } else {  
                 endLine <- popLocations[i+1] - 1
               }
               
               #parse individual line
               
               for(line in lines[beginLine:endLine]) {
                 
                 #split id & alleles apart
                 individuo <- unlist(strsplit(line, ","))
                 nombres[counter]<- individuo[1]
                 estructuras[counter] <- i
                 haplotipo <- individuo[2]
                 
                 #split alleles apart on whitespace
                 loci <- unlist(strsplit(haplotipo, '[[:space:]]+'))
                 loci <- loci[2:length(loci)]
                 
                 #saving non zero lines into out
                 
                 out[counter, ] <- loci
                 counter <- counter + 1 
               }
             }
             out <- data.frame(out)
             rownames(out) <- nombres
             colnames(out) <- nombres.loci
             estructuras <- data.frame(factor(estructuras))
             colnames(estructuras)[1] <- "pop"
             rownames(estructuras) <- nombres
             list("G" = out, "S" = estructuras)
             
             ecogen(G=out, S=estructuras)
             
           })


#' Conversion form ecogen to genind and genind to ecogen
#' 
#' @description These functions export from ecogen to genind and viceversa
#' @param from Object of class "ecogen" / "genind"
#' @rdname ecogen2genind
#' @examples
#' 
#' \dontrun{
#' 
#' data(eco.test)
#' 
#' # ecogen to genind
#' outGenind <- ecogen2genind(eco)
#' outGenind
#' 
#' # genind to ecogen
#' outEco <- genind2ecogen(outGenind)
#' 
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export



setGeneric("ecogen2genind", function(from) { 
  
  if(!require(adegenet)) stop("Please install the adegenet package first")
  
  to <- adegenet::genind()
  
  if(!any(dim(from@XY) == 0)) {
    to@other$xy <- from@XY
  }
  
  if(!any(dim(from@A) == 0)) {
    to@tab <- from@A
    to@loc.fac <- from@INT@loc.fac
    nomloc <- levels(from@INT@loc.fac)
    temp<- tapply(rep(1, length(from@INT@loc.fac)), from@INT@loc.fac, sum)
    #array to list ("as.list()" do not works with the array)
    temp <- as(temp, "numeric")
    names(temp) <- nomloc
    to@loc.n.all <- temp
    temp <-  tapply(from@INT@all.names, names(from@INT@all.names), function(x) return(unname(x)),simplify = FALSE)
    #reorder temp an convert to list
    temp <- temp[pmatch(nomloc, names(temp))]
    nomloc <- names(temp)
    temp <- as(temp, "list")
    names(temp) <- nomloc
    to@all.names <- temp
    to@ploidy <- rep(from@INT@ploidy, nrow(from@A))
    to@type <- ifelse(from@INT@type == "codominant", "codom", "PA")
  }
  #to@other
  #to@pop
  if(!any(dim(from@S) == 0)) {
    to@strata <- from@S
  }
  #to@hierarchy
  
  to
})

#' genind2ecogen
#' @rdname ecogen2genind
#' @export

setGeneric("genind2ecogen", function(from) { 
  
  if(!require(adegenet)) stop("Please install the adegenet package first")
  
  to <- adegenet::genind2df(from, usepop = FALSE)
  to[to == ""] <- NA
  to <- ecogen(G = to)
 
  if(!is.null(from@other$xy)) {
    if(nrow(from@other$xy) != nrow(to@G)) {
    message("Note: other$xy slot with a different number of rows 
    than the present in the genetic data table. Skipping this data\n")  
    } else {
    ecoslot.XY(to) <- from@other$xy
    }
  }
 
  if(!is.null(from@strata)) {
    if(nrow(from@strata) != nrow(to@G)) {
      message("Note: strata slot with a different number of rows 
      than the present in the genetic data table. Skipping this data\n")  
    } else {
    ecoslot.S(to) <- from@strata
    }
  }
  to
})



#' Conversion from ecogen to gstudio and gstudio to ecogen
#' 
#' @description These functions converts the genetic 
#' data of an ecogen object in a gstudio data frame or viceversa.
#' @param from Input object of class "ecogen" or "gstudio" (depending the direction of conversion)
#' @param type The type of data:  "codominant" (for codominant data); "dominant" for presence - absence data. 
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' data(eco.test)
#' togstudio <- ecogen2gstudio(eco, type = "codominant")
#' togstudio
#' toeco <- gstudio2ecogen(togstudio, ID = "ID", lat = "Latitude", 
#' lon = "Longitude", struct = "pop")
#' toeco
#' # as ID, Latitude and Longitude are column names in the <togstudio> data frame 
#' # (that match default parameter values for gstudio2ecogen), 
#' # the latter is identical to this:
#' toeco <- gstudio2ecogen(togstudio, struct = "pop")
#' toeco
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export

setGeneric("ecogen2gstudio", 
           function(from, type = c("codominant", "dominant")) {
             
             require(gstudio)
             
             type <- match.arg(type)
             if(type == "codominant") {
               dat <- eco.convert(from@G, "matrix", sep.out = ":")
               dat <- as.data.frame(dat, stringsAsFactors = FALSE)
               for(i in 1:ncol(dat)) {
                 class(dat[, i]) <- "locus"
                 #out[[i]] <- gstudio::locus(dat[, i, drop = FALSE], type = "separated")
               }
               dat[is.na(dat)] <- gstudio::locus(NA)
               colnames(dat) <- colnames(from@G)
               
               #dominant case
             } else {
               dat<-eco@G
               dat <- as.data.frame(dat, stringsAsFactors = FALSE)
               for(i in 1:ncol(dat)) {
                 class(dat[, i]) = "locus"
               }
               dat[is.na(dat)] <- gstudio::locus(NA)
               colnames(dat) <- colnames(from@G)
             }
             
             #create output data frame
             if(all(dim(from@XY) != 0)) {
               to <- data.frame(ID = rownames(from@XY), Latitude = from@XY[,2],  Longitude = from@XY[,1])
             }
             if(all(dim(from@S) != 0)) {
               to <- data.frame(to, from@S)
             }
             to <- data.frame(to, dat)
             
             to
           })


#' Conversion from gstudio to ecogen
#' 
#' @param ID name of the column with ID (default "ID") 
#' @param lat name of the column with latitude (default "Latitude")
#' @param lon name of the column with longitude (default "Longitude") 
#' @param struct vector with name of the columns with structures (default NULL)
#' @rdname ecogen2gstudio
#' @export


setGeneric("gstudio2ecogen", function(from, ID = "ID", 
                                      lat = "Latitude", lon = "Longitude",
                                      struct = NULL) {
  
  myID <- myLat <- myLon <- NULL
  
  myID <- match(ID, colnames(from))
  if(is.na(myID)) {
    message(paste0("Note: <", ID,"> does not match the name of any column. Creating generic labels"))
    myID <- paste0("I.", seq_len(nrow(from)))
  } else {
    myID <- from[, myID]
  }

    myLat <- match(lat, colnames(from))
    if(is.na(myLat)) {
      message(paste0("Note: <", lat, "> does not match the name of any column"))
    }
  
    myLon <- match(lon, colnames(from))
    if(is.na(myLon)) {
      message(paste0("Note: <", lon, "> does not match the name of any column"))
    }

  if(!is.null(struct)) {
    myStruct <- match(struct, colnames(from))
    if(!is.na(myStruct) && length(myStruct) < length(struct)) {
      stop("some of all names of structures are not matching with the name of columns in 'from'")
    }
  } 
  
  if(is.na(myLat) && !is.na(myLon) || !is.na(myLat) && is.na(myLon)) {
    stop("Longitude requires Latitude data an viceversa, one of both is NA")
  }
  
  to<- ecogen()
  
  cuales <- sapply(from, class)
  myloc <- from[, cuales == "locus"]
  myloc <- as.matrix(myloc)
  rownames(myloc) <- myID
  myloc[myloc == ""] <- NA
  #G <- eco.convert(myloc, sep.in = ":", sep.out = "")
  ecoslot.G(to, sep =":") <- myloc
  
  if(!is.na(myLat) && !is.na(myLon)) {
    XY <- data.frame(Longitude = from[, myLon], Latitude = from[, myLat])
    rownames(XY) <- myID
    ecoslot.XY(to) <- XY
  }
  
  if(!is.null(struct)) {
    S <- data.frame(from[, myStruct])
    rownames(S) <- myID
    ecoslot.S(to) <- S
  }
  
  to@ATTR$names <- myID
  validObject(to)
  to
  
})



#' Converting an ecogen genetic data frame into a hierfstat data frame
#' 
#' @description This function converts the genetic 
#' data of an ecogen object into a hierfstat data frame. 
#' @param eco Object of class "ecogen".
#' @param pop The name of the S slot column with the groups 
#' for the hierfstat data frame.
#' @param to_numeric Recode the genetic data into numeric format? If TRUE, 
#' the functions performs the correction via \code{\link{eco.format}}.
#' Additional formatting parameters can be passed to this function.
#' @param nout Number of digits in the output when to_numeric = TRUE.
#' @param recode Recode mode when to_numeric = TRUE: "all" for recoding
#' the data considering all the individuals values at once (e.g., protein data), 
#' "column" for recoding the values by column (e.g., microsatellite data), "paired" 
#' for passing the values of allelic states and corresponding replacement values, using 
#' the replace_in and replace_out arguments (e.g. replace_in = c("A", "T", "C", "G"),
#' replace_out = c(1,2,3,4)).
#' @param replace_in vector with states of the data matrix to be replaced, when recode = "paired".
#' This argument must be used in conjunction with the argument "replace_out".
#' @param replace_out vector with states of the data matrix used for replacement, when recode = "paired".
#' This argument must be used in conjunction with the argument "replace_in".
#' @param ... Additional parameters passed to \code{\link{eco.format}} when to_numeric = TRUE
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' data(eco.test)
#' hiereco <- ecogen2hierfstat(eco, "pop", to_numeric = TRUE)
#' require("hierfstat")
#' basic.stats(hiereco)
#' 
#' }
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export


setGeneric("ecogen2hierfstat", 
           function(eco, pop = NULL, to_numeric = FALSE, nout = 3, 
                    recode = c("all", "column", "paired"),
                    replace_in = NULL,
                    replace_out =NULL,
                    ...) {
             
             recode <- match.arg(recode)
             
             u <- eco@G
             
             # check that the data is in numeric format, using the first <= 20 columns
            
             u <- int.check.to_numeric(u, to_numeric = to_numeric, 
                                       nout = nout, recode = recode, 
                                       replace_in = replace_in, replace_out = replace_out,
                                       ploidy = eco@INT@ploidy,
                                       ncod = eco@INT@ncod, ...)
             
             groups <- eco@S
             
             if(is.null(pop))
             {
               factord <- as.data.frame(rep(1, nrow(u)))
               cnom <- "pop"
               rnom <- rownames(eco@G)
               Gord <- u
             } else {
               
               pop <- match(pop, colnames(groups), nomatch = 0)
               pop <- pop[pop != 0]
               if(length(pop) == 0) {
                 stop("incorrect factor name")
               }
               orden <- order(groups[, pop])
               Gord <- u[orden,]
               factord <- groups[orden, pop]
               factord <- as.numeric(factord)
               cnom <- colnames(groups[pop])
               rnom <- rownames(eco@G)[orden]
             }
             
             datahier <- data.frame(factord, Gord)
             colnames(datahier)[1] <- cnom
             rownames(datahier) <- rnom
             datahier
             
             #class control
             clases <- character()
             j <- 1
             for(i in 2:ncol(datahier)) {
               clases[j] <- class(datahier[, i])
               j <- j + 1
             }
             if(any(clases != "numeric" | clases != "integer")) {
               datahier <- as.matrix(datahier)
               colhier <- ncol(datahier)
               rowhier <- nrow(datahier)
               datahier <- matrix(as.numeric(datahier), ncol = colhier, nrow= rowhier)
               datahier <- as.data.frame(datahier)
               datahier[, 1] <- as.factor(datahier[, 1])
             }
             
             rownames(datahier) <- rownames(eco@G)
             colnames(datahier)[1] <- "Pop"
             colnames(datahier)[-1] <- colnames(eco@G)
             
             datahier
             
           })


#' Exporting an ecogen genetic data frame into SPAGeDi format
#' 
#' @description This function converts the genetic data of an ecogen object 
#' in a SPAGeDi input file.  
#' When distance classes are required, they can be constructed by combining 
#' the parameters "int", "smin", "smax", "nclass", "seqvec" and "size", as
#' described in the function \code{\link{eco.lagweight}}.
#' A distance matrix can also be included using the "distmat" parameter.
#' Missing data must be coded as a single "NA" in the G data frame. 
#' @param eco Object of class "ecogen".
#' @param dir output path. Default = "" (current directory).
#' @param pop The name of the S slot column with the groups 
#' for the output data. The default option includes all the individuals into 
#' a single group.
#' @param ndig Number of digits coding each allele in the output file
#'  (e.g., 1: x, 2: xx, or 3: xxx). If NULL, the vale will be deduced from
#'  the number of digits used for coding alleles in the ecogen object.
#' @param outName The name of the output file.
#' @param int Distance interval in the units of the XY slot data.
#' @param smin Minimum class distance in the units of the XY slot data.
#' @param smax Maximum class distance in the units of the XY slot data.
#' @param nclass Number of classes.
#' @param seqvec Vector with breaks in the units of the XY slot data.
#' @param size Number of individuals per class.
#' @param bin Rule for constructing intervals when a partition parameter (int, 
#' nclass or size) is not given. Default is Sturge's rule (Sturges, 1926). Other
#' option is Freedman-Diaconis method (Freedman and Diaconis, 1981).
#' @param distmat Distance matrix to include (optional).
#' @param latlon Are the coordinates in decimal degrees format? Defalut FALSE. If TRUE,
#' the coordinates must be in a matrix/data frame with the longitude in the first
#' column and latitude in the second. The position is projected onto a plane in
#' meters with the function \code{\link[SoDA]{geoXY}}.
#' @param to_numeric Recode the genetic data into numeric format? If TRUE, 
#' the functions performs the correction via \code{\link{eco.format}}.
#' Additional formatting parameters can be passed to this function.
#' @param nout Number of digits in the output when to_numeric = TRUE.
#' @param recode Recode mode when to_numeric = TRUE: "all" for recoding
#' the data considering all the individuals values at once (e.g., protein data), 
#' "column" for recoding the values by column (e.g., microsatellite data), "paired" 
#' for passing the values of allelic states and corresponding replacement values, using 
#' the replace_in and replace_out arguments (e.g. replace_in = c("A", "T", "C", "G"),
#' replace_out = c(1,2,3,4)).
#' @param replace_in vector with states of the data matrix to be replaced, when recode = "paired".
#' This argument must be used in conjunction with the argument "replace_out".
#' @param replace_out vector with states of the data matrix used for replacement, when recode = "paired".
#' This argument must be used in conjunction with the argument "replace_in".
#' @param ... Additional parameters passed to \code{\link{eco.format} when to_numeric = TRUE}
#' @examples
#' 
#' \dontrun{
#' 
#' data(eco.test)
#' ecogen2spagedi(eco, dir = "", pop = "pop", ndig = 1,int=2, smax=6, outName="infile.spagedi.txt")
#' 
#' }
#' 
#' @references 
#' 
#' Freedman D., and P. Diaconis. 1981. On the histogram as a density estimator: 
#' L 2 theory. Probability theory and related fields, 57: 453-476.
#' 
#' Hardy O. and X Vekemans. 2002. SPAGeDi: a versatile computer program 
#' to analyse spatial genetic structure at the individual or population levels. 
#' Molecular ecology notes, 2: 18-620.
#' 
#' Sturges  H. 1926. The choice of a class interval. Journal of the American 
#' Statistical Association, 21: 65-66.
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export


setGeneric("ecogen2spagedi", 
                        function(eco, 
                         pop = NULL, 
                         ndig = NULL,
                         dir = "",
                         outName = "infile.spagedi.txt", 
                         smin = 0,
                         smax= NULL,
                         int = NULL, 
                         nclass = NULL,
                         seqvec = NULL,
                         size = NULL,
                         bin = c("sturges", "FD"),
                         distmat = NULL,
                         latlon = FALSE,
                         to_numeric = FALSE,
                         nout = 3, 
                         recode = c("all", "column", "paired"),
                         replace_in = NULL,
                         replace_out =NULL,
                         ...) {
                          
 if(dir != "") {
 #add "/" to the end if path is "xxx/xxx"
 if(!grep("/$", dir)) {
 dir <- paste0(dir, "/")
  }
}
  
                          
  recode <- match.arg(recode)                        
  bin <- match.arg(bin)
  if(is.null(pop)) {
    pop <- rep(1, nrow(eco@XY))
  } else {
    pop <- as.numeric(eco@S[, which(colnames(eco@S)== pop)])
  }
  
  if(is.null(ndig)) {
  ndig <-eco@INT@ncod
  }
  
  if(sum(pop == 0)) {
    stop("non matching S column name")
  }
  
  # check that the data is in numeric format, using the first <= 20 columns
  
  gmat <- int.check.to_numeric(eco@G, to_numeric = to_numeric, 
                            nout = ndig, recode = recode, ploidy = eco@INT@ploidy,
                            ncod = eco@INT@ncod, ...)
  gmat <- as.matrix(gmat)
  
  ploidy <- eco@INT@ploidy
  gmat[gmat == "NA" | is.na(gmat)] <- paste(rep("0", ploidy), collapse="")
  
  matriz <- data.frame(rownames(eco@G), pop, eco@XY, gmat)
  matriz <- as.matrix(matriz)
  colnames(matriz) <- c("Individual", "Population",
                        colnames(eco@XY), colnames(eco@G))
  
  arriba <- rep("", ncol(matriz))
  arriba[1] <- nrow(matriz)
  arriba[2] <- max(pop)
  arriba[3] <- ncol(eco@XY)
  arriba[4] <- ncol(eco@G)
  arriba[5] <- ndig
  arriba[6] <- eco@INT@ploidy
  
  if(!is.null(smax) | !is.null(seqvec)) {
    
    xy <- eco@XY[,1:2]
    if(latlon) {
      dist(SoDA::geoXY(xy[,2], xy[,1], unit=1))
    }
    
    input <- int.break(XY = xy, 
                       int = int, 
                       smin = smin,
                       smax = smax,
                       size = size,
                       nclass = nclass,
                       seqvec = seqvec,
                       latlon = latlon,
                       bin = bin)
    
    breakpoints <- input$breakpoints
    
  } else  {
    breakpoints <- NULL
  }
  
  final <- rep("", ncol(matriz))
  final[1] <- "END"
  
  # start to write file here
  out <- ifelse(dir == "", paste0(getwd(), "/", outName), paste0(dir, outName))
  sink(out)
  cat(arriba, sep = "\t")
  cat("\n")
  if(!is.null(breakpoints)) {
    cat(length(breakpoints), breakpoints, sep = "\t")
    cat("\n")
  } else {
    cat(0)
  }
  cat("\n")
  cat(colnames(matriz), sep = "\t")
  cat("\n")
  write.table(matriz, sep = "\t", quote = FALSE, row.names = FALSE, 
              col.names = FALSE)
  cat(final, sep = "\t")
  
  if(!is.null(distmat)) {
    if(inherits(distmat, "dist")) {
      distmat <- as.matrix(distmat)
    } 
    if(!(inherits(distmat, "matrix") || inherits(distmat, "data.frame"))) {
      stop("invalid distance matrix format (It should be of class: dist, matrix or data.frame")
    }
    distnames <- rownames(distmat)
    distmat<-data.frame(distnames, distmat)
    colnames(distmat)[1]<-paste("M", nrow(eco@XY), sep="")
    cat("\n")
    write.table(distmat, sep = "\t", quote = FALSE, row.names = FALSE, 
                col.names = TRUE)
    cat("END")
  }
  
  sink()
  
  # output message
  print(paste0("File written to: ", out))
  
})


#' Importing a SPAGeDi file, via conversion to ecogen
#' 
#' @description This function converts a SPAGeDi file into a ecogen object

#' @param infile Path to the SPAGeDi file.
#' @param sep Character separating alleles (codominant data). 
#' Default option is no character separating alleles.
#' @param missCode characters to represent missing genotypes in codominant markers. 
#' If NULL, is computed as "0" times the numer of characters coding alleles.
#' @param type Marker type: "codominant" or "dominant".
#' @param ... additional arguments passed to \code{ecogen}

#' @examples
#' 
#' \dontrun{
#' 
#' data(eco.test)
#' ecogen2spagedi(eco, dir = "", pop = "pop", ndig = 1,int=2, smax=6, outName="infile.spagedi.txt")
#' spagedi2ecogen("infile.spagedi.txt", sep = "")
#' }
#' 
#' 
#' @author Leandro Roser \email{learoser@@gmail.com}
#' @export



setGeneric("spagedi2ecogen", 
           function(infile, sep = "", missCode = NULL, 
                    type = c("codominant", "dominant"), 
                    ...) {
  
  type <- match.arg(type)
  # get lines
  Lines <- readLines(infile)
  Lines <- Lines[Lines != ""]
  # remove comments (comments start with //)
  com <-sapply(Lines, substring, first=1, last=2)
  Lines<-Lines[com != "//"]
  
  # get data from the first line
  fileheader <- as.integer(strsplit(Lines[1], "\t")[[1]])
  
  # number of individuals
  nind<-fileheader[1] 
  # Number of categories
  ncat<-fileheader[2] 
  # Number of spatial coordinates
  nXY<- fileheader[3]
  # Number of digits to represent alleles
  ndig<-fileheader[5] 
  
  # Read the rest of the file as a table and converto to ecogen
  myLines <- Lines[3:(3+nind)]
  myLines <- strsplit(myLines, "\t")
  myLines <- do.call("rbind", myLines)
  colnames(myLines) <- myLines[1, ]
  myLines <- myLines[-1, ]
  myLines <- gsub(" ", "", myLines)
  rownames(myLines) <- myLines[, 1]
  myLines <- myLines[, -1, drop = FALSE]
  
  if(ncat != 0) {
    S <-myLines[, 1, drop = FALSE]
    G <- myLines[, -1, drop = FALSE]
  } else  {
    S <- data.frame()
  }
  
  if(nXY > 0) {
    XY <- G[, seq_len(nXY), drop = FALSE]
    G <- G[, -seq_len(nXY), drop = FALSE]
  } else {
    XY <- data.frame()
  }
  
  
  # if is null missing, missing is 0 times the number of characters coding alleles
  if(type == "codominant") {
    if(is.null(missCode)) {
      miss <- paste(rep(0, ndig), collapse = "")
    }
    
    # remove missing individuals
    G[as.numeric(G) == 0] <- miss
    
    # replace incomplete individuals by missing data
    G <- gsub(paste0("(^", miss, ".*)|(.*", miss, "$)"), miss, G)
  }
  
  ecogen(XY = XY, G = G, S = S, missing = miss, type = type, ...)
  
})
