context("Test general functions")

require(vegan)
require(magrittr)
require(raster)

data(eco.test)
data(eco3)
data(eco2)
data(tab)
set.seed(6)


test_that("eco.alfreq works fine", {
  skip_on_cran()
  expect_true(class(eco.alfreq(eco)[[1]])[2] == "ggplot")
  expect_true(class(eco.alfreq(eco, "pop")[[1]])[2] == "ggplot")
})

test_that("eco.association works fine", {
  skip_on_cran()
  t1 <- eco.association(eco, "within", "pop")
  t2 <- eco.association(eco, "within", "pop", adjust="fdr")
  t3 <- eco.association(eco, "within", "pop", method = "chisq.test")
  t4 <- eco.association(eco, "between", "pop", ndig = 1)
  t5 <- eco.association(eco, "between", "pop", method = "chisq.test", ndig = 1)
  expect_true(all(dim(t1) == c(40,1)))
  expect_true(all(dim(t2) == c(40,1)))
  expect_true(all(dim(t3) == c(40,2)))
  expect_true(all(dim(t4) == c(10,1)))
  expect_true(all(dim(t5) == c(10,2)))
})

test_that("eco.convert works fine", {
  skip_on_cran()
  thisdim <- dim(eco3@G)
  loc2al <- eco.convert(eco3[["G"]], "matrix", "alleles.matrix", ploidy = 2)
  al2loc <- eco.convert(loc2al, "alleles.matrix", "matrix", ploidy = 2)
  loc2loc <- eco.convert(eco3[["G"]], "matrix", "matrix", ploidy = 2, sep.out = "/")
  loc2loc.nosep <- eco.convert(loc2loc, "matrix", "matrix", ploidy = 2, sep.in = "/", sep.out = "")
  loc2list <- eco.convert(eco3[["G"]], "matrix", "list", ploidy = 2)
  al2list <- eco.convert(eco3[["G"]], "matrix",  "alleles.list", ploidy = 2)
  

  expect_true(dim(loc2al)[2] == (2*thisdim)[2])
  expect_true(dim(al2loc)[2] == thisdim[2])
  expect_true(dim(loc2loc)[2] == thisdim[2])
  expect_true(nchar(loc2loc[1]) == 3)
  expect_true(nchar(loc2loc.nosep[1]) == 2)
  expect_true(class(loc2list) == "list")
  expect_true(nchar(loc2list[[1]])[1] == 2)
  expect_true(nchar(al2list[[1]])[1] == 1)
})


test_that("eco.detrend works fine", {
  skip_on_cran()
  obj <- eco.detrend(Z = eco2[["P"]][,2], XY =  eco2[["XY"]], degree =  1)
  expect_true(class(obj) == "eco.detrend")
  expect_true(all(dim(obj@RES) == c(900, 1)))
})


test_that("eco.format works fine", {
  skip_on_cran()
  set.seed(6)
  example <- as.matrix(genotype[1:10,])
  mode(example) <- "character"
  ex1 <- eco.format(example, ncod = 1, ploidy = 2, nout = 3)
  expect_true(nchar(ex1[1]) == 6)
  
  tetrap <- as.matrix(example)
  tetrap <- matrix(paste(example,example, sep = ""), ncol = ncol(example)) 
  ex2 <- eco.format(tetrap, ncod = 1, ploidy = 4, sep.out = "/")
  expect_true(nchar(ex2[1]) == 15)
  
  temp <- c("A","T","G","C")
  temp <- sample(temp, 100, rep= T)
  temp <- matrix(temp, 10, 10)
  colnames(temp) <- letters[1:10]
  rownames(temp) <- LETTERS[1:10]
  ex3 <- eco.format(temp, ploidy = 1, nout = 1,  recode = "all", show.codes = TRUE)
  expect_true(nchar(ex3[[1]][1]) == 1)

  samp1 <- sample(c("A","T","G","C"), 100, replace = TRUE)
  samp2 <- sample(c("A","T","G","C"), 100, replace = TRUE)
  paired <- matrix(paste0(samp1, samp2), 10, 10)
  # Generate some NAs
  paired[sample(1:100, 10)]<-NA
  #ex4 <- eco.format(paired, recode = "paired", replace_in = c("A", "T", "G", "C"),
  #                  replace_out = c(1, 2, 3, 4))
  #expect_true(ex4[1] == "32")
  
  
  temp2 <- c("Ala", "Asx", "Cys", "Asp", "Glu", "Phe", "Gly", "His", "Ile", 
          "Lys", "Leu", "Met", "Asn", "Pro", "Gln", "Arg", "Ser", "Thr", 
          "Val", "Trp")
  temp2 <- sample(temp2, 100, rep= T)
  ex5 <- paste(temp2, temp2, sep="")
  missing.ex <- sample(1:100, 20)
  ex5[missing.ex] <-NA
  ex5 <- matrix(ex5, 10, 10)
  colnames(ex5) <- letters[1:10]
  rownames(ex5) <- LETTERS[1:10]
 
  ex5 <- eco.format(ex5, ncod = 3, ploidy = 2, 
                        nout = 2, recode = "column")
 
  #expect_that(ex5[1], equals("1414"))
  
  temp2 <- as.data.frame(temp2)

  ex6 <- eco.format(temp2, ploidy = 1, recode = "all")
  #expect_that(ex6[1], equals("113"))

})


test_that("eco.lmtree works fine", {
  skip_on_cran()
  
  obj1 <- eco.lmtree(df1 = eco3[["P"]], df2 = eco3[["E"]], 
  analysis = "mlm")                                 
  obj2 <- eco.lmtree(df1 = eco3[["P"]], df2 = eco3[["E"]], 
  analysis = "mctree", fact = eco3[["S"]]$pop) 
  
  expect_true(class(obj1) == "eco.mlm")
  expect_true(class(obj2) == "eco.mctree")
  
})

test_that("eco.formula works fine", {
  skip_on_cran()
  
 obj <- eco.formula(eco, P1 + P2 + P3 + U(A) ~ E1 + E2 + Condition(X+Y))
 obj2 <- eco.formula(eco, P1 + P2 + P3 + U(A) ~ E1 + E2 + X + Y)
 obj3 <- eco.formula(eco, P1 + P2 + P3 + U(A) ~ U(E) + Condition(X+Y)) %>% rda
 
 expect_true(class(obj) == "formula")
 expect_true(class(rda(obj))[1] == "rda")
 expect_true(class(obj2) == "formula")
 expect_true(class(lm(obj2)) == "lm")
 expect_true(class(obj3)[1] == "rda")
  
})


test_that("eco.pairtest works fine", {
  skip_on_cran()
  t1 <- eco.pairtest(eco = eco3, df = "P", x = "structure")
  t2 <- eco.pairtest(eco = eco3, df = "E", x = "structure")
  t3 <- eco.pairtest(eco = eco3, df = "P", x = "structure", only.p = FALSE)
  t4 <- eco.pairtest(eco = eco3, df = "P", x = "structure", test = "tukey")
  expect_true(inherits(t1$kruskall.test, "matrix"))
  expect_true(inherits(t2$kruskall.test, "matrix"))
  expect_true(inherits(t3$kruskall.test, "matrix"))
  expect_true(inherits(t4$aov, "matrix"))
})




test_that("eco.NDVI works fine", {
  skip_on_cran()
  
  temp <-list()
  
  # we create 4 simulated rasters for the data included in the object tab:
  
  for(i in 1:4) {
    temp[[i]] <- runif(19800, 0, 254)
    temp[[i]] <- matrix(temp[[i]], 180, 110)
    temp[[i]] <- raster(temp[[i]],  crs = "+proj=utm +zone=32")
    extent(temp[[i]])<-c(3770000, 3950000, 6810000, 6920000)
  }
  
  writeRaster(temp[[1]], "20040719b4.tif", overwrite=T)
  writeRaster(temp[[2]], "20040719b3.tif", overwrite=T)
  writeRaster(temp[[3]], "20091106b4.tif", overwrite=T)
  writeRaster(temp[[4]], "20091106b3.tif", overwrite=T)
  eco.NDVI(tab, "COST", "NDVI", "LT5")
  
  example <- raster("NDVICOST20040719.tif")
  
  expect_true(all(dim(example) == c(180, 110, 1)))
  
  eco.NDVI.post(tab, "COST", "NDVI", what = c("mean", "var"))
  mean.ndvi <- raster("NDVI.COST.mean.tif")
  
  
  expect_true(all(dim(example) == c(180, 110, 1)))
  
  expect_true(all(dim(mean.ndvi) == c(180, 110, 1)))
  
  file.remove(c("NDVICOST20040719.tif", "NDVICOST20091106.tif",
                "20040719b4.tif", "20040719b3.tif", "20091106b4.tif", 
                "20091106b3.tif", "NDVI.COST.mean.tif", "NDVI.COST.var.tif",
                "NDVICOSTtime.tif"))
})


test_that("eco.theilsen works fine", {
  skip_on_cran()
  set.seed(6)
  
  temp <- list()
  for(i in 1:100) {
    temp[[i]] <- runif(36,-1, 1)
    temp[[i]][sample(1:36, 10)] <- NA
    temp[[i]] <- matrix(temp[[i]], 6, 6)
    temp[[i]] <- raster(temp[[i]])
  }
  
  temp <- brick(temp)
  
  
  writeRaster(temp,"temporal.tif", overwrite=T)
  rm(temp)
  ndvisim <- brick("temporal.tif")
  
  date <- seq(from = 1990.1, length.out = 100, by = 0.2)
  
  # parallel
  # For some reason, the check allows up to 2 workers
  eco.theilsen(ndvisim, date, workers = 2, run_parallel = TRUE)
  
  pvalue <- raster("pvalue.tif")
  slope <- raster("slope.tif")
  
  expect_true(file.exists("slope.tif"))
  expect_true(file.exists("pvalue.tif"))
  # expect_true(!all(is.na(as.matrix(slope))))
  # expect_true(!all(is.na(as.matrix(pvalue))))
  
  file.remove(c("slope.tif", "pvalue.tif"))
  
  ## serial
  eco.theilsen(ndvisim, date, run_parallel  = FALSE)
  
  pvalue <- raster("pvalue.tif")
  slope <- raster("slope.tif")
  
  expect_true(file.exists("slope.tif"))
  expect_true(file.exists("pvalue.tif"))
  # expect_true(!all(is.na(as.matrix(slope))))
  # expect_true(!all(is.na(as.matrix(pvalue))))
  # 
  file.remove(c("temporal.tif", "slope.tif", "pvalue.tif"))
})


test_that("eco.dom_af works fine", {
  skip_on_cran()
  out <- eco.dom_af(my_ecopop)
  expect_true(all(names(out) == c("freq", "var")))
})


