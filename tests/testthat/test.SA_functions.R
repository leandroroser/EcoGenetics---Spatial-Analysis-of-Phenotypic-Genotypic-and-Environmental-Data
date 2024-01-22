context("Test SA functions")

require(adegenet)
data(eco.test)
data(eco3)

test_that("eco.correlog Moran'S I works", {
  skip_on_cran()
  obj <- eco.correlog(Z=eco[["P"]][,1], XY = eco[["XY"]], 
                      method = "I", smax=10, size=1000)
  obj2 <- eco.correlog(Z=eco[["P"]][,1], XY = eco[["XY"]], 
                                  method = "I", smax = 10, size = 1000, angle  = seq(0, 175, 5))
  obj3 <- eco.correlog(Z=eco[["P"]], XY = eco[["XY"]], method = "I")
  expect_true(class(obj) == "eco.correlog")
  expect_true(class(eco.plotCorrelog(obj))[1] == "plotly")
  expect_true(class(eco.plotCorrelog(obj, interactivePlot = FALSE))[2] == "ggplot")
  expect_true(class(obj2) == "eco.correlogB")
  expect_true(class(eco.plotCorrelogB(obj2))[1] ==  "plotly")
  expect_true(class(eco.plotCorrelogB(obj2, interactivePlot = FALSE))[2] == "ggplot")
  expect_true(class(eco.plotCorrelogB(obj2, var = 1))[1] == "plotly")
  expect_true(class(eco.plotCorrelogB(obj2, var = 1, interactivePlot = FALSE))[2] ==  "ggplot")
  expect_true(class(obj3) == "eco.correlog")
  expect_true(class(eco.plotCorrelog(obj3)[[1]])[1] == "plotly")
})

test_that("eco.correlog Geary's C works", {
  skip_on_cran()
  obj <- eco.correlog(Z = eco[["P"]][,1], XY = eco[["XY"]], method = "C", smax=10, size=1000)
  expect_true(class(obj) == "eco.correlog")
  expect_true(class(eco.plotCorrelogB(obj))[1]  == "plotly")
})

test_that("eco.correlog Bivariate Moran's I works", {
  skip_on_cran()
  obj <- eco.correlog(Z=eco[["P"]][,1], XY = eco[["XY"]], Y = eco[["P"]][, 1],
                      method = "CC", int= 2, smax=15)
  expect_true(class(obj) == "eco.correlog")
  expect_true(class(eco.plotCorrelogB(obj))[1] == "plotly")
})

test_that("eco.variogram works", {
  skip_on_cran()
  obj <- eco.variogram(Z = eco[["P"]][, 2],XY =  eco[["XY"]])
  obj2 <- eco.variogram(Z = eco[["P"]][, 2],XY =  eco[["XY"]], angle = 30)
  expect_true(class(obj) == "eco.correlog")
  expect_true(class(eco.plotCorrelog(obj))[1] == "plotly")
  expect_true(class(eco.plotCorrelog(obj2))[1] == "plotly")
})

test_that("eco.cormantel works", {
  skip_on_cran()
  obj <- eco.cormantel(M = dist(eco[["P"]]), size=1000,smax=7, XY = eco[["XY"]],
                        nsim = 99)
  objd<- eco.cormantel(M = dist(eco[["P"]]), size=1000,smax=7, XY = eco[["XY"]],
                          nsim = 99, angle = seq(0, 170, 10))
  objp <- corm <- eco.cormantel(M = dist(eco[["P"]]), MC = dist(eco[["E"]]),
                                size=1000, smax=7, XY = eco[["XY"]], nsim = 99)
  expect_true(class(obj) == "eco.correlog")
  expect_true(class(eco.plotCorrelog(obj))[1] == "plotly")
  expect_true(class(objd) == "eco.correlogB")
  expect_true(class(eco.plotCorrelogB(objd))[1] ==  "plotly")
  expect_true(class(eco.plotCorrelogB(objd, interactivePlot = FALSE))[2] == "ggplot")
  expect_true(class(objp) == "eco.correlog")
  expect_true(class(eco.plotCorrelog(objp))[1] == "plotly")
})

test_that("eco.gsa works", {
  skip_on_cran()
  
  con <- eco.weight(eco[["XY"]], method = "circle", d1 = 0, d2 = 2)
  global <- eco.gsa(Z = eco[["P"]][, 1], con = con, method = "I", nsim = 200)
  
  con2<-chooseCN(eco[["XY"]], type = 1, result.type = "listw", plot.nb = FALSE)
  global2 <- eco.gsa(Z = eco[["P"]][, 1], con = con2, method = "I", nsim = 200)
  
  global.C <- eco.gsa(Z = eco[["P"]][, 1], con = con, method = "C", nsim = 200)

  global.Ixy <- eco.gsa(Z = eco[["P"]][, 1], Y = eco[["E"]][, 1], 
                        con = con, method = "CC", nsim = 200)
  
  Z <- 2* eco[["A"]]
  jc <- eco.gsa(Z[, 1], con = con, method = "JC")
 
  global.JC <- eco.gsa(Z[, 1:10], con = con, method = "JC", nsim = 99)
 
  expect_true(class(global) == "eco.gsa")
  expect_true(class(global2) == "eco.gsa")
  expect_true(class(global.C) == "eco.gsa")
  expect_true(class(global.Ixy) == "eco.gsa")
  expect_true(class(jc) == "eco.gsa")
  expect_true(class(eco.plotGlobal(jc, interactivePlot = TRUE))[1] == "plotly")
  expect_true(class(eco.plotGlobal(jc, interactivePlot = FALSE))[2] == "ggplot")
})

test_that("eco.mantel works", {
  skip_on_cran()
  
  obj1 <- eco.mantel(d1 = dist(eco[["P"]]), d2 = dist(eco[["E"]]), nsim = 99)  
  obj2 <- eco.mantel(d1 = dist(eco[["P"]]), d2 = dist(eco[["E"]]), dc = dist(eco[["XY"]]), nsim = 99)                               
  con <- eco.weight(eco@XY, method="circle", d2=5)
  obj3 <- eco.mantel(dist(eco[["P"]]), dist(eco[["XY"]]), con=con)
  con2 <- eco.bearing(XY = eco[["XY"]], theta = 37)
  obj4 <- eco.mantel(dist(eco[["P"]]), dist(eco[["XY"]]), con = con2)
 
  expect_true(class(obj1) == "eco.gsa")
  expect_true(class(obj2) == "eco.gsa")
  expect_true(class(obj3) == "eco.gsa")
  expect_true(class(obj4) == "eco.gsa")
})

test_that("eco.lsa works", {
  skip_on_cran()
  con_rs <- eco.weight(eco[["XY"]], method = "knearest",  k = 4, row.sd = TRUE)  
  con_s<- eco.weight(eco[["XY"]], method = "knearest",  k = 4, self = TRUE) # self = TRUE for G*
  con <- eco.weight(eco[["XY"]], method = "knearest", k = 4) 
 
  localmoran <- eco.lsa(eco[["P"]][, 1], con_rs, method = "I", nsim = 99) 
  all.traits <- eco.lsa(eco[["P"]], con_rs, method = "I", nsim = 99)     
  all.single.traits <- eco.lsa(eco[["P"]],con_rs, method = "I", nsim = 99, multi="list")
  getis.ak <- eco.lsa(eco[["P"]][, 1], con_s, method = "G*", nsim = 99, adjust = "none")
  getis <- eco.lsa(eco[["P"]][, 1], con, method = "G", nsim = 99)
  localgeary <- eco.lsa(eco[["P"]][, 1], con_rs, method = "C", nsim = 99, adjust = "none")

  expect_true(class(localmoran) == "eco.lsa")
  expect_true(class(all.traits) == "eco.multilsa")
  expect_true(class(all.single.traits) == "eco.listlsa")
  expect_true(class(getis.ak) == "eco.lsa")
  expect_true(class(getis) == "eco.lsa")
  expect_true(class(localgeary) == "eco.lsa")

  expect_true(class(eco.plotLocal(localmoran))[1] == "plotly")
  expect_true(class(eco.plotLocal(all.traits))[1] == "plotly")
  expect_true(class(eco.plotLocal(all.single.traits))[1] == "NULL")
  expect_true(class(eco.plotLocal(getis.ak))[1] == "plotly")
  expect_true(class(eco.plotLocal(getis))[1] == "plotly")
  expect_true(class(eco.plotLocal(localgeary))[1] == "plotly")
  
})

test_that("eco.malecot works", {
  skip_on_cran()
  obj <- eco.malecot(eco=eco, method = "global", smax=10,
                     size=1000)
  obj2 <- eco.malecot(eco=eco, method = "global", smax=10,
                               size=1000, angle = 30)
  obj3 <- eco.malecot(eco=eco, method = "local",
                            type = "knearest", kmax = 5, 
                            adjust = "none")
  obj4 <- eco.malecot(eco=eco, method = "local",
                            type = "radialdist", smax = 3, 
                            adjust = "none")
 
  expect_true(class(obj) == "eco.IBD")
  expect_true(class(eco.plotCorrelog(obj))[1] == "plotly")
  expect_true(class(obj2) == "eco.IBD")
  expect_true(class(eco.plotCorrelog(obj2))[1] ==  "plotly")
  expect_true(class(obj3) == "eco.lsa")
  expect_true(class(eco.plotLocal(obj3))[1] == "plotly")
  expect_true(class(obj4) == "eco.lsa")
  expect_true(class(eco.plotLocal(obj4))[1] == "plotly")
})
