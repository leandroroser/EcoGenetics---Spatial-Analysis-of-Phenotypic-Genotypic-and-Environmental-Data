% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eco.bearing.R
\name{eco.bearing}
\alias{eco.bearing}
\title{Angular Spatial Weights}
\usage{
eco.bearing(con, theta, XY = NULL, latlon = FALSE)
}
\arguments{
\item{con}{an eco.weight or eco.lagweight object}

\item{theta}{reference angle in degrees, between 0 and 180,
counterclockwise, with 0 representing the positive x axis,
90 representing the positive y axis, 180 representing 
the negative x axis. Note that angles 0 and 180 yield identical results.}

\item{XY}{Matrix/data frame with projected coordinates. Default NULL.}

\item{latlon}{Are the coordinates in decimal degrees format? Defalut FALSE. If TRUE,
the coordinates must be in the XY matrix/data frame with longitude in the first
column and latitude in the second. The position is projected onto a plane in
meters with the function \code{\link{geoXY}}.}
}
\value{
An object of class eco.weight with a bearing weights matrix

\strong{ACCESS TO THE SLOTS}
The content of the slots can be accessed 
with the corresponding accessors, using
the generic notation of EcoGenetics 
(<ecoslot.> + <name of the slot> + <name of the object>).
See help("EcoGenetics accessors") and the Examples
for the function eco.weight and eco.lagweight.
}
\description{
Construction of angular spatial weights
}
\details{
This program computes an angular weights object (AW) 
(or a list of AW). If a weights object is passed as argument
("con") the program computes an AW with this element.
If XY is passed, the program first computes a matrix of N x N, where N is the
number of rows in XY, and then uses the matrix as input to compute the AW.
Each element in the weights matrices is then weighted by the 
 squared cosine of the angle formed with the x positive axis by a line connecting 
the pair of points. Note that this method assumes that
the distances in the eco.weight or eco.lagweight object
are projected as great-circle distances (for example, 
using latlon = TRUE during weights construction or UTM coordinates for 
elements passed with "con", or latlon set TRUE in this function for 
a coordinates element passed with XY).

Note also that when angular weights are constructed for XY coordinates,
the output consists of a weights object with values bounded between 0 and 1,
being 1 if the if the direction pointed by the vector V connecting the elements
i, j in the matrix points in the same direction of the reference vector R (with 
and angle theta with the positive x axis), and 0 if V is perpendicular to R.
}
\examples{

\dontrun{

data(eco3)

"circle" method

con <- eco.weight(eco3[["XY"]], method = "circle", d1 = 0, d2 = 500)
bearing_con <- eco.bearing(con, 90)

W_list <- eco.lagweight(eco[["XY"]]) 
bearing_W_list <- eco.bearing(W_list, 90)

}

}
\references{
Rosenberg, M. 2000. The bearing correlogram: a new method 
 of analyzing directional spatial autocorrelation. 
 Geographical Analysis, 32: 267-278.
}
\author{
Leandro Roser \email{learoser@gmail.com}
}
