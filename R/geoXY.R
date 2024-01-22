#' Geodetic coordinates from latitude and longitude
#'
#' Given a set of points on the earth's surface, in latitude and longitude form, this function returns
#' the corresponding coordinates in X (east-west) and Y (north-south) distances along the surface
#' of the earth, from a specified origin.
#'
#' @param latitude,longitude Pairs of latitude and longitude values for the points to be used.
#'
#' @param lat0,lon0 The two latitude, longitude defining the origin for the desired coordinates. By
#' default, the southwest corner of the data; that is, the minimum values for the supplied latitude
#' and longitude coordinates.
#'
#' @param unit The unit to be used for the coordinates, in meters; e.g., unit=1000 causes the coordinates
#' to be in kilometers.
#'
#' @details The coordinates returned are an alternative to projecting the points onto a plane or other
#' surface. Unlike projections, there is no distortion or approximation involved, other than computational
#' error in the algorithm for geodetic distances. The coordinates are in principle exact replications
#' of the latitude and longitude, but expressed in distances along the corresponding horizontal and vertical
#' geodesics. Essentially, the coordinates are rotated to a parallel of latitude and a north-south meridian
#' through the origin, and distances returned along those lines to the latitude and longitude of the data points.
#' For purposes of data visualization, the advantage is that the points are suitable for plotting as x, y values
#' directly, regardless of the location, so long as the range of the latitude is not large compared to the surface
#' of the earth.
#'
#' The specific computation can be imagined as follows. For each pair of latitude and longitude in the data, the
#' corresponding x coordinate is the distance from the origin to a point that has the same latitude as the origin
#' and the same longitude as the data. The y coordinate is the distance from the origin to a point with the same
#' longitude as the origin and the same latitude as the data. In each case the distance is distance on the surface
#' of the earth, as computed by the algorithm in geoDist, with a sign given by the corresponding difference in
#' latitude (for the y coordinate) or longitude (for the x coordinate).
#'
#' @return A two-column matrix of coordinates, with column names "X", "Y".
#'
#' @references Vincenty,T. (1975). Direct and inverse solutions of geodesics on the ellipsoid with application of
#' nested equations. Survey Review, vol. 23(176):88-94.
#'
#' @seealso geoDist, which computes the distances.
#'
#' @examples
#' \dontrun{
#' xy <- geoXY(gpsObject1@latitude, gpsObject1@longitude, unit = 1000)
#' plot(xy[,1], xy[,2], asp = 1)
#' }
#'
#' @export

geoXY <- function(latitude, longitude,
           lat0 = min(latitude, na.rm=TRUE),
           lon0 = min(longitude, na.rm=TRUE),
           unit = 1.) {
    lat0 <- rep(lat0, length(latitude))
    lon0 <- rep(lon0, length(longitude))
    yDist <- geoDist(lat0, lon0, latitude, lon0)
    xDist <- geoDist(lat0, lon0, lat0, longitude)
    cbind(X= xDist * sign(longitude - lon0)/unit,
          Y = yDist * sign(latitude - lat0)/unit)
  }