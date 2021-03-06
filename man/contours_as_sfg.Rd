% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{contours_as_sfg}
\alias{contours_as_sfg}
\title{Convert output from the \code{\link[grDevices:contourLines]{grDevices::contourLines()}} function to an \link[sf:sfc]{sf::sfc} object}
\usage{
contours_as_sfg(lst, vals, crs = 4326)
}
\arguments{
\item{lst}{List as returned from the \code{\link[grDevices:contourLines]{grDevices::contourLines()}} function}

\item{vals}{A vector of depth values to use. Each will be filtered and the returned list will
hakev one element for each depth}

\item{crs}{Coordinate reference system represented by an integer}
}
\value{
An \link[sf:sfc]{sf::sfc} object
}
\description{
Convert output from the \code{\link[grDevices:contourLines]{grDevices::contourLines()}} function to an \link[sf:sfc]{sf::sfc} object
}
\details{
Creates a list of \code{LINESTRING} objects which represent contours on the BC coast.
The data are transformed to NAD83 for congruence with the rest of this package.
}
