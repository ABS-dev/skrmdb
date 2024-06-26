% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-skrmdb.r
\name{getvar}
\alias{getvar}
\title{Accessor to retrieve the variance from a skrmdb object}
\usage{
getvar(x)
}
\arguments{
\item{x}{class skrmdb.}
}
\value{
the estimated variance.
}
\description{
This is an accessor function for retrieving the numeric value of the variance
from a \linkS4class{skrmdb} object object generated by \code{ReedMuench}, \code{SpearKarb},
or \code{DragBehr}.
}
\examples{
temp <- DragBehr(y = c(0, 3, 5, 8, 10, 10), n = rep(10, 6), x = 1:6)
getvar(temp)
}
