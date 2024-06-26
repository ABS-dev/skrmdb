% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-skrmdb.r
\name{getdata}
\alias{getdata}
\title{Accessor to retrieve the the data used in the call to create an skrmdb object}
\usage{
getdata(x)
}
\arguments{
\item{x}{class skrmdb.}
}
\value{
a dataframe containing the sorted data.
}
\description{
This is an accessor function for retrieving the data used to compute the
effective median dose from a \linkS4class{skrmdb} object object generated by
\code{ReedMuench}, \code{SpearKarb}, or \code{DragBehr}.
}
\examples{
temp <- DragBehr(y = c(0, 3, 5, 8, 10, 10), n = rep(10, 6), x = 1:6)
getdata(temp)
}
