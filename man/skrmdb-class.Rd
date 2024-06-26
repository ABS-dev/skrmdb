% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-skrmdb.r
\name{skrmdb-class}
\alias{skrmdb-class}
\title{Class definition for skrmdb object}
\arguments{
\item{eval}{Evaluation method. One of "ReedMuench", "SpearKarb", or
"DragBehr". character string.}

\item{ed}{Median effective dose by eval method. numeric.}

\item{var}{Variance (for Spearman-Kärber method only). numeric.}

\item{data}{The data used to compute the effective median dose. data.frame,
numeric.}

\item{message}{Messages and warnings generated durning the calculation.
character string.}
}
\description{
The skrmdb object holds output from functions in the skrmdb package. A valid
skrmdb object is a list whicn contains the followling components:
}
