\docType{class}
\name{skrmdb-class}
\alias{skrmdb-class}
\title{Class definition for skrmdb object}
\description{
The skrmdb object holds output from functions in the skrmdb
package.
}
\details{
\describe{ \item{\code{eval}}{Evaluation method. One of
'ReedMeunch', 'SpearKarb', or 'DragBehr'. Character
string.} \item{\code{ed}}{Median effective dose by eval
method. Numeric.} }
}
\examples{
new("skrmdb", ed = 2.906593, eval = "DragBehr")
}
\author{
Marie Vendettuoli
\email{marie.c.vendettuoli@aphis.usda.gov}
}
\seealso{
\code{\link{sk-class}}
}

