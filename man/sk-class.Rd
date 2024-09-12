\docType{class}
\name{sk-class}
\alias{sk-class}
\title{Class definition for sk object}
\description{
The sk object holds values for the Spear Karb estimator for
median estimated dose. It extends the skrmdb data object
with value for variance.
}
\details{
\describe{ \item{\code{eval}}{Evaluation method.
"SpearKarb". Character string.} \item{\code{ed}}{Median
effective dose by eval method. Numeric.}
\item{\code{sk.var}}{variance. Numeric.} }
}
\examples{
new('sk', sk.var = 0.06888889, ed = 2.9, eval = "SpearKarb")
}
\author{
Marie Vendettuoli
\email{marie.c.vendettuoli@aphis.usda.gov}
}
\seealso{
\code{\link{skrmdb-class}}
}

