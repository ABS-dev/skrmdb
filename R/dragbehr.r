
#' Gives the Dragstedt-Behrens estimate of median effective dose (ED50)
#'
#' Data input may either be a formula and data frame, or variables may be input directly (see example).  
#' 
#' The Dragstedt-Behrens method estimates the median effective dose by interpolating between the two 
#' doses that bracket the dose producing median response. It accumulates sums in both directions by
#' assuming that those that responded at a lower dose would respond at a higher dose, and those that 
#' did not respond at a higher dose would not respond at a lower dose. The hypothetical fraction that 
#' would have responded at a particular dose is estimated from the cumulative sums at that dose. The 
#' ED50 is estimated by interpolation on the line that connects the hypothetical fractions of the 
#' bracketing doses.	
#' 
#' @title Dragstedt-Behrens estimator
# @usage DragBehr(formula, data)
# DragBehr(y, n, x)
#' @param formula a formula of the form \code{cbind(y,n) ~ x}
#' @param data a data frame
#' @param y the number responding (response should be monotone increasing)
#' @param n the group size
#' @param x log dilution or dose
#' @param warn.me boolean to give warning message
#' @param show boolean to display extended summary
#' @return object of \code{skrmdb} class with slots: \cr 
#' \item{ed}{Estimated median effective dose (ED50)}
#' \item{eval}{Evaluation method: 'DragBehr'}
#' @note Many microbiology texts mistakenly present the Dragstedt-Behrens method 
#' as the Reed-Muench method.\cr
#'
#' And yes, it is absurd to have an R function for an archaic method developed 
#' to avoid complex calculations. \cr\cr
#' 
#' 
#' Input data is expected to be sorted by x (either increasing or decreasing). 
#' Use of unsorted data will result in error.
#' @seealso The function \link{ReedMuench} gives the Reed-Muench estimate of ED50, \code{\link{skrmdb-class}}
#' @export
#' @references Miller, Rupert G. (1973). Nonparametric estimateors of the mean tolerance in bioassay. \emph{Biometrika.} \bold{60: 535 - 542}. \cr \cr
#' Behrens, B. (1929) Zur Auswertung der Digitalisblatter im Froschversuch. \emph{Arkiv fur Experimentelle Pathologie und Pharmakologie.} \bold{140: 237-256}. \cr \cr
#' Dragstedt, CA., Lang, VF. (1928). Respiratory Stimulants in acute poisoning in rabbits. \emph{J. of Pharmacology} \bold{32: 215--222}.
#' @author David Siev \email{David.Siev@@aphis.usda.gov}
#' @examples 
#' X <- data.frame(dead=c(0,3,5,8,10,10),total=rep(10,6),dil=1:6)	
#' DragBehr(cbind(dead,total) ~ dil, X)
#'	# or	
#' DragBehr(y=c(0,3,5,8,10,10), n=rep(10,6), x=1:6)
#'
#' #           db
#' #     2.906593
#' 
#' 
#' #' \dontrun{
#' 
#' ## unordered data
#' X2 <- data.frame(dead = c(10,8,5,3,0), total = rep(10, 5), dil = c(1, 3, 2, 4, 5))
#' DragBehr(cbind(dead,total) ~ dil, X2)
#' DragBehr(y = X2$dead, n = X2$total, x = X2$dil)
#' 
#' ## monotone decreasing (note that x variable direction is ignored!!)
#' reverse <- data.frame(dead = c(10, 8, 5, 3, 0), total = rep(10, 5), dil = 5:1)
#' DragBehr(cbind(dead,total) ~ dil, reverse)
#' DragBehr(y = reverse$dead, n = reverse$total, x = reverse$dil)
#' 
#' ## not monotone
#' X3 <- data.frame(dead = c(1:3, 5, 4), total = rep(10, 5), dil = 1:5 )
#' DragBehr(cbind(dead, total) ~ dil, X3)
#' DragBehr(y = X3$dead, n = X3$total, x = X3$dil)
#' }

DragBehr <- function(formula=NULL, data=NULL, y, n, x, warn.me = T, show = F){
  A <- .checkdata(data = data, formula = formula)
  if(is.null(A)){
    A <- .checkvars(y, n, x)
  }	
  y <- A[,1]
  n <- A[,2]
  x <- A[,3]
  
  d <- unique(diff(zapsmall(x)))
  p <- y/n
  a <- cumsum(y)
  b <- rev(cumsum(rev(n - y)))
  h <- length(y)
  P <- a/(a + b)
  i <- max(c(1:h)[P <= 0.5])
  if(length(d) > 1) {
    d <- x[i + 1] - x[i]
    if(warn.me)
      warning("Uneven dilution scheme")
  }
  ed <- x[i] + ((d * (1/2 - P[i]))/(P[i + 1] - P[i]))
  names(ed) <- 'db'
  if(show)
    print(cbind(y, n, p, a, b, P = round(P, 2), x, d))
  return(new('skrmdb', ed = ed, eval = 'DragBehr'))
}
