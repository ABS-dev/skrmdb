
#' Gives the Reed-Muench estimate of median effective dose (ED50)
#'
#' Data input may either be a formula and data frame, or variables may be input directly (see example).
#'
#' The Reed-Muench method estimates the median effective dose by interpolating between the two
#' doses that bracket the dose producing median response. It accumulates sums in both directions 
#' that represent the hypothetical number that would have responded or not at each dose. It does 
#' so by assuming that those that responded at a lower dose would respond at a higher dose, and 
#' those that did not respond at a higher dose would not respond at a lower dose. The ED50 is the 
#' intersection of the lines connecting the two sets of cumulative sums between the bracketing doses.  
#'
#' @title Reed-Muench estimator
# @usage ReedMuench(formula, data, warn.me = T, show = F)
#	ReedMuench(y, n, x, warn.me = T, show = F)
#' @param formula a formula of the form \code{cbind(y,n) ~ x}
#' @param data a data frame
#' @param y the number responding (response should be monotone increasing)
#' @param n the group size
#' @param x log dilution or dose
#' @param warn.me boolean to give warning message
#' @param show boolean to display extended summary
#' @return object of class \code{SKRMDB} with slots:\cr 
#' \item{ed}{Estimated median effective dose (ED50)}
#' \item{eval}{Evaluation method: 'ReedMuench'}
#' @export
#' @references Miller, Rupert G. (1973). Nonparametric estimateors of the mean tolerance in bioassay. \emph{Biometrika.} \bold{60: 535 -- 542}. \cr \cr
#' Reed LJ, Muench H (1938). A simple method of estimating fifty percent endpoints. \emph{American Journal of Hygiene.} \bold{27:493--497.} 
#' @author \link{skrmdb-package}
#' @note Many microbiology texts mistakenly present the Dragstedt-Behrens method 
#' as the Reed-Muench method.\cr
#'
#' And yes, it is absurd to have an R function for an archaic method developed 
#' to avoid complex calculations. \cr \cr
#'
#' Input data is expected to be sorted by X variable (either increasing or decreasing). 
#' Use of unsorted X variables will result in error. Y variables are evaluated 
#' for monotone, increasing or decreasing; however the estimate will be calculated 
#' in the original order regardless.
#' @seealso The function \code{\link{DragBehr}} gives the Dragstedt-Behrens estimate of ED50 \code{\link{skrmdb-class}}
#' @examples 
#' X <- data.frame(dead=c(0,3,5,8,10,10),total=rep(10,6),dil=1:6)
#' ReedMuench(cbind(dead,total) ~ dil, X)
#' # or
#' ReedMuench(y=c(0,3,5,8,10,10), n=rep(10,6), x=1:6)
#'
#' #        rm 
#' #  2.916667 
#' 
#' 
#' \dontrun{
#' 
#' ## unordered data
#' X2 <- data.frame(dead = c(10,8,5,3,0), total = rep(10, 5), dil = c(1, 3, 2, 4, 5))
#' ReedMuench(cbind(dead,total) ~ dil, X2)
#' ReedMuench(y = X2$dead, n = X2$total, x = X2$dil)
#' 
#' ## monotone decreasing (note that x variable direction is ignored!!)
#' reverse <- data.frame(dead = c(10, 8, 5, 3, 0), total = rep(10, 5), dil = 5:1)
#' ReedMuench(cbind(dead,total) ~ dil, reverse)
#' ReedMuench(y = reverse$dead, n = reverse$total, x = reverse$dil)
#' 
#' ## not monotone
#' X3 <- data.frame(dead = c(1:3, 5, 4), total = rep(10, 5), dil = 1:5 )
#' ReedMuench(cbind(dead, total) ~ dil, X3)
#' ReedMuench(y = X3$dead, n = X3$total, x = X3$dil)
#' }
ReedMuench <- function(formula=NULL, data=NULL, y, n, x, warn.me = T, show = F){
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
    #  d <-  d[i]
    if(warn.me) warning("Uneven dilution scheme")
  }
  if(length(unique(n)) == 1)
    ed <- x[i] + (d * (b[i] - a[i]))/(n[i] - y[i] + y[i + 1])
  else {
    A <- cumsum(p)
    B <- rev(cumsum(rev(1. - p)))
    ed <- x[i] + (d * (B[i] - A[i]))/(1. - p[i] + p[i + 1])
  }
  names(ed) <- 'rm'
  if(show)
    print(cbind(y, n, p, a, b, P = round(P, 2), x, d))
  #return(ed)
  return(new('skrmdb', ed = ed, eval = 'ReedMuench'))
}
