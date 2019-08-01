#' @keywords internal
"_PACKAGE"

#' Gives the Spearman-Karber estimate of the mean effective dose
#'
#' Data input may either be a formula and data frame, or variables may be input 
#' directly (see example).
#'
#' The Spearman-Karber method gives a non-parametric estimate of the mean of an tolerance 
#' distribution from its empirical distribution (EDF). The 
#' empirical PMF is derived from the EDF by differencing and the estimator is \eqn{\sum{ x f(x)}}{\sum x f(x)}.
#' If the EDF does not cover the entire support of \var{x}, \code{SpearKarb()}
#' extends it by assuming the next lower dilution would produce zero response and the next 
#' higher dilution would produce complete response.
#'
#' @title Spearman-Karber estimator
# @usage SpearKarb(formula, data)
#	SpearKarb(y, n, x)
#' @param formula a formula of the form \code{cbind(y,n) ~ x}
#' @param data a data frame
#' @param y the number responding (response should be monotone increasing)
#' @param n the group size
#' @param x log dilution or dose 
#' @return  object of class \code{sk}
#' \item{ed}{estimator of mean response}
#' \item{sk.var}{variance}
#' \item{eval}{evaluation method: 'SpearKarb'}
#' @export
#' @references Miller, Rupert G. (1973). Nonparametric estimateors of the mean tolerance in bioassay. \emph{Biometrika.} \bold{60: 535 -- 542}.\cr \cr
#'  Karber, G. (1931). Beitrag zur kollektiven Behandlung Parmakogischer Reihenversuche. \emph{Archiv fur Experimentelle Pathologie und Pharmakologie.} \bold{162: 480--487}. \cr \cr
#'  Spearman, C. (1908). The method of "right and wrong cases" ("constant stimuli") without Gauss's formulae. \emph{Brit J. of Psychology.} \bold{2: 227--242.}
#' @note Input data is expected to be sorted by X variable (either increasing or decreasing). 
#' Use of unsorted X variables will result in error. Y variables are evaluated 
#' for monotone, increasing or decreasing; however the estimate will be calculated 
#' in the original order regardless of direction.
#' @author \link{skrmdb-package}
#' @examples 
#' X <- data.frame(dead=c(0,3,5,8,10), total=rep(10,5), dil=1:5)
#' SpearKarb(cbind(dead,total) ~ dil, X)
#'
#' #        sk     sk.var 
#' #2.90000000 0.06888889 
#'
#'
#' # without zero and complete response
#' X <- data.frame(dead=c(3,5,8),total=rep(10,3),dil=2:4)
#' SpearKarb(cbind(dead,total) ~ dil, X)
#' # or
#' SpearKarb(y=c(3,5,8), n=rep(10,3), x=2:4)
#'
#' #         sk     sk.var
#' #2.90000000 0.06888889 
#' 
#' 
#' \dontrun{
#' ## unordered
#' X2 <- data.frame(dead = c(10,8,5,3,0), total = rep(10, 5), dil = c(1, 3, 2, 4, 5))
#' SpearKarb(cbind(dead,total) ~ dil, X2)
#' SpearKarb(y = X2$dead, n = X2$total, x = X2$dil)
#' 
#' ## monotone decreasing (note that x variable direction is ignored!!)
#' reverse <- data.frame(dead = c(10, 8, 5, 3, 0), total = rep(10, 5), dil = 5:1)
#' SpearKarb(cbind(dead,total) ~ dil, reverse)
#' SpearKarb(y = reverse$dead, n = reverse$total, x = reverse$dil)
#' 
#' ## not monotone
#' X3 <- data.frame(dead = c(1:3, 5, 4), total = rep(10, 5), dil = 1:5 )
#' SpearKarb(cbind(dead, total) ~ dil, X3)
#' SpearKarb(y = X3$dead, n = X3$total, x = X3$dil)
#' }
SpearKarb <- function(formula = NULL, data = NULL, y, n, x){
  A <- .checkdata(data = data, formula = formula)
  if(is.null(A)){
    A <- .checkvars(y, n, x)
	}	
  y <- A[,1]
  n <- A[,2]
  x <- A[,3]

	cum <- y/n
	p <- diff(c(0, cum, 1))
	initial <- 2 * x[1] - x[2]
	final <- 2 * x[length(x)] - x[length(x) - 1]
	D <- apply(rbind(c(x, final), c(initial, x)), 2, mean)
	SK <- sum(p * D)
	p.i <- y/n
	d <- diff(c(initial, x))
	# spacing
	sk.var <- sum((d^2 * p.i * (1 - p.i))/(n - 1))
	return(new('sk', ed = SK, sk.var = sk.var, eval = "SpearKarb"))
	#return(c(sk = SK, sk.var = sk.var))
}
