#' @title Class definition for skrmdb object
#' @description The skrmdb object holds output from functions in the skrmdb package.
#' @details
#' \describe{
#' 		\item{\code{eval}}{Evaluation method. One of 'ReedMuench', 'SpearKarb',  or 'DragBehr'. Character string.}
#' 		\item{\code{ed}}{Median effective dose by eval method. Numeric.}
#' }
#' @name skrmdb-class
#' @rdname skrmdb-class
#' @exportClass skrmdb
#' @seealso \code{\link{sk-class}}
#' @author \link{skrmdb-package}
#' @examples
#' new("skrmdb", ed = 2.906593, eval = "DragBehr")
skrmdb <- setRefClass('skrmdb', fields = list(ed = 'numeric', eval = 'character'), methods = list(
	initialize = function(ed = numeric(), eval = character()){
		initFields(ed = ed, eval = eval)
	}, 
	print = function(){
		if(length(.self$ed) == 0){
			cat("empty construct\n")
		} else {
			cat(paste('ED50 by method ', .self$eval, '\n', sep = ''))
			cat(.self$ed, '\n')
		}
	}
	))

setMethod('show', 'skrmdb', function(object){object$print()})
setMethod('print', 'skrmdb', function(x,...){x$print()})

#' @name getED
#' @title Accessor to retrieve the numeric median effective dose from a skrmdb or sk object
#' @description This is an accessor function for retrieving the numeric value of effective 
#' from a skrmdb or sk data object object generated by ReedMuench, SpearKarb or DragBehr
# @usage getED(x)
#' @param object class skrmdb or sk
#' @docType methods
#' @rdname getED-methods
#' @export
#' @seealso \code{\link{skrmdb-class}}, \code{\link{sk-class}}
#' @author \link{skrmdb-package}
#' @examples
#' ## with an object of class skrmdb
#' temp1 <- DragBehr(y=c(0,3,5,8,10,10), n=rep(10,6), x=1:6)
#' getED(temp1)
#' ##  with an object of class sk
#' X <- data.frame(dead=c(0,3,5,8,10), total=rep(10,5), dil=1:5)
#' temp2 <- SpearKarb(cbind(dead,total) ~ dil, X)
#' getED(temp2)
setGeneric(name = 'getED', def = function(object){standardGeneric('getED')})

#' @rdname getED-methods
#' @aliases getED,skrmdb-method
setMethod('getED', 'skrmdb', function(object){return(as.numeric(object$ed))})

#' @title Class definition for sk object
#' @description The sk object holds values for the Spear Karb estimator for median estimated dose. It extends the skrmdb data object with value for variance.
#' @details
#' \describe{
#' 		\item{\code{eval}}{Evaluation method. "SpearKarb". Character string.}
#' 		\item{\code{ed}}{Median effective dose by eval method. Numeric.}
#' 		\item{\code{sk.var}}{variance. Numeric.}
#' }
#' @name sk-class
#' @rdname sk-class
#' @exportClass sk
#' @seealso \code{\link{skrmdb-class}}
#' @author \link{skrmdb-package}
#' @examples
#' new('sk', sk.var = 0.06888889, ed = 2.9, eval = "SpearKarb")
sk <- setRefClass('sk', contains = 'skrmdb', fields = list(sk.var = 'numeric'),
	methods = list(
	
	initialize = function(ed = numeric(), eval = 'SpearKarb', sk.var = numeric()){
		if(eval != 'SpearKarb'){

			stop("[sk: validation] the estimator must be 'SpearKarb'")
		} else {
			initFields(ed = ed, eval = eval, sk.var = sk.var)
		}
	},
	print = function(){
		if(length(.self$ed) == 0){
			cat('Empty Construct\n')
		} else {
			cat(paste('ED50 by method ', .self$eval, '\n',sep = ''))
			cat(paste('ed: ', ed = getED(.self), '\n'))
			cat(paste('sk.var: ', sk.var = .self$sk.var, '\n'))
		}
	}
	))
					
setMethod('show', 'sk', function(object){object$print()})
setMethod('print', 'sk', function(x,...){x$print()})
