
.checkdata <- function(data, formula){
  if(!is.null(data) & !is.null(formula)){
    A <- .checkmatrixorder(as.matrix(model.frame(formula = formula, data = data)))
    return(A)
  } else {
    return(NULL)
  }
}

.checkvars <- function(y, n, x){
  if(missing(y) | missing(n) | missing(x)){
    return(NULL)
  } else if ((length(y) != length(x)) | (length(y) != length(n)) | 
               (length(x) != length(n))){
    stop('skrmdb :: variables x, n, and y must be the same length')
  } else {
    A <- .checkmatrixorder(data.frame(y = y, n = n, x = x))
    
    return(A)  
  }
  
  
}

.checkmatrixorder <- function(A){
  if(is.null(rownames(A))){
    rownames(A) <- 1:nrow(A)
  }
  
  # check order of x variable (column 3)
  # enforce that user must order by x variable
  x <- A[, 3]
  if(!(all(x == cummax(x)) | all(x == cummin(x)))){
    stop('SpearKarb :: data must be ordered by x variable (either increasing or decreasing)')
  } else {
    # check that y is monotone
    # calculate SK estimate regardless, but let them know what type (monotone incr,
    #     monotone decr, not monotone) response is.
    y <- A[, 1]
    if(all(y == cummax(y))){
      message('skrmdb :: y is monotone increasing')
    } else if(all(y == cummin(y))){
      message('skrmdb :: y is monotone decreasing. calculations assume y to be monotone increasing!!')
    } else {
      warning('skrmdb :: y is not monotone')
    }
  }
  
  return(A)
}