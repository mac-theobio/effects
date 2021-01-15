#' Print varpred objects
#'
#' @param x varpred object.
#' @param ... additional aurguments passed to print.
#'
#' @method print varpred
#' @export
#' @export print.varpred

print.varpred <- function(x, ...){
	cat("Call:\n")
	print(x$call)
	cat("\n")
	df <- x$preds
	if(is.null(df)) df <- x
	print(df, ...)
}

#' Coerce varpred object to as.data.frame 
#'
#' @param x varpred object.
#' @param ... additional aurguments passed to as.data.frame.
#'
#' @method as.data.frame varpred
#' @export
#' @export as.data.frame.varpred

as.data.frame.varpred <- function(x, ...) {
	df <- x$preds
	if(is.null(df)) df <- x
	return(as.data.frame(df, ...))
}

#' Coerce varpred object to data.frame 
#'
#' @param x varpred object.
#' @param ... additional aurguments passed to data.frame.
#'
#' @method data.frame varpred
#' @export
#' @export data.frame.varpred

data.frame.varpred <- function(x, ...) {
	df <- x$preds
	if(is.null(df)) df <- x
	return(data.frame(df, ...))
}
