# Some of the implemented methods

## default lm
vareffobj.lm <- function(mod, ...){
	out <- list()
	out$coefficients <- coef(mod)
	out$variance_covariance <- vcov(mod)
	out$formula <- formula(mod)
	out$link <- family(mod)
	class(out) <- "vareffobj"
	return(out)
}

get_xlevels.lm <- function(mod) {
	return(mod$xlevels)
}

## glmmTMB

vareffobj.glmmTMB <- function(mod, ...) {
	out <- list()
	out$coefficients <- glmmTMB::fixef(mod)$cond
	out$variance_covariance <- vcov(mod)$cond
	out$formula <- formula(mod, fixed.only=TRUE)
	out$link <- family(mod)
	class(out) <- "vareffobj"
	return(out)
}

get_xlevels.glmmTMB <- function(mod) {
	xlevels <- .getXlevels(terms(mod), model.frame(mod))
	return(xlevels)
}

## Statistics
get_stats.glmmTMB <- function(mod, level, dfspec, ...) {
	mulz <- qnorm(1 - (1 - level)/2)
	return(mulz)
}

## vcov
vcov.vareffobj <- function(x, ...)x$variance_covariance

## Statistics
get_stats.default <- function(mod, level, dfspec, ...) {
	df <- ifelse(
		grepl("df.residual", paste(names(mod), collapse=""))
		, mod$df.residual, dfspec
	)
	mult <- qt(1 - (1 - level)/2, df)
	return(mult)
}

