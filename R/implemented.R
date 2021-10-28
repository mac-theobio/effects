# Some of the implemented methods

## default lm
vareffobj.lm <- function(mod, ...){
	out <- list()
	out$coefficients <- coef(mod)
	out$variance_covariance <- vcov(mod)
	out$formula <- formula(mod)
	out$link <- family(mod)
	out$contrasts <- mod$contrasts
	class(out) <- "vareffobj"
	return(out)
}

get_model.mm.default <- function(mod, ...) {
	mm <- model.matrix(mod, ...)
	return(mm)
}

get_contrasts.lm <- function(mod, ...) {
	mod$contrasts
}

get_xlevels.lm <- function(mod) {
	return(mod$xlevels)
}

check_intercept.default <- function(mod, ...) {
	any(names(coefficients(mod))=="(Intercept)")
}

## glmmTMB

vareffobj.glmmTMB <- function(mod, ...) {
	out <- list()
	out$coefficients <- glmmTMB::fixef(mod)$cond
	out$variance_covariance <- vcov(mod)$cond
	out$formula <- formula(mod, fixed.only=TRUE)
	out$link <- family(mod)
	out$contrasts <- attr(getME(mod, "X"), "contrasts")
	class(out) <- "vareffobj"
	return(out)
}

get_model.mm.glmmTMB <- function(mod, ...) {
	mm <- getME(mod,"X")
	return(mm)
}

get_contrasts.glmmTMB <- function(mod, ...) {
	attr(getME(mod, "X"), "contrasts")
}

get_xlevels.glmmTMB <- function(mod) {
	xlevels <- .getXlevels(terms(mod), model.frame(mod))
	return(xlevels)
}

check_intercept.glmmTMB <- function(mod, ...) {
	any(names(fixef(mod)$cond)=="(Intercept)")	
}

## lme4
vareffobj.glmerMod <- function(mod, ...) {
	out <- list()
	out$coefficients <- lme4::fixef(mod)
	out$variance_covariance <- vcov(mod)
	out$formula <- formula(mod, fixed.only=TRUE)
	out$link <- family(mod)
	out$contrasts <- attr(getME(mod, "X"), "contrasts")
	class(out) <- "vareffobj"
	return(out)
}

get_model.mm.glmerMod <- function(mod, ...) {
	mm <- getME(mod,"X")
	return(mm)
}

get_contrasts.glmerMod <- function(mod, ...) {
	attr(getME(mod, "X"), "contrasts")
}

vareffobj.merMod <- function(mod, ...) {
	out <- list()
	out$coefficients <- lme4::fixef(mod)
	out$variance_covariance <- vcov(mod)
	out$formula <- formula(mod, fixed.only=TRUE)
	out$link <- family(mod)
	out$contrasts <- attr(getME(mod, "X"), "contrasts")
	class(out) <- "vareffobj"
	return(out)
}

get_model.mm.merMod <- function(mod, ...) {
	mm <- getME(mod,"X")
	return(mm)
}

get_contrasts.merMod <- function(mod, ...) {
	attr(getME(mod, "X"), "contrasts")
}

get_xlevels.glmerMod <- function(mod) {
	xlevels <- .getXlevels(terms(mod), model.frame(mod))
	return(xlevels)
}

get_xlevels.merMod <- function(mod) {
	xlevels <- .getXlevels(terms(mod), model.frame(mod))
	return(xlevels)
}

check_intercept.glmerMod <- function(mod, ...) {
	any(names(fixef(mod))=="(Intercept)")	
}

check_intercept.merMod <- function(mod, ...) {
	any(names(fixef(mod))=="(Intercept)")	
}

## Statistics
get_stats.glmmTMB <- function(mod, level, dfspec, ...) {
	mulz <- qnorm(1 - (1 - level)/2)
	return(mulz)
}

get_stats.glmerMod <- function(mod, level, dfspec, ...) {
	mulz <- qnorm(1 - (1 - level)/2)
	return(mulz)
}

get_stats.merMod <- function(mod, level, dfspec, ...) {
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

## sigma
get_sigma.default <- function(mod, ...) {
	stats::sigma(mod, ...)
}

get_sigma.glmmTMB <- function(mod, ...) {
	rand_comp <- glmmTMB::VarCorr(mod)$cond
	sigma <- unlist(lapply(names(rand_comp), function(x)attr(rand_comp[[x]], "stddev")))
	total_sd <- sqrt(sum(sigma^2))
	total_sd
}

get_sigma.merMod <- function(mod, ...) {
	rand_comp <- lme4::VarCorr(mod)
	sigma <- unlist(lapply(names(rand_comp), function(x)attr(rand_comp[[x]], "stddev")))
	total_sd <- sqrt(sum(sigma^2))
	total_sd
}

get_sigma.glmerMod <- function(mod, ...) {
	rand_comp <- lme4::VarCorr(mod)
	sigma <- unlist(lapply(names(rand_comp), function(x)attr(rand_comp[[x]], "stddev")))
	total_sd <- sqrt(sum(sigma^2))
	total_sd
}

## Include random effects
includeRE.default <- function(mod, ...) {
	re <- 0
	return(re)
}

includeRE.glmmTMB <- function(mod, ...) {
	ran_eff <- as.data.frame(ranef(mod))
	ran_eff <- ran_eff[, "condval", drop=FALSE]
	re <- as.vector(getME(mod, "Z") %*% as.matrix(ran_eff))
	return(re)	
}

includeRE.merMod <- function(mod, ...){
	ran_eff <- as.data.frame(ranef(mod))
	ran_eff <- ran_eff[, "condval", drop=FALSE]
	re <- as.vector(getME(mod, "Z") %*% as.matrix(ran_eff))
	return(re)	
}
		
includeRE.glmerMod <- function(mod, ...){
	ran_eff <- as.data.frame(ranef(mod))
	ran_eff <- ran_eff[, "condval", drop=FALSE]
	re <- as.vector(getME(mod, "Z") %*% as.matrix(ran_eff))
	return(re)	
}
