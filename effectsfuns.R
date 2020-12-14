#' Compute marginal means
#'
#' @param mod
#' @param focal predictor
#' @param isolate logical
#' @param isoValue default NULL
#' @param level default 0.05
#' @param steps default 101
#' @param dfspec default 100
#' @param at default NULL
#' @param vcmat default NULL
#'
#' @importFrom stats model.frame model.matrix vcov
#'
#' @export
#'
varpred <- function(mod, focal, isolate = FALSE
	, isoValue = NULL, level = 0.95, steps = 101
	, at = NULL, vcmat = NULL) {
	betahat <- extractcoef(mod)
	mod_objs <- extract_assign(mod)
	vnames <- mod_objs$assign
	modMat <- mod_objs$modmat
	modFrame <- mod_objs$modframe
	check_vars <- vnames %in% focal
	if (!any(check_vars)) stop("Specified variable not in the model")
	focalVar <- unique(vnames[check_vars])
	
	xlevels <- mod$xlevels
	contrs <- mod$contrasts

	if(is.null(at)) {
		vv <- varfun(modFrame[[focalVar]], steps)
	} else {
		vv <- at
	}
  	steps <- length(vv)
	
	rowMean <- apply(modMat, 2, mean)
	modMean <- t(replicate(steps, rowMean))

	# Model matrix with progression of focal variable
	varFrame <- modFrame[rep(1, steps), ]
	varFrame[focalVar] <- vv
	modTerms <- delete.response(terms(mod))
	varFrame <- model.frame(modTerms, varFrame
		, xlev = xlevels, drop.unused.levels = TRUE
	)
	newMat <- model.matrix(modTerms, varFrame
		, contr = contrs, xlev = xlevels
	)

	## Better way to do this?
	modVar <- modMean
	focal_cols <- grep(focalVar, names(vnames), value = TRUE)
	modVar[, focal_cols] <- newMat[, focal_cols]
  
  	if (is.null(vcmat)){
		vc <- vcov(mod)
		if (inherits(mod, "clmm")){
			f <- c(names(mod$alpha)[[1]], names(mod$beta))
			vc <- vc[f, f]
		}
	} else {
		vc <- vcmat
	}

	if(!identical(colnames(modMat), names(betahat))){
		print(setdiff(colnames(modMat), names(betahat)))
		print(setdiff(names(betahat), colnames(modMat)))
		stop("Effect names do not match: check for empty factor levels?")
	}
  	pred <- modVar %*% betahat

	# (Centered) predictions for SEs
	if (isolate) {
		if(!is.null(isoValue)){
			modFrame[focalVar] <- 0*modFrame[focalVar]+isoValue
			modMat <- model.matrix(modTerms, modFrame
				, contr = contrs, xlev = xlevels
			)
			rowMean <- apply(modMat, 2, mean)
			modMean <- t(replicate(steps, rowMean))

		}
		modVar <- modVar - modMean
	}

	pse_var <- sqrt(diag(modVar %*% tcrossprod(data.matrix(vc), modVar)))
	
	# Stats
	df <- ifelse(
		grepl("df.residual", paste(names(mod), collapse=""))
		, mod$df.residual, dfspec
	)
	mult <- qt(1 - (1 - level)/2, df)

	df <- data.frame(var = vv
		, fit = pred
		, lwr = pred - mult*pse_var
		, upr = pred + mult*pse_var
	)
  	names(df)[[1]] <- focal
	df <- df[order(df[[focal]]), ]
	return(df)
}

#' Extract model coefficients
#'
#' @param mod model object
#' @noRd
#' @keywords internal
#'

extractcoef <- function(mod){
	if (inherits(mod, "lm")) return (coef(mod))
	if (inherits(mod, "mer")) return (fixef(mod))
	if (inherits(mod, "glmerMod")) return (fixef(mod))
	if (inherits(mod, "glmmTMB")) return (fixef(mod))
	if (inherits(mod, "clmm")) {
		ef <- c(0, mod$beta)
		names(ef) <- c("(Intercept)", names(mod$beta))
		return (ef)
	} else {
		stop("Don't recognize model type")
	}
}

#' Variable levels or sequence
#'
#' @param varcol focal variable or column
#' @param steps
#' @noRd
#' @keywords internal

varfun <- function(vcol, steps){
	if(is.numeric(vcol)){
		if(is.null(steps)){return(sort(unique(vcol)))}
		return(seq(min(vcol), max(vcol), length.out = steps))
	}
	return(unique(vcol))
}

#' Extract assign for model object esp. for glmmTMB
#'
#' @param mod fitted model object
#' @param intercept logical. Set to \code{TRUE} if fitted model had intercept term, else \code{FALSE}.
#'
#' @export 
#'
## Extract assign for model object esp. for glmmTMB
extract_assign <- function(mod){
	mat <- model.matrix(mod)
	modframe <- model.frame(mod, drop.unused.levels = TRUE)
	xnames <- colnames(mat)
	Terms <- terms(mod)
	var_labs <- attr(Terms, "term.labels")
	assign <- attr(mat, "assign")
	if(any(assign==0)) {
		var_labs <- c("(Intercept)", var_labs)
		assign <- assign + 1
	}
	assign <- setNames(var_labs[assign], xnames)
	return(list(assign = assign, modmat = mat, modframe = modframe))
}

## zero out non-focal covariances from matrix
zero_vcov <- function(m, focal_vars) {
	assign <- extract_assign(m)$assign
	check_vars <- assign %in% focal_vars
	if (!any(check_vars)) stop("Specified variable(s) not in the model")
	focal_vars <- names(assign)[check_vars]
	v <- vcov(m)
	if(inherits(m, "glmmTMB")){
		v <- v$cond
	}
	focal_var <- v[focal_vars,focal_vars]
	v[] <- 0 ## set all to zero, preserving dims/names
	v[focal_vars, focal_vars] <- focal_var
	return(v)
}


