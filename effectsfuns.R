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
	, at = NULL,  dfspec = 100, vcmat = NULL) {
	betahat <- extractcoef(mod)
	mod_objs <- extract_assign(mod)
	vnames <- mod_objs$assign
	modMat <- mod_objs$modmat
	modFrame <- mod_objs$modframe
	check_vars <- vnames %in% focal
	if (!any(check_vars)) stop("Specified variable not in the model")
	focalVar <- unique(vnames[check_vars])
	
	xlevels <- .getXlevels(terms(mod), modFrame)
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
		if (inherits(mod, "glmmTMB")) {
			vc <- vcov(mod)$cond
		} else {
			vc <- vcov(mod)
		}
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
	if (inherits(mod, "glmmTMB")) return (fixef(mod)$cond)
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

#' Zero out non-focal covariances from matrix
#'
#' @param m model object
#' @param focal_vars variables appearing on the right side of the formula in the fitted model
#'
#' @export
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

#' Scale model coefficients from unscaled coefficients
#'
#' @param mod fitted model object
#' @param scale_vars variables to scale. This can include response variable
#' @param tfun transformation function. Default is scale
#'
#' @export

scalecoef <- function(mod, scale_vars, tfun = function(x){scale(x, center = TRUE, scale = TRUE)}){
	betahat <- extractcoef(mod)
	btemp <- betahat
	mod_objs <- extract_assign(mod)
	vnames <- mod_objs$assign
	check_vars <- vnames %in% scale_vars
	if (any(!check_vars)) {
		if (all(!check_vars)) stop("All specified variables not in the model.")
		out_vars <- scale_vars[!scale_vars %in% vnames]
		if (length(out_vars)){
			warning(c(paste(out_vars, collapse = ", "), " not in the fitted model. It (they) will be ignored."))
		}
	}
#	scale_vars <- scale_vars[scale_vars %in% vnames]
#	scale_coefs <- grep(paste0(scale_vars, collapse="|"), names(vnames), value = TRUE)
#	noscale_coefs <- setdiff(names(vnames), scale_coefs)
	scale_coefs <- names(vnames)[check_vars]
	noscale_coefs <- names(vnames)[!check_vars]
	modMat <- tfun(mod_objs$modmat[, scale_coefs, drop = FALSE])
	scale_sd <- c(attr(modMat, "scaled:scale"))
	scale_mu <- c(attr(modMat, "scaled:center"))
	if (length(noscale_coefs)){
		noscale_sd <- rep(1, length(noscale_coefs))
		noscale_mu <- rep(0, length(noscale_coefs))
		sds <- c(noscale_sd, scale_sd)
		names(sds) <- c(noscale_coefs, names(scale_mu))
		mus <- c(noscale_mu, scale_mu)
		names(mus) <- c(noscale_coefs, names(scale_mu))
	} else {
		sds <- scale_sd
		mus <- scale_mu
	}
	sds <- sds[names(betahat)]
	mus <- mus[names(betahat)]
	if(grepl("(Intercept)", names(betahat))){
		betahat[-1] <- betahat[-1]*sds[-1]
		betahat[1] <- btemp[1] + sum(btemp[-1]*mus[-1])
	} else {
		sds <- sds[!names(sds) %in% "(Intercept)"]
		betahat <- mus + betahat*sds
	}
	return(list(betahat))
}
