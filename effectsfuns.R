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
	mod_objs <- extractvnames(mod)
	vnames <- mod_objs$vnames
	termnames <- mod_objs$termnames
	mm <- mod_objs$mm
	mf <- recoverdata(mod)
	Terms <- mod_objs$Terms
	rTerms <- delete.response(Terms)
	if (any(vnames %in% focal)) {
		check_vars <- vnames %in% focal
	} else {
		check_vars <- termnames %in% focal
	}
	if (!any(check_vars)) stop(paste0(focal, " not in the model"))
	focalVar <- unique(vnames[check_vars])
	
	xlevels <- .getXlevels(Terms, model.frame(mod))
	contrs <- mod$contrasts

	if(is.null(at)) {
		vv <- varfun(mf[[focalVar]], steps)
	} else {
		vv <- at
	}
  	steps <- length(vv)
	
	rowMean <- apply(mm, 2, mean)
	modMean <- t(replicate(steps, rowMean))

	# Model matrix with progression of focal variable
	varFrame <- mf[rep(1, steps), ]
	varFrame[focalVar] <- vv
	varFrame <- model.frame(rTerms, varFrame
		, xlev = xlevels, drop.unused.levels = TRUE
	)
	newMat <- model.matrix(rTerms, varFrame
		, contr = contrs, xlev = xlevels
	)

	## Better way to do this?
	modVar <- modMean
	focal_cols <- grep(focalVar, names(vnames), value = TRUE)
#	focal_cols <- focal_cols[!grepl(":", focal_cols)]

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

	if(!identical(colnames(mm), names(betahat))){
		print(setdiff(colnames(mm), names(betahat)))
		print(setdiff(names(betahat), colnames(mm)))
		stop("Effect names do not match: check for empty factor levels?")
	}
  	pred <- modVar %*% betahat

	# (Centered) predictions for SEs
	if (isolate) {
		if(!is.null(isoValue)){
			mf[focalVar] <- 0*mf[focalVar]+isoValue
			mm <- model.matrix(rTerms, mf
				, contr = contrs, xlev = xlevels
			)
			rowMean <- apply(mm, 2, mean)
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
  	names(df)[[1]] <- "xvar"
	df <- df[order(df[["xvar"]]), ]
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

#' Extract variable names from model object esp
#'
#' @param mod fitted model object
#'
#' @export 
#'

extractvnames <- function(mod){
	mat <- model.matrix(mod)
	coefnames <- colnames(mat)
	Terms <- terms(mod)
	vnames <- all.vars(delete.response(Terms)) 
	termnames <- attr(Terms, "term.labels") # Model based names
	termnames <- gsub(" ", "", termnames)
	assign <- attr(mat, "assign")
	if(any(assign==0)) {
		vnames <- c("(Intercept)", vnames)
		termnames <- c("(Intercept)", termnames)
		assign <- assign + 1
	}
	vnames <- setNames(vnames[assign], coefnames)
	termnames <- setNames(termnames[assign], coefnames)
	# In case there are interactions
	vnames[is.na(vnames)] <- termnames[is.na(vnames)]
	return(list(vnames = vnames, termnames = termnames, Terms = Terms, mm = mat))
}

#' Zero out non-focal covariances from matrix
#'
#' @param m model object
#' @param focal_vars variables appearing on the right side of the formula in the fitted model
#'
#' @export
zero_vcov <- function(m, focal_vars, complete) {
	assign <- extractvnames(m)$vnames
	check_vars <- assign %in% focal_vars
	if (!any(check_vars)) stop("Specified variable(s) not in the model")
	focal_vars <- names(assign)[check_vars]
	v <- stats::vcov(m)
	if(inherits(m, "glmmTMB")){
		v <- v$cond
	}
	focal_var <- v[focal_vars,focal_vars]
	v[] <- 0 ## set all to zero, preserving dims/names
	v[focal_vars, focal_vars] <- focal_var
	return(v)
}

#' Recover data from the data from the model 
#'
#' @param mod fitted model
#' @param envir data environment 
#'
recoverdata <- function(mod, envir = environment(formula(mod)), ...) {
	f <- formula(mod)
	data <- eval(mod$call$data, envir)
	df <- if(is.null(data)) eval(call("model.frame", f), envir) 
	else eval(call("model.frame", data = data, drop.unused.levels = TRUE), envir)
	return(df)
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
	mod_objs <- extractvnames(mod)
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
	mm <- tfun(mod_objs$modmat[, scale_coefs, drop = FALSE])
	scale_sd <- c(attr(mm, "scaled:scale"))
	scale_mu <- c(attr(mm, "scaled:center"))
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

#' Plot marginal predictions 

plotpreds <- function(df, xlabs = "Predictor value", pos = 0.5){
	pos <- position_dodge(pos)
	if (is.null(df$model)){
		p1 <- (ggplot(df, aes(x = xvar, y = fit), colour="black", alpha = 0.2)
			+ guides(fill = FALSE)
			+ theme(legend.position = "none")
		)
	} else {
		p1 <- (ggplot(df, aes(x = xvar, y = fit, colour = model), alpha = 0.2)
			+ guides(fill = FALSE)
			+ theme(legend.position = "right")
		)
	}
	p1 <- p1+ labs(x = xlabs, y = "Prediction")
	if (class(df[["xvar"]]) %in% c("numeric", "integer")) {
		p2 <- (p1
			+ geom_line()
			+ geom_line(aes(y = lwr), lty = 2)
			+ geom_line(aes(y = upr), lty = 2)
			+ scale_colour_viridis_d(option = "plasma")
		)
	} else {
		p2 <- (p1 
			+ geom_point(position = pos, size = 0.6, colour="black")
			+ geom_pointrange(aes(ymin = lwr, ymax = upr), position = pos, colour = "black")
		)
	}
	return(p2)
}

#' Varpred theme

varpredtheme <- function(){
   theme_set(theme_bw() +
      theme(panel.spacing = grid::unit(0,"lines")
      	, plot.title = element_text(hjust = 0.5)
			, legend.position = "bottom"
			, axis.ticks.y = element_blank()
			, axis.text.x = element_text(size = 12)
			, axis.text.y = element_text(size = 12)
			, axis.title.x = element_text(size = 12)
			, axis.title.y = element_text(size = 12)
			, legend.title = element_text(size = 13, hjust = 0.5)
			, legend.text = element_text(size = 13)
			, panel.grid.major = element_blank()
			, legend.key.size = unit(0.8, "cm")
			, legend.key = element_rect(fill = "white")
			, panel.spacing.y = unit(0.3, "lines")
			, panel.spacing.x = unit(1, "lines")
			, strip.background = element_blank()
			, panel.border = element_rect(colour = "grey"
				, fill = NA
				, size = 0.8
			)
			, strip.text.x = element_text(size = 11
				, colour = "black"
				, face = "bold"
			)
      )
   )
}

