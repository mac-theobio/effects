#' Conditional and marginal predictions
#'
#' Computes conditional and marginal prediction for supported simple and generalized linear models.
#'
#' @details
#' The main distinction between the two predictions lies on how the standard errors (SEs) are computed. In conditional predictions, non-focal predictors are conditioned (by some meaningful averaging) and standard errors computed using "full" variance-covariance matrix. However, in marginal predictions, the uncertainities relating to non-focal predictors are isolated (removed) using some meaningful way -- discussed somewhere else.
#'
#' @param mod fitted model object. See details for supported class of models.
#' @param focal_predictors a character vector of one or more predictors. For models with interaction, the marginal predictions are obtained by specifying the corresponding predictors together. For example \code{~x1*x2} is specified as \code{c("x1", "x2")} to obtain the marginal prediction for \code{x1} with respect to \code{x2}. If no interactions are present in the model, specifying more than one predictors compares predictions between the predictors.
#' @param x.var a character specifying the predictor to define the x variable (horizontal axis on the plot). The default is \code{NULL}, of which the first predictor in \code{focal_predictors} is used.
#' @param type a character specifying the desired prediction. \code{type = "response"} applies inverse transformation, if exists. \code{type = "link"} requests the results as a linear predictor.
#' @param isolate logical. If \code{TRUE}, the \code{SEs} are centered around the mean value of \code{x.var}. By default, it is the deviation of the predictor value from its mean but other values can be specified through \code{isolate.value}.
#' @param isolate.value numeric (default \code{isolate.value = NULL}). If \code{isolate = TRUE}, otherwise ignored, is the deviation from the mean \code{x.var}. If \code{NULL} and \code{isolate = TRUE}, the resulting predictions are equivalent to that of model fitted with scaled predictor.
#' @param level desired confidence interval for computing marginal predictions. Default is \code{0.95}.
#' @param steps number of points to evaluate numerical predictors in \code{focal_predictors}. The default is \code{100}.
#' @param at default \code{NULL}. Otherwise, is a named \code{list} specifying points to evaluate \code{focal_predictors}. The names in the list should match the names used in \code{focal_predictors}.
#' @param dfspec default \code{100}. Specified degrees of freedom for model which do not return \code{df}. This is used in computation of confidence intervals.
#' @param vcov. a function or a matrix. If a function, it is used to compute the variance-covariance matrix of the model coefficients. The function should take model as it's first (or maybe only) argument. A matrix of variance-covariance matrix of the estimated coefficient can also be used. Otherwise \code{vcov(mod)} is used internally. Specifying \code{vcov.} Is important when "zeroed-out" predictions are required. However, with this approach, the predictors should be centered. {isolate=TRUE} marginalizes without requiring zeroing-out. See examples.
#' @param internal logical. If \code{TRUE}, the entries of the non-focal predictor (see x.var) in the variance-covariance matrix are internally zeroed-out using \code{\link[vareffects]{zero_vcov}}. Default is \code{FALSE}.
#' @param avefun the averaging scheme (function) to be used in conditioning non-focal predictors. Default is \code{mean}.
#' @param zero_out_interaction logical. If \code{TRUE} the uncertainty as a result of interaction terms are removed (set to zero) when \code{ignored if isolate = FALSE}. Only main effect predictions are computed.
#' @param which.interaction if \code{TRUE}, condition interactions the same Effect package 
#' @param returnall logical. If \code{TRUE}, all other named computed quantities are also returned. Otherwise, only predictions are returned. 
#'
#' @seealso
#'\code{\link[vareffects]{plot.vareffects}}
#'
#' @examples
#'
#' # Set theme for ggplot. Comment out if not needed
#' varefftheme()
#' set.seed(101)
#' N <- 100
#' x1_min <- 1
#' x1_max <- 9
#' b0 <- 0.3
#' b1 <- 0.1
#' b2 <- -0.6
#' b3 <- 0.01
#' x2_levels <- factor(c("A", "B", "D"))
#' df <- expand.grid(x1u = runif(n=N, min=x1_min, max=x1_max)
#' 	, x2u = x2_levels
#' )
#' X <- model.matrix(~x1u + x2u, df)
#' betas <- c(b0, b1, b2, b3)
#' df$y <- rnorm(nrow(df), mean= X %*% betas, sd=1)
#' df2 <- df
#' df <- transform(df
#' 	, x1c = drop(scale(x1u, scale=FALSE))
#' )
#' head(df)
#'
#' # Unscaled model
#' m1u <- lm(y ~ x1u + x2u, df)
#' # Conditional prediction of x1u
#' pred1u <- varpred(m1u, "x1u")
#' plot(pred1u)
#'
#' # Scaled model (x1 centered = x1 - mean(x1))
#' m1c <- lm(y ~ x1c + x2u, df)
#' # Conditional prediction of x1u
#' pred1c <- varpred(m1c, "x1c")
#' plot(pred1c)
#'
#' # Marginal predictions
#' # We can get marginal predictions from unscaled model similar to m1c
#' # Using zero_vcov by specifying vcov.
#' vv <- zero_vcov(m1c, "x1c")
#' pred2c <- varpred(m1c, "x1c", vcov. = vv)
#' plot(pred2c)
#'
#' # Using mean centering (isolate)
#' pred3c <- varpred(m1u, "x1u", isolate = TRUE)
#' plot(pred3c)
#' all.equal(pred2c$pred[,-1], pred3c$pred[,-1], check.attributes = FALSE)
#'
#' # Compare across groups
#' pred4c <- varpred(m1c, c("x1c", "x2u"), x.var = "x1c", isolate = TRUE, zero_out_interaction = TRUE)
#' plot(pred4c)
#' 
#' @importFrom stats model.frame model.matrix vcov .getXlevels as.formula coef coefficients delete.response formula qt setNames terms

#'
#' @export
#'

varpred <- function(mod, focal_predictors, x.var = NULL
	, type = c("response", "link"), isolate = FALSE, isolate.value = NULL, level = 0.95
	, steps = 101, at = list(),  dfspec = 100, vcov. = NULL, internal = FALSE, avefun = mean
	, zero_out_interaction = FALSE, which.interaction = c("emmeans", "effects")
	, pop.ave = c("none", "quantile", "population"), include.re = FALSE
	, bias.adjust = FALSE, sigma = NULL, modelname = NULL, returnall = FALSE, ...) {
	which.interaction <- match.arg(which.interaction)
	pop.ave <- match.arg(pop.ave)
	vareff_objects <- vareffobj(mod)
	betahat <- coef(vareff_objects)
	mod_names <- get_vnames(mod)
	vnames <- mod_names$vnames
	termnames <- mod_names$termnames
	Terms <- mod_names$Terms
	focal.predictors <- NULL
	rTerms <- delete.response(Terms)
	for (focal in focal_predictors){
		if (any(vnames %in% focal)) {
			check_vars <- vnames %in% focal
		} else {
			check_vars <- termnames %in% focal
		}
		if (!any(check_vars)) stop(paste0(focal, " not in the model"))
		focal.predictors[[focal]] <- unique(vnames[check_vars])
	}
	
	if (!is.null(x.var) & !any(focal.predictors %in% x.var) & length(focal.predictors)>1L) 
		stop(paste0(x.var, " not in ", focal.predictors))

#	formula.rhs <- insight::find_formula(mod)$conditional #formula(mod)[c(1, 3)]
	model_frame_objs <- clean_model(focal.predictors = focal.predictors, mod = mod
		, xlevels = at, default.levels = NULL, formula.rhs = rTerms, steps = steps
		, x.var = x.var, typical = avefun, vnames = vnames
		, which.interaction = which.interaction, pop.ave = pop.ave
	)
	formula.rhs <- formula(vareff_objects)[c(1,3)]
	excluded.predictors <- model_frame_objs$excluded.predictors
	predict.data <- model_frame_objs$predict.data
	factor.levels <- model_frame_objs$factor.levels
	factor.cols <- model_frame_objs$factor.cols
	n.focal <- model_frame_objs$n.focal
	x <- model_frame_objs$x
	X.mod <- model_frame_objs$X.mod
	cnames <- model_frame_objs$cnames
	X <- model_frame_objs$X
	x.var <- model_frame_objs$x.var
	mf <- model.frame(rTerms, predict.data, xlev = factor.levels, na.action=NULL)
	typical <- avefun
	mod.matrix.all <- model.matrix(mod)
	mod.matrix <- model.matrix(formula.rhs, data = mf, contrasts.arg = vareff_objects$contrasts)
	mm <- get_model_matrix(mod, mod.matrix, mod.matrix.all, X.mod
		, factor.cols, cnames, focal.predictors, excluded.predictors
		, typical, apply.typical.to.factors = TRUE
	)
	
	if (is.null(x.var) & n.focal>1L) {
		x.var <- focal.predictors[[2]]
		message(paste0("x.var was not specified, ", x.var, " is used instead."))
	} else if (is.null(x.var)) {
		x.var <- focal.predictors[[1]]
	}

	check <- !termnames %in% x.var
	terms_non.focal <- termnames[check]
	terms_focal <- termnames[!check]
	betahat_non.focal <- betahat[check]
	betahat_focal <- betahat[!check]
	# Stats
	mult <- get_stats(mod, level, dfspec)
	
	if (pop.ave=="quantile" || pop.ave=="population") {
		quant <- seq(0, 1, length.out=steps)
		mf_x.var <- mf[[x.var]]
		col_mean <- apply(mm, 2, typical)
		mm2 <- mm
		if (isolate) {
			col_mean[check] <- 0
			mm2[] <- 0
			mm2[, terms_focal] <- mm[, terms_focal]
		}
		if (pop.ave=="population" && !is.factor(mf_x.var)) {
			mm_x.var <- as.vector(mm[, terms_focal])
			mm_x.var <- as.vector(quantile(mm_x.var, quant))
			if (isolate) {
				mm2 <- mm2[1:steps, , drop=FALSE]
				mm2[, terms_focal] <- mm_x.var
			}
		}
		if (isolate || is.factor(mf_x.var)) {
			pse_var <- mult*get_sderror(mod=mod, vcov.=vcov., mm=mm2, col_mean=col_mean, isolate=isolate
				, isolate.value=isolate.value, vareff_objects=vareff_objects, x.var=x.var
				, typical=typical, formula.rhs=formula.rhs, zero_out_interaction=zero_out_interaction
			)
		}
		if (include.re) {
			re <- includeRE(mod)
		} else {
			re <- 0
		}
		if (is.factor(mf_x.var)) {
			pred <- mm %*% betahat + re
		} else {
			if (length(re)>1) {
				if (pop.ave=="quantile") {
					re <- as.vector(quantile(re, quant))
				} 
			}
			mm_non.focal <- mm[, terms_non.focal, drop=FALSE]
			pred_non.focal <- mm_non.focal %*% betahat_non.focal
			if (pop.ave=="quantile") {
				mm_x.var <- as.vector(mm[, terms_focal])
			}
			est <- lapply(1:length(mm_x.var), function(i){
				if (!isolate) {
					mm2[, terms_focal] <- mm_x.var[[i]]
					pse_var <- mult*get_sderror(mod=mod, vcov.=vcov., mm=mm2
						, col_mean=col_mean, isolate=isolate, isolate.value=isolate.value
						, vareff_objects=vareff_objects, x.var=x.var, typical=typical
						, formula.rhs=formula.rhs, zero_out_interaction=zero_out_interaction
					)
				} else {
					pse_var <- pse_var[[i]]
				}
				if (length(re)>1 && pop.ave=="quantile") {
					out <- lapply(re, function(j){
						out <- as.vector(c(mm_x.var[[i]] %*% betahat_focal) + pred_non.focal + j)
						return(data.frame(pred=out, pse_var=pse_var))
					})
					out <- do.call("rbind", out)
					pred <- out[["pred"]]
					pse_var <- out[["pse_var"]]
				} else {
					pred <- as.vector(c(mm_x.var[[i]] %*% betahat_focal) + pred_non.focal + re)
				}
				lwr <- pred - pse_var
				upr <- pred + pse_var
				out <- data.frame(x=mm_x.var[[i]], pred = pred, lwr=lwr, upr=upr, pse_var=pse_var)
				return(out)
			})
			est <- do.call("rbind", est)
			pred <- est[["pred"]]
			lwr <- est[["lwr"]]
			upr <- est[["upr"]]
			pse_var <- est[["pse_var"]]
			predict.data <- data.frame(...xxx=est[["x"]])
			colnames(predict.data) <- x.var
		}
	} else if (pop.ave=="none") {
		if (which.interaction=="emmeans") {
			ff_cols <- factor.cols[factor.cols==TRUE]
			if (length(focal.predictors)>1L) {
				ncats <- 0
				for (f in focal.predictors) {
					name <- names(grep(f, vnames, value=TRUE))
					if (!any(grepl("\\:", name)) & length(name)>1L) {
						ncats <- ncats + length(name)
					}	
				}
				ncats <- ifelse(ncats==0, 1, ncats)
				ff_cols <- factor.cols[factor.cols==TRUE]
				for (name in names(ff_cols)) {
					components <- unlist(strsplit(name, ':'))
					component <- components[1]
					if (length(components) > 1) {
						if (any(vnames[names(vnames) %in% name] %in% focal.predictors)) ncats <- 1
						mm[,name] <- apply(mm[,c(component, name)], 1, prod) * ncats
					}
				}
			}
		}
		col_mean <- apply(mod.matrix.all, 2, typical)
		pse_var <- mult*get_sderror(mod=mod, vcov.=vcov., mm=mm, col_mean=col_mean, isolate=isolate
			, isolate.value=isolate.value, vareff_objects=vareff_objects, x.var=x.var
			, typical=typical, formula.rhs=formula.rhs, zero_out_interaction=zero_out_interaction
		)
		pred <- as.vector(mm %*% betahat)
		lwr <- pred - pse_var
		upr <- pred + pse_var
	}
	
	# For bias-adjustment: coppied directry from emmeans
	## Currently, we just use a 2nd-order approx for everybody:
	## E(h(nu + E))  ~=  h(nu) + 0.5*h"(nu)*var(E)
	.make.bias.adj.link <- function(link, sigma) {
		 if (is.null(sigma))
			  stop("Must specify 'sigma' to obtain bias-adjusted back transformations", call. = FALSE)
		 link$inv = link$linkinv
		 link$der = link$mu.eta
		 link$sigma22 = sigma^2 / 2
		 link$der2 = function(eta) with(link, 1000 * (der(eta + .0005) - der(eta - .0005)))
		 link$linkinv = function(eta) with(link, inv(eta) + sigma22 * der2(eta))
		 link$mu.eta = function(eta) with(link, der(eta) +
														  1000 * sigma22 * (der2(eta + .0005) - der2(eta - .0005)))
		 link
	}

	link <- vareff_objects$link

	if (bias.adjust) {
		if (is.null(sigma)) {
			sigma <- get_sigma(mod, ...)
		}
		link <- .make.bias.adj.link(link, sigma)
	}

	out <- list(term = paste(focal.predictors, collapse="*")
		, formula = formula(mod), response = get_response(mod)
		, variables = x, fit = pred
		, x = if (pop.ave=="none") {predict.data[, 1:n.focal, drop=FALSE]} else predict.data[, x.var, drop=FALSE]
		, model.matrix = mm, data = X, x.var=x.var
		, se = pse_var/mult
		, lwr = lwr#pred - mult*pse_var
		, upr = upr#pred + mult*pse_var
		, family = vareff_objects$link$family
		, link = link 
	)

	## Organize
   type <- match.arg(type)
   linkinv <- if (is.null(out$link$linkinv)) I else out$link$linkinv
   linkmu.eta <- if(is.null(out$link$mu.eta)) I else out$link$mu.eta
   temp <- out$x
   for (var in names(temp)){
	  if (is.factor(temp[[var]])){
		 # handle factors with "valid" NA level
		 temp[[var]] <- addNA(temp[[var]]) 
	  }
   }
	out$temp <- temp
	result <- switch(type
	  	, response= { if (is.null(out$se)) 
		  	data.frame(out$x, fit=as.vector(transform(out$fit)))
			else 
				data.frame(out$x, fit=as.vector(linkinv(out$fit))
					, se = as.vector(linkmu.eta(out$fit) * out$se)
					, lwr=as.vector(linkinv(out$lwr))
					, upr=as.vector(linkinv(out$upr)))
		} , link = { if (is.null(out$se)) 
		  	data.frame(out$x, fit=as.vector(out$fit))
		 	else 
				data.frame(out$x, fit=as.vector(out$fit), se=as.vector(out$se)
					, lwr=as.vector(out$lwr), upr= as.vector(out$upr))}
	)

	if (pop.ave=="quantile" || pop.ave=="population"){
		form <- as.formula(paste0(".~", paste0(colnames(out$x), collapse = "+")))
		result <- aggregate(form, result, FUN=function(x)mean(x, na.rm=TRUE))
	} 

	if (!is.null(modelname)) {
		result$model <- modelname
	}
	attr(result, "type") <- type
	attr(result, "focal") <- unlist(focal.predictors)
	attr(result, "response") <- out$response
	attr(result, "x.var") <- out$x.var 
	if (returnall) {
		res <- list(preds = result, raw = out)
	} else {
		res <- list(preds = result)
	}
	res$call <- match.call()
	class(res) <- c("vareffects", "varpred")
	return(res)
}


#' Zero-out entries of non-focal entries of variance-covariance matrix.
#'
#' Transforms entries of variance-covariance of the coefficients corresponding to the non-focal variables to zero.
#'
#' @param mod fitted model object or a full variance-covariance matrix with terms appearing the same way they appear in the coefficients.
#' @param focal_vars a character vector specifying the variable(s) appearing on the right side of the formula of the fitted model. The entries of this variable with be non-zero.
#' @param complete logical indicating if the full variance-covariance matrix should be returned.
#'
#' @return a variance-covariance matrix of the estimated parameters with non-zero entry corresponding to the focal variable.
#'
#' @examples
#' set.seed(4567)
#' x <- rnorm(100, 3, 5)
#' y <- 0.4 + 0.7*x + rnorm(100)
#' df <- data.frame(y = y, x = x)
#' m1 <- lm(y ~ x, df)
#' print(zero_vcov(m1, "x"))
#'
#' @export
zero_vcov <- function(m, focal_vars, complete) {
	if (is.matrix(m)|is.data.frame(m)) {
		v <- m
	} else {
		assign <- get_vnames(m)$vnames
		check_vars <- assign %in% focal_vars
		if (!any(check_vars)) stop(paste0(focal_vars, " not in the model formula"))
		focal_vars <- names(assign)[check_vars]
		v <- stats::vcov(m)
		if(inherits(m, "glmmTMB")){
			v <- v$cond
		}
	}
	focal_var <- v[focal_vars,focal_vars]
	v[] <- 0 ## set all to zero, preserving dims/names
	v[focal_vars, focal_vars] <- focal_var
	return(v)
}

##' Recover data from the data from the model 
##'
##' @param mod fitted model
##' @param optional character vector or formula specifying the predictors. Important when the transformation are applied in the formula. 
##' @param envir data environment 
##' @param ... for future implementations
##'
##' @details
##' It uses the fitted model and the global environment to reconstruct the data used in the model. If \code{data} option is specified in the model formula, a dataframe with columns corresponding to the variable in the formula is returned. Any transformation, e.g. \code{log} specified in the formula terms is not evaluated on the returned data frame. However, if no is provided, the dataframe is constructed from the formula terms with all transformations evaluated.
##'
##' @return a dataframe
##'
##' @examples
##' set.seed(4567)
##' x <- rnorm(100, 3, 5)
##' y <- 0.4 + 0.7*x + rnorm(100)
##' df <- data.frame(y = y, x = x)
##' m1 <- lm(y ~ x, df)
##' d1 <- recoverdata(m1)
##' head(d1)
##' m2 <- lm(y ~ x)
##' d2 <- recoverdata(m2)
##' head(d2)
##'
##' @export 
##'
## add Stack Overflow URL
## TEST CASES???
recoverdata <- function(mod, extras = NULL, envir = environment(formula(mod)), ...) {
	f <- formula(mod)
	data <- eval(getCall(mod)$data, envir)
	if (is.null(data)) {
            if (is.null(extras)) {
                ## df <- eval(bquote(model.frame(.(f))), envir)
			df <- eval(call("model.frame", f), envir) 
		} else {
			df <- eval(call("expand.model.frame", f, extras = extras), envir) 
		}
	} else {
		df <- eval(call("model.frame", data = data, drop.unused.levels = TRUE), envir)
	}
	if (!is.null(extras) || !is.null(data)) {
		resp <- get_response(mod)
		xvars <- all.vars(delete.response(terms(mod)))
		df <- df[, colnames(df) %in% c(resp, xvars), drop=FALSE]
	}
	return(df)
}

