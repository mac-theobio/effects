#' Predictor effects for GL(M)Ms
#'
#' Computes predictor effect for generalized linear (mixed) models together with the associated confidence intervals anchored on some values, mostly, model center. It also incorporates proaches for correcting bias in predictions for GL(M)Ms involving nonlinear link functions.
#'
#' @details
#' Predictor effects computes \code{E(Y|X)} by meaningfully holding the non-focal predictors constant (or averaged in some meaningful way) while varying the focal predictor, with the goal that the response (\code{E(Y|X)}) represents how the model responds to the changes in the focal predictor.
#'
#' The traditional way to compute variances for predictions is \eqn{\sigma^2 = \textrm{Diag}(\bX^\star \Sigma \bX^{\star\top})}, so that the confidence intervals are \eqn{\eta \pm q\sigma}, where \eqn{q} is an appropriate quantile of Normal or t distribution. This approach incorporates all the uncertainties -- including the uncertainty due to non-focal predictors.  But what if we are only interested in the uncertainty as a result of the focal predictor, so that the confidence intervals are \eqn{\eta \pm q \sigma_f} (what we call anchored CIs)? There are two ways to anchor CIs: \emph{variance-covariance} matrix based which requires properly scaled input predictors prior to model fitting; and \emph{centered model matrix} which is more general and does not require scaled input predictors prior to model fitting.
#'
#' Currently, the package supports \code{lm, glm, lme4} and \code{glmmTMB} models.
#'
#' @param mod fitted model object. See details for supported class of models.
#' @param focal_predictors a character vector of one or more predictors. For models with interaction, the marginal predictions are obtained by specifying the corresponding predictors together. For example \code{~x1*x2} is specified as \code{c("x1", "x2")} to obtain the predictor effect for \code{x1} while holding \code{x2} and \code{x1:x2} at their typical values. If no interactions are present in the model, specifying more than one predictors compares predictions between the predictors.
#' @param x.var a character specifying the predictor to define the x variable (horizontal axis on the plot). The default is \code{NULL}, of which the first predictor in \code{focal_predictors} is used.
#' @param type a character specifying the desired prediction. \code{type = "response"} applies inverse transformation, if exists. \code{type = "link"} requests the results as a linear predictor.
#' @param isolate logical. If \code{TRUE}, the \code{CIs} are anchored around the mean value of \code{x.var}, i.e., centered model matrix. By default, it is the deviation of each of the variables in the model matrix from its mean but other values can be specified through \code{isolate.value}.
#' @param isolate.value numeric (default \code{isolate.value = NULL}). If \code{isolate = TRUE}, otherwise ignored, is the deviation from the mean of \code{x.var}.
#' @param level desired confidence interval for computing marginal predictions. Default is \code{0.95}.
#' @param steps number of points to evaluate numerical predictors in \code{focal_predictors}. The default is \code{100}. Unique levels of \code{focal_predictors} are used in the case categorical predictors.  
#' @param at default \code{NULL}. Otherwise, is a named \code{list} specifying points to evaluate \code{focal_predictors}. The names in the list should match the names used in \code{focal_predictors}.
#' @param dfspec default \code{100}. Specified degrees of freedom for model which do not return \code{df}. This is used in computation of confidence intervals.
#' @param vcov. a function or a matrix. If a function, it is used to compute the variance-covariance matrix of the model coefficients. The function should take model as it's first (or maybe only) argument. A matrix of variance-covariance matrix of the estimated coefficient can also be used. Otherwise \code{vcov(mod)} is used internally. Specifying \code{vcov.} is important when "anchored" CIs are required. However, with this approach, the predictors should be properly scaled, for example, scaled. {isolate=TRUE} centers at the mean of the model matrix without requiring the scaled input predictors. See examples.
#' @param internal logical. If \code{TRUE}, the entries of the non-focal predictor (see x.var) in the variance-covariance matrix are internally zeroed-out using \code{\link[vareffects]{zero_vcov}}. Default is \code{FALSE}.
#' @param within.category only for categorical focal predictors. If \code{TRUE}, the columns of the no-focal variables in the model matrix are averaged across the levels of the categorical focal predictors, i.e., unweighted average. If \code{FALSE} (default), the population (in the model matrix) or weighted averages is used, i.e., anchored at the model center. 
#' @param zero_out_interaction logical [EXPERIMENTAL]. If \code{TRUE} the uncertainty as a result of interaction terms are removed (set to zero) when \code{ignored if isolate = FALSE}. Only main effect SEs are computed. To obtain centered CIs, the numerical predictors in the interaction terms should be scaled (centered); and sum to zero contrast used in case of categorical predictor.
#' @param avefun the averaging scheme (function) to be used in conditioning non-focal predictors. Default is \code{mean}.
#' @param offset a function or a value (FIXME:).
#' @param bias.adjust specifies the bias correction method. If "none" (default), no bias correction method is applied; if "delta", delta method is used; if "population", all the values of non-focal predictors are used; otherwise, if "quantile", quantiles of non-focal numerical predictors are use. The options "quantile" and "population" (both EXPERIMENTAL) are used for bias correction in GL(M)M models involving non-linear link functions.
#' @param sigma standard deviation used in delta method (only if \code{bias.adjust="delta"}).
#' @param include.re logical. If \code{TRUE}, the random effects components of mixed models is included.
#' @param character string naming the predictions. Useful when comparing several predictions.
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
#' # Predictor rffects of x1u
#' pred1u <- varpred(m1u, "x1u")
#' plot(pred1u)
#'
#' # Scaled model (x1 centered = x1 - mean(x1))
#' m1c <- lm(y ~ x1c + x2u, df)
#' # All uncertainities included
#' pred1c <- varpred(m1c, "x1c")
#' plot(pred1c)
#'
#' # Centered predictor effects
#' # Results similar to m1c
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

varpred <- function(mod
	, focal_predictors
	, x.var = NULL
	, type = c("response", "link")
	, isolate = FALSE
	, isolate.value = NULL
	, level = 0.95
	, steps = 100
	, at = list()
	, dfspec = 100
	, vcov. = NULL
	, internal = FALSE
	, within.category = FALSE
	, zero_out_interaction = FALSE
	, avefun = mean
	, offset = NULL
	, bias.adjust = c("none", "delta", "quantile", "population")
	, sigma = c("lp", "mod")
	, include.re = FALSE
	, modelname = NULL
	, returnall = FALSE, ...) {
	
	bias.adjust <- match.arg(bias.adjust)

	if (!is.numeric(sigma)) {
		sigma <- match.arg(sigma)
	}

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

	n.focal <- length(focal.predictors)
	if (is.null(x.var) & n.focal>1L) {
		x.var <- focal.predictors[[2]]
		message(paste0("x.var was not specified, ", x.var, " is used instead."))
	} else if (is.null(x.var)) {
		x.var <- focal.predictors[[1]]
	}
	
#	formula.rhs <- insight::find_formula(mod)$conditional #formula(mod)[c(1, 3)]
	model_frame_objs <- clean_model(focal.predictors=focal.predictors
		, mod = mod
		, xlevels=at
		, default.levels=NULL
		, formula.rhs=rTerms
		, steps=steps
		, x.var=x.var
		, typical=avefun
		, vnames=vnames
		, within.category=within.category
		, bias.adjust = bias.adjust
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
	within.category <- model_frame_objs$within.category
	typical <- avefun
	
	mf <- model.frame(rTerms, predict.data, xlev = factor.levels, na.action=NULL)
	mod.matrix.all <- model.matrix(mod)
	mod.matrix <- model.matrix(formula.rhs
		, data=mf
		, contrasts.arg=vareff_objects$contrasts
	)
	mm <- get_model_matrix(mod
		, mod.matrix
		, mod.matrix.all
		, X.mod
		, factor.cols
		, cnames
		, focal.predictors
		, excluded.predictors
		, typical
		, apply.typical.to.factors=TRUE
		, focal.type=x[[x.var]]$is.factor
		, within.category=within.category
	)
	
	if (within.category & x[[x.var]]$is.factor) {
		## FIXME: better way to average over categories of the focal predictors
		mm <- lapply(split(as.data.frame(mm), x[[x.var]]$levels), colMeans)
		mm <- do.call("rbind", mm)
	}

	# Stats
	mult <- get_stats(mod, level, dfspec)
		
	# Predictions
	# col_mean <- apply(mod.matrix.all, 2, typical)
	col_mean <- apply(mm, 2, typical)
	pse_var <- mult*get_sderror(mod=mod, vcov.=vcov., mm=mm, col_mean=col_mean, isolate=isolate
		, isolate.value=isolate.value, internal=internal, vareff_objects=vareff_objects, x.var=x.var
		, typical=typical, formula.rhs=formula.rhs, zero_out_interaction=zero_out_interaction, mf=mf
	)

	# Offset
	if (is.null(offset)) offset <- mean
	if (is.numeric(offset) && length(offset) == 1) {
		off <- offset
	} else if (is.function(offset)) {
		mod.off <- model.offset(mf)
		if (is.null(mod.off)) {
			off <- 0
		} else {
			off <- offset(mod.off)
		}
	} else {
		stop("offset must be a function or a number")
	}

	pred <- off + as.vector(mm %*% betahat)
	lwr <- pred - pse_var
	upr <- pred + pse_var
	
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

	if (bias.adjust=="delta") {
		if (sigma=="mod") {
			sigma <- get_sigma(mod, ...)
		} else if (sigma=="lp") {
			sigma <- sd(pred) 
		}
		link <- .make.bias.adj.link(link, sigma)
	}
	
	out <- list(term = paste(focal.predictors, collapse="*")
		, formula = formula(mod), response = get_response(mod)
		, variables = x, fit = pred
		, x = if (bias.adjust=="none"||bias.adjust=="delta") {predict.data[, 1:n.focal, drop=FALSE]} else predict.data[, x.var, drop=FALSE]
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
   
	## FIXME: still on within category averaging
	if (within.category) {
		temp1 <- list()
	}
	temp <- out$x
   for (var in colnames(temp)){
	  if (is.factor(temp[[var]])){
		 # handle factors with "valid" NA level
		 if (within.category) {
		 	temp1[[var]] <- factor.levels[[var]]
		 }
		 temp[[var]] <- addNA(temp[[var]]) 
	  } else {
		 if (within.category) {
		 	temp1[[var]] <- unique(temp[[var]])
		 }
	  }
   }
	if (within.category) temp <- do.call("expand.grid", temp1)
	out$x <- temp
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

	if (bias.adjust=="quantile" || bias.adjust=="population"){
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

