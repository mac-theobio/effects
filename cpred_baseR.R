## Customize based R predict method to add CI and zero-out non-focal predictors
### Manually compute the uncertainty around the estimates

cpredict <- function(mod, newdata, level = 0.95, vvfun = NULL) {
	beta_hat <- coef(mod)
	mterms <- terms(mod)
	newX <- model.matrix(delete.response(mterms), newdata)
	lp <- as.vector(newX %*% beta_hat) ## Predicted outcome (but might consider link functions for other model types)

	## Compute variance-covariance
	if (is.null(vvfun)){
		vv <- vcov(mod)
	} else {
		vv <- vvfun
	}

	## Compute variance
	vars <- newX %*% vv %*% t(newX)

	## Compute the standard errors
	sds <- sqrt(diag(vars))

	## t-value
	residual <- mod$df.residual
	t_value <- qt(1 - (1 - level)/2, residual)
	bound <- t_value * sds
	pred_df <- data.frame(fit = lp
		, lwr = lp - bound
		, upr = lp + bound
	)
	return(pred_df)
}
