## Extract assign for model object esp. for glmmTMB
extract_assign <- function(mod, intercept = TRUE){
	mat <- model.matrix(mod)
	xnames <- colnames(mat)
	mod_term <- terms(mod)
	mod_form <- formula(delete.response(mod_term))
	var_labs <- all.vars(mod_form)
	assign <- attr(mat, "assign")
	if(intercept) {
		var_labs <- c("(Intercept)", var_labs)
		assign <- assign + 1
	}
	assign <- setNames(var_labs[assign], xnames)
	return(assign)
}

## zero out all but focal effects of focal variable(s)
zero_vcov <- function(m, focal_vars, intercept = TRUE) {
	assign <- extract_assign(m, intercept)
	focal_vars <- names(assign)[assign %in% focal_vars]
	v <- vcov(m)
	if(inherits(m, "glmmTMB")){
		v <- v$cond
	}
	focal_var <- v[focal_vars,focal_vars]
	v[] <- 0 ## set all to zero, preserving dims/names
	v[focal_vars, focal_vars] <- focal_var
	return(v)
}

