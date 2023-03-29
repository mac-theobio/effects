library(varpred)

library(shellpipes)
loadEnvironments()


## Chat GPT 


get_model_columns <- function(model, variable) {
	varname <- deparse(substitute(variable))
	predictors <- all.vars(formula(model))[-1]
	model_matrix <- model.matrix(model)
	cn <- colnames(model_matrix)
	return(cn)

	var_cols <- which(cn == varname)
	
	
	return(var_cols)
}

get_model_columns(mod, "nitro")

