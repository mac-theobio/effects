library(shellpipes)
library(vareffects); varefftheme()
library(ggpubr)
library(ggplot2)
library(dplyr)
library(glmmTMB)

loadEnvironments()
startGraphics()

## Model
mod <- mod_cont_joint

## Clean predictor names
all_pred_vars <- all.vars(parse(text=attr(delete.response(terms(mod)), "term.labels")))
print(all_pred_vars)

## Compute predictions

## No bias correction
all_varpred_none_df <- sapply(all_pred_vars, function(x){
	focal <- unique(c("services", x))
	pred <- varpred(mod
		, focal
		, x.var = x
		, type="response"
		, steps=20
		, isolate=TRUE
		, zero_out_interaction=TRUE
		, bias.adjust="none"
		, modelname="none"
	)
	return(pred)
}, simplify=FALSE)

## Population-based
all_varpred_pop_df <- sapply(all_pred_vars, function(x){
	focal <- unique(c("services", x))
	pred <- varpred(mod
		, focal
		, x.var = x
		, type="response"
		, isolate=TRUE
		, steps=20
		, bias.adjust="population"
		, zero_out_interaction=TRUE
		, include.re=TRUE
		, modelname="bias corrected"
	)
	return(pred)
}, simplify=FALSE)

saveVars(all_pred_vars
	, all_varpred_pop_df
	, all_varpred_none_df
)
