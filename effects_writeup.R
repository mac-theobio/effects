### R code from vignette source '/home/dushoff/screens/research/effects/effects_writeup.Rnw'

###################################################
### code chunk number 1: set_up
###################################################
library(ggplot2)
library(margins)
library(effects)
library(emmeans)
library(jdeffects)
jdtheme()


###################################################
### code chunk number 2: simple_sim
###################################################
set.seed(101)
N <- 100
x1_min <- 1
x1_max <- 9
b0 <- 0.3
b1 <- 0.1
b2 <- -0.6
x2_levels <- factor(c("A", "B"))
df <- expand.grid(x1u = runif(n=N, min=x1_min, max=x1_max)
	, x2u = x2_levels
)
X <- model.matrix(~x1u + x2u, df)
betas <- c(b0, b1, b2)
df$y <- rnorm(nrow(df), mean= X %*% betas, sd=1)
df2 <- df
df <- transform(df
	, x1c = drop(scale(x1u, scale=FALSE)) 
	, x1s = drop(scale(x1u, center=FALSE))
	, x1sd = drop(scale(x1u))
)
head(df)


###################################################
### code chunk number 3: model_lm_u
###################################################
df_temp <- drop(df) 	# margins doesn't work with transform 
                   	# but we need df to extract attributes :(
lm_u <- lm(y ~ x1u + x2u, data = df_temp)


###################################################
### code chunk number 4: model_lm_c
###################################################
lm_c <- lm(y ~ x1c + x2u, data = df_temp)


###################################################
### code chunk number 5: model_lm_s
###################################################
lm_s <- lm(y ~ x1s + x2u, data = df_temp)


###################################################
### code chunk number 6: model_lm_sd
###################################################
lm_sd <- lm(y ~ x1sd + x2u, data = df_temp)


###################################################
### code chunk number 7: model_summary
###################################################
modsummary <- function(patterns="^lm_", fun, simplify = TRUE
	, combine = c("cbind", "rbind"), match_colnames = NULL){
	mod <- objects(name=1L, pattern=patterns, sorted=TRUE)
	combine <- match.arg(combine)
	out <- sapply(mod, function(m){
		out <- fun(get(m))
		out <- round(out, 4)
		if (combine=="rbind"){
			if (!is.null(match_colnames)){
				colnames(out) <- match_colnames
			}
			out <- cbind.data.frame(out, model = m)
		}
		return(out)
	}, simplify = FALSE)
	if (simplify){
		out <- do.call(combine, out)
		if (combine=="cbind") {
			colnames(out) <- mod
		}
	}
	return(out)
}


###################################################
### code chunk number 8: simple_coefs
###################################################
coef_est <- modsummary(fun=coef)
print(coef_est)


###################################################
### code chunk number 9: simple_loglik
###################################################
ll_est <- modsummary(fun=logLik)
print(ll_est)


###################################################
### code chunk number 10: simple_vcov
###################################################
vcov_est <- modsummary(fun=function(x)vcov(x), simplify=TRUE
	, combine="rbind", match_colnames = names(coef(lm_u)))
print(vcov_est)


###################################################
### code chunk number 11: condional_summary
###################################################
condsummary <- function(mod_list, fun, resp = "y", simplify = TRUE
	, combine = c("cbind", "rbind"), scale_param = list(), fmethod = NULL){
	focal <- names(mod_list)
	combine <- match.arg(combine)
	out <- sapply(focal, function(f){
		mod <- mod_list[[f]]
		out <- fun(mod, f)
		if (inherits(out, c("emmeans", "emmGrid"))) {
			out <- as.data.frame(out)
			oldn <- c(f, "emmean"
				, grep("\\.CL", colnames(out), value=TRUE)
			)
			newn <- c("xvar", "fit", "lwr", "upr")
			colnames(out)[colnames(out) %in% oldn] <-  newn
		} else if (inherits(out, "eff")) {
			out <- as.data.frame(out)
			oldn <- c(f, "lower", "upper")
			newn <- c("xvar", "lwr", "upr")
			colnames(out)[colnames(out) %in% oldn] <-  newn
		} else if (any(colnames(out) %in% c("xvals", "yvals"))) {
			## Not proper way to handle margins object.
			## Maybe extend cplot to return inheritable object
			oldn <- c("xvals", "yvals", "lower", "upper")
			newn <- c("xvar", "fit", "lwr", "upr")
			colnames(out)[colnames(out) %in% oldn] <-  newn
		} else {
			out <- data.frame(out)
			colnames(out)[colnames(out)%in%f] <- "xvar"
		}
		if (combine=="rbind"){
			out <- cbind.data.frame(out, model = f)
			out <- out[, c("xvar", "fit", "lwr", "upr", "model")]
		}
		pp <- names(scale_param)
		if (!is.null(pp)){
			if(grepl("s$",f)){
				vv <- grep("s$",f, value=TRUE)
				xsd <- scale_param[[vv]]
				out[,"xvar"] <- out[,"xvar"]*xsd
			}
			if(grepl("c$",f)){
				vv <- grep("c$",f, value=TRUE)
				xmu <- scale_param[[vv]]
				out[,"xvar"] <- out[,"xvar"] + xmu
			}
			if(grepl("sd$",f)){
				vv <- grep("sd$",pp,value=TRUE)
				xmu <- scale_param[[vv]][1]
				xsd <- scale_param[[vv]][2] 
				out[,"xvar"] <- out[,"xvar"]*xsd + xmu
				
			}
		}
		if (!is.null(fmethod)){
			out[,"method"] <- fmethod
		}
		return(out)
	}, simplify = FALSE)
	f <- attr(out, "focal")
	if (simplify){
		out <- do.call(combine, out)
		if (combine=="cbind") {
			colnames(out) <- mod
		} else {
			rownames(out) <- NULL
		}
	}
	attr(out, "response") <- resp
	return(out)
}


###################################################
### code chunk number 12: simple_model
###################################################
simple_models <- list(x1u = lm_u, x1c = lm_c, x1s = lm_s, x1sd = lm_sd)


###################################################
### code chunk number 13: simple_conditional_specific
###################################################
## varpred
simple_vpred_all <- condsummary(mod_list=simple_models, fun=function(x, f){
	dd <- varpred(x, f)
}, simplify = TRUE, combine = "rbind", fmethod = "varpred")

## emmeans
simple_empred_all <- condsummary(mod_list=simple_models, fun=function(x, f){
	spec <- as.formula(paste0("~", f))
	dd <- emmeans(x, spec=spec, cov.keep=f)
}, simplify = TRUE, combine = "rbind", fmethod = "emmeans")

## effects
simple_efpred_all <- condsummary(mod_list=simple_models, fun=function(x, f){
	dd <- Effect(f, x, xlevels=100)
}, simplify = TRUE, combine = "rbind", fmethod = "effects")

## margins
simple_margpred_all <- condsummary(mod_list=simple_models, fun=function(x, f){
	dd <- cplot(x, f, what="prediction", n=100, draw=FALSE)
}, simplify = TRUE, combine = "rbind", fmethod = "margins")


## Combine all the estimates
simple_pred_all <- do.call("rbind"
	, list(simple_vpred_all, simple_empred_all, simple_efpred_all, simple_margpred_all)
)


###################################################
### code chunk number 14: effects_writeup.Rnw:295-300
###################################################
class(simple_pred_all) <- c("jdeffects", "data.frame") # plot.effects
simple_pred_all_plot <- (plot(simple_pred_all) 
	+ facet_wrap(~method) 
	+ theme(legend.position="bottom")
)


###################################################
### code chunk number 15: simple_pred_all_plot
###################################################
simple_pred_all_plot


###################################################
### code chunk number 16: simple_conditional_backtrans
###################################################
## Extract scaling parameters
scale_param <- list(x1s = unlist(attributes(df$x1s))
	, x1c = unlist(attributes(df$x1c))
	, x1sd = unlist(attributes(df$x1sd))
)

## varpred
simple_vpred_spec <- condsummary(mod_list=simple_models, fun=function(x, f){
		dd <- varpred(x, f)
	}, simplify = TRUE, combine = "rbind"
	, scale_param = scale_param, fmethod = "varpred"
)

## emmeans
simple_empred_spec <- condsummary(mod_list=simple_models, fun=function(x, f){
		spec <- as.formula(paste0("~", f))
		dd <- emmeans(x, spec=spec, cov.keep=f)
	}, simplify = TRUE, combine = "rbind"
	, scale_param = scale_param, fmethod = "emmeans"
)

## effects
simple_efpred_spec <- condsummary(mod_list=simple_models, fun=function(x, f){
		dd <- Effect(f, x, xlevels=100)
	}, simplify = TRUE, combine = "rbind"
	, scale_param = scale_param, fmethod = "effects"
)

## margins
simple_margpred_spec <- condsummary(mod_list=simple_models, fun=function(x, f){
		dd <- cplot(x, f, what="prediction", n=100, draw=FALSE)
	}, simplify = TRUE, combine = "rbind"
	, scale_param = scale_param, fmethod = "margins"
)

## Combine all the estimates
simple_pred_spec <- do.call("rbind"
	, list(simple_vpred_spec, simple_empred_spec, simple_efpred_spec, simple_margpred_spec)
)


###################################################
### code chunk number 17: effects_writeup.Rnw:359-364
###################################################
class(simple_pred_spec) <- c("jdeffects", "data.frame") # plot.effects
simple_pred_spec_plot <- (plot(simple_pred_spec)
	+ facet_wrap(~method)
	+ theme(legend.position="bottom")
)


###################################################
### code chunk number 18: simple_pred_spec_plot
###################################################
simple_pred_spec_plot


###################################################
### code chunk number 19: simple_conditional_specific_cat
###################################################
## varpred
simple_models_cat <- list(x2u = lm_u)
simple_vpred_cat <- condsummary(mod_list=simple_models_cat, fun=function(x, f){
	dd <- varpred(x, f)
}, simplify = TRUE, combine = "rbind", fmethod = "varpred")

## emmeans
simple_empred_cat <- condsummary(mod_list=simple_models_cat, fun=function(x, f){
	spec <- as.formula(paste0("~", f))
	dd <- emmeans(x, spec=spec, cov.keep=f)
}, simplify = TRUE, combine = "rbind", fmethod = "emmeans")

## effects
simple_efpred_cat <- condsummary(mod_list=simple_models_cat, fun=function(x, f){
	dd <- Effect(f, x, xlevels=100)
}, simplify = TRUE, combine = "rbind", fmethod = "effects")

## margins
simple_margpred_cat <- condsummary(mod_list=simple_models_cat, fun=function(x, f){
	dd <- cplot(x, f, what="prediction", n=100, draw=FALSE)
}, simplify = TRUE, combine = "rbind", fmethod = "margins")


## Combine all the estimates
simple_pred_cat <- do.call("rbind"
	, list(simple_vpred_cat, simple_empred_cat, simple_efpred_cat, simple_margpred_cat)
)


###################################################
### code chunk number 20: effects_writeup.Rnw:411-416
###################################################
class(simple_pred_cat) <- c("jdeffects", "data.frame") # plot.effects
simple_pred_cat_plot <- (plot(simple_pred_cat)
	+ facet_wrap(~method)
	+ theme(legend.position="bottom")
)


###################################################
### code chunk number 21: simple_pred_cat_plot
###################################################
simple_pred_cat_plot


###################################################
### code chunk number 22: full_vcov
###################################################
lm_u_vcov <- vcov(lm_u)
print(lm_u_vcov)


###################################################
### code chunk number 23: zero_out_ex
###################################################
lm_u_vcov_zero <- zero_vcov(lm_u, focal_vars = "x1u")
print(lm_u_vcov_zero)


###################################################
### code chunk number 24: simple_marginal_specific
###################################################
## varpred
simple_vpred_all <- condsummary(mod_list=simple_models, fun=function(x, f){
	dd <- varpred(x, f, vcov. = zero_vcov(x, f))
}, simplify = TRUE, combine = "rbind", fmethod = "varpred")

## emmeans
simple_empred_all <- condsummary(mod_list=simple_models, fun=function(x, f){
	spec <- as.formula(paste0("~", f))
	dd <- emmeans(x, spec=spec, cov.keep=f, vcov. = zero_vcov(x, f))
}, simplify = TRUE, combine = "rbind", fmethod = "emmeans")

## effects
simple_efpred_all <- condsummary(mod_list=simple_models, fun=function(x, f){
	dd <- Effect(f, x, xlevels=100, vcov. = function(x, complete=FALSE)zero_vcov(x, f, complete))
}, simplify = TRUE, combine = "rbind", fmethod = "effects")

## margins
simple_margpred_all <- condsummary(mod_list=simple_models, fun=function(x, f){
	dd <- cplot(x, f, what="prediction", n=100, draw=FALSE, vcov=function(x)zero_vcov(x, f))
}, simplify = TRUE, combine = "rbind", fmethod = "margins")


## Combine all the estimates
simple_pred_all <- do.call("rbind"
	, list(simple_vpred_all, simple_empred_all, simple_efpred_all, simple_margpred_all)
)


###################################################
### code chunk number 25: effects_writeup.Rnw:488-493
###################################################
class(simple_pred_all) <- c("jdeffects", "data.frame")
simple_pred_all_plot <- (plot(simple_pred_all)
	+ facet_wrap(~method)
	+ theme(legend.position="bottom")
)


###################################################
### code chunk number 26: simple_pred_all_plot
###################################################
simple_pred_all_plot


###################################################
### code chunk number 27: simple_marginal_backtrans
###################################################
## varpred
simple_vpred_spec <- condsummary(mod_list=simple_models, fun=function(x, f){
		dd <- varpred(x, f, vcov. = zero_vcov(x, f))
	}, simplify = TRUE, combine = "rbind"
	, scale_param = scale_param, fmethod = "varpred"
)

## emmeans
simple_empred_spec <- condsummary(mod_list=simple_models, fun=function(x, f){
		spec <- as.formula(paste0("~", f))
		dd <- emmeans(x, spec=spec, cov.keep=f, vcov. = zero_vcov(x, f))
	}, simplify = TRUE, combine = "rbind"
	, scale_param = scale_param, fmethod = "emmeans"
)

## effects
simple_efpred_spec <- condsummary(mod_list=simple_models, fun=function(x, f){
		dd <- Effect(f, x, xlevels=100, vcov. = function(x, complete=FALSE)zero_vcov(x, f, complete))
	}, simplify = TRUE, combine = "rbind"
	, scale_param = scale_param, fmethod = "effects"
)

## margins
simple_margpred_spec <- condsummary(mod_list=simple_models, fun=function(x, f){
		dd <- cplot(x, f, what="prediction", n=100, draw=FALSE, vcov=function(x)zero_vcov(x, f))
	}, simplify = TRUE, combine = "rbind"
	, scale_param = scale_param, fmethod = "margins"
)

## Combine all the estimates
simple_pred_spec <- do.call("rbind"
	, list(simple_vpred_spec, simple_empred_spec, simple_efpred_spec, simple_margpred_spec)
)


###################################################
### code chunk number 28: effects_writeup.Rnw:550-553
###################################################
## Plot centered and non-centered separately
simple_pred_spec1 <- subset(simple_pred_spec, model=="x1u"|model=="x1s")
simple_pred_spec2 <- subset(simple_pred_spec, model=="x1c"|model=="x1sd")


###################################################
### code chunk number 29: simple_pred_spec_plot1
###################################################
class(simple_pred_spec1) <- c("jdeffects", "data.frame")
simple_pred_spec_plot1 <- (plot(simple_pred_spec1)
	+ facet_wrap(~method)
	+ theme(legend.position="bottom")
)
simple_pred_spec_plot1


###################################################
### code chunk number 30: simple_pred_spec_plot2
###################################################
class(simple_pred_spec2) <- c("jdeffects", "data.frame")
simple_pred_spec_plot2 <- (plot(simple_pred_spec2)
	+ facet_wrap(~method)
	+ theme(legend.position="bottom")
)
simple_pred_spec_plot2


###################################################
### code chunk number 31: vapred_isolate_numeric
###################################################
## Centered predictions from unscaled model
vpred_c <- varpred(lm_u, focal = "x1u", isolate = TRUE)
plot(vpred_c)


###################################################
### code chunk number 32: simple_marginal_specific_cat
###################################################
## varpred
simple_models_cat <- list(x2u = lm_u)
simple_margvpred_cat <- condsummary(mod_list=simple_models_cat, fun=function(x, f){
	dd <- varpred(x, f, isolate=TRUE)
}, simplify = TRUE, combine = "rbind", fmethod = "varpred")

## emmeans
simple_margempred_cat <- condsummary(mod_list=simple_models_cat, fun=function(x, f){
	spec <- as.formula(paste0("~", f))
	dd <- emmeans(x, spec=spec, cov.keep=f, vcov. = zero_vcov(x, f))
}, simplify = TRUE, combine = "rbind", fmethod = "emmeans")

## effects
simple_margefpred_cat <- condsummary(mod_list=simple_models_cat, fun=function(x, f){
	dd <- Effect(f, x, xlevels=100, vcov. = function(x, complete=FALSE)zero_vcov(x, f, complete))
}, simplify = TRUE, combine = "rbind", fmethod = "effects")

## margins
simple_margpred_cat <- condsummary(mod_list=simple_models_cat, fun=function(x, f){
	dd <- cplot(x, f, what="prediction", n=100, draw=FALSE, vcov=function(x)zero_vcov(x, f))
}, simplify = TRUE, combine = "rbind", fmethod = "margins")


## Combine all the estimates
simple_margpred_cat <- do.call("rbind"
	, list(simple_margvpred_cat, simple_margempred_cat, simple_margefpred_cat, simple_margpred_cat)
)


###################################################
### code chunk number 33: effects_writeup.Rnw:637-642
###################################################
class(simple_margpred_cat) <- c("jdeffects", "data.frame")
simple_margpred_cat_plot <- (plot(simple_margpred_cat)
	+ facet_wrap(~method)
	+ theme(legend.position="bottom")
)


###################################################
### code chunk number 34: simple_margpred_cat_plot
###################################################
simple_margpred_cat_plot


