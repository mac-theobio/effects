library(dplyr)
library(data.table)
library(emmeans)

source("makestuff/makeRfuns.R")
commandEnvironments()
sourceFiles()

## emmeans function
empredfun <- function(mod, spec, at, cov.keep, model, vvfun=NULL){
	if (is.null(vvfun)){
		pred <- emmeans(mod, spec, at = at
			, cov.keep=cov.keep, CIs=TRUE
		)
	} else {
		pred <- emmeans(mod, spec, at = at
			, cov.keep = cov.keep, CIs = TRUE
			, vcov. = vvfun
		)
	}
	pred <- as.data.frame(pred)
	pred$x <- at[[cov.keep]]
	pred$model <- model
	return(pred)
}

# Marginal effect of x1

## Unscaled predictions
### Unzeroed vcov
x1u_em <- empredfun(mod_unscaled, spec = ~x1
	, at = list(x1 = pred_df_x1$x1), cov.keep = "x1"
	, model = "em_u"
)
head(x1u_em)

### Zeroed non-focal vv
x1u_em_zero <- empredfun(mod_unscaled, spec = ~x1
	, at = list(x1 = pred_df_x1$x1), cov.keep = "x1"
	, model = "em_u_zero"
	, vvfun = zero_vcov(mod_unscaled, "x1")
)
head(x1u_em_zero)

## Scaled
### Unzeroed vcov
x1std_em <- empredfun(mod_scaled, spec = ~x1std
	, at = list(x1std = pred_df_x1std$x1std), cov.keep = "x1std"
	, model = "em_s"
)
head(x1std_em)

### Zeroed vcov for non-focal predictors
x1std_em_zero <- empredfun(mod_scaled, spec = ~x1std
	, at = list(x1std = pred_df_x1std$x1std), cov.keep = "x1std"
	, model = "em_s_zero"
	, vvfun = zero_vcov(mod_scaled, "x1std")
)
head(x1std_em_zero)

## Combine the predictions
predict_x1_em <- (list(x1u_em, x1std_em, x1u_em_zero, x1std_em_zero)
	%>% bind_rows()
	%>% setnames(old = c("emmean", "lower.CL", "upper.CL"), new = c("fit", "lwr", "upr"))
	%>% select(!!c("fit", "lwr", "upr", "x", "model"))
)
head(predict_x1_em)

saveVars(predict_x1_em)


