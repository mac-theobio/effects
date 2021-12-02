library(dplyr)
library(data.table)
library(vareffects)
library(shellpipes)

loadEnvironments()

## Varpred function function
varpredfun <- function(mod, focal, at, vcmat = NULL, model){
	if (is.null(vcmat)){
		pred <- varpred(mod, focal, at = at)
	} else {
		pred <- varpred(mod, focal, at = at, vcov. = vcmat)
	}
	pred <- pred$preds
	pred$model <- model
	pred$x <- unlist(at)
	return(pred)
}

# Marginal effect of x1

## Unscaled predictions
### Unzeroed vcov
x1u_jd <- varpredfun(mod_unscaled, focal = "x1"
	, at = list(x1 = pred_df_x1$x1)
	, model = "jd_u"
)
head(x1u_jd)

### Zeroed non-focal vv
x1u_jd_zero <- varpredfun(mod_unscaled, focal = "x1"
	, at = list(x1 = pred_df_x1$x1)
	, model = "jd_u_zero"
	, vcmat = zero_vcov(mod_unscaled, "x1")
)
head(x1u_jd_zero)

## Scaled
### Unzeroed vcov
x1std_jd <- varpredfun(mod_scaled, focal = "x1std"
	, at = list(x1std = pred_df_x1std$x1std),
	, model = "jd_s"
)
head(x1std_jd)

### Zeroed vcov for non-focal predictors
x1std_jd_zero <- varpredfun(mod_scaled, focal = "x1std"
	, at = list(x1std = pred_df_x1std$x1std)
	, model = "jd_s_zero"
	, vcmat = zero_vcov(mod_scaled, "x1std")
)
head(x1std_jd_zero)

## Combine the predictions
predict_x1_jd <- (list(x1u_jd, x1std_jd, x1u_jd_zero, x1std_jd_zero)
	%>% bind_rows()
	%>% select(!!c("fit", "lwr", "upr", "x", "model"))
)
head(predict_x1_jd)

saveVars(predict_x1_jd)


