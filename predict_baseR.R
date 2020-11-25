library(dplyr)
source("makestuff/makeRfuns.R")
commandEnvironments()
sourceFiles()

## Predict function. Calls customized base R predict
predfun <- function(mod, var, newdata, model, level = 0.95, vvfun=NULL){
	pred <- cpredict(mod, newdata, level, vvfun)
	pred$x <- newdata[[var]]
	pred$model <- model
	return(pred)
}

# Marginal effect of x1

## Unscaled predictions
### Unzeroed vv
x1u_base <- predfun(mod_unscaled
	, var = "x1", newdata = pred_df_x1
	, mode = "base_u"
)
head(x1u_base)

### vv for non-focal predictors zeroed out
x1u_base_zero <- predfun(mod_unscaled
	, var = "x1", newdata = pred_df_x1
	, mode = "base_u_zero"
	, vvfun = zero_vcov(mod_unscaled, "x1")
)
head(x1u_base_zero)

## Scaled
### Unzeroed vv
x1s_base <- predfun(mod_scaled
	, var = "x1s", newdata = pred_df_x1s
	, mode = "base_s"
)
head(x1s_base)

### Non-focal vv zeroed out
x1s_base_zero <- predfun(mod_scaled
	, var = "x1s", newdata = pred_df_x1s
	, mode = "base_s_zero"
	, vvfun = zero_vcov(mod_scaled, "x1s")
)
head(x1s_base_zero)

## Merge the predicitons for each predictor
predict_x1_base <- bind_rows(list(x1u_base, x1u_base_zero, x1s_base, x1s_base_zero))
head(predict_x1_base)

saveVars(predict_x1_base)


