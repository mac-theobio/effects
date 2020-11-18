library(dplyr)
source("makestuff/makeRfuns.R")
commandEnvironments()
sourceFiles()

# Marginal effect of x1
## Unscaled predictions
predict_x1u_base <- data.frame(predict(mod_unscaled, newdata = pred_df_x1, interval = "confidence", level = 0.95))
predict_x1u_base$model <- "base_u"
predict_x1u_base$x <-  pred_df_x1$x1
head(predict_x1u_base)

## Scaled
predict_x1s_base <- data.frame(predict(mod_scaled, newdata = pred_df_x1s, interval = "confidence", level = 0.95))
predict_x1s_base$model <- "base_s"
predict_x1s_base$x <-  pred_df_x1s$x1s
head(predict_x1s_base)

# Marginal effect of x2
## Unscaled predictions
predict_x2u_base <- data.frame(predict(mod_unscaled, newdata = pred_df_x2, interval = "confidence", level = 0.95))
predict_x2u_base$model <- "base_u"
predict_x2u_base$x <-  pred_df_x2$x2
head(predict_x2u_base)

## Scaled
predict_x2s_base <- data.frame(predict(mod_scaled, newdata = pred_df_x2s, interval = "confidence", level = 0.95))
predict_x2s_base$model <- "base_s"
predict_x2s_base$x <-  pred_df_x2s$x2s
head(predict_x2s_base)

## Merge the predicitons for each predictor
predict_x1_base <- bind_rows(predict_x1u_base, predict_x1s_base)
head(predict_x1_base)
predict_x2_base <- bind_rows(predict_x2u_base, predict_x2s_base)
head(predict_x2_base)

saveVars(predict_x1_base
	, predict_x2_base
)


