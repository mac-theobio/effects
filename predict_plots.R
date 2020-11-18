library(dplyr)
library(ggplot2); theme_set(theme_bw())

source("makestuff/makeRfuns.R")
commandEnvironments()
sourceFiles()
makeGraphics()

######################################################################

# x1 marginal effects
pred_x1 <- (bind_rows(predict_x1_base, predict_x1_em)
	%>% mutate(trans = ifelse(grepl("_u", model), "unscaled", "scaled")
		, model = gsub("_u|_s", "", model)
	)
)

x1_plot <- (ggplot(pred_x1, aes(x = x, group = model))
	+ geom_line(aes(y = fit, colour = model), alpha=0.1)
	+ geom_ribbon(aes(ymin = lwr, ymax = upr, fill = model), colour = NA, alpha=0.1)
	+ scale_colour_viridis_d(limits = c("em", "em_zero", "base"))
	+ scale_fill_viridis_d(limits = c("em", "em_zero", "base"))
	+ labs(x = "x1", fill = "model", colour = "model")
	+ facet_wrap(~trans, scales = "free_x")
)
print(x1_plot)

## Zoom-in to emmeans only
pred_x1_zoomed <- (pred_x1
	%>% filter(grepl("em", model))
)

x1_plot_zoomed <- (ggplot(pred_x1_zoomed, aes(x = x, group = model))
	+ geom_line(aes(y = fit, colour = model), alpha=0.1)
	+ geom_ribbon(aes(ymin = lwr, ymax = upr, fill = model), colour = NA, alpha=0.1)
	+ scale_colour_viridis_d(limits = c("em", "em_zero"))
	+ scale_fill_viridis_d(limits = c("em", "em_zero"))
	+ labs(x = "x1", title = "Zoomed", fill = "model", colour = "model")
	+ facet_wrap(~trans, scales = "free_x")
)
print(x1_plot_zoomed)

######################################################################

# x2 marginal effects
pred_x2 <- (bind_rows(predict_x2_base, predict_x2_em)
	%>% mutate(trans = ifelse(grepl("_u", model), "unscaled", "scaled")
		, model = gsub("_u|_s", "", model)
	)
)
head(pred_x2)

x2_plot <- (ggplot(pred_x2, aes(x = x, group = model))
	+ geom_line(aes(y = fit, colour = model), alpha=0.1)
	+ geom_ribbon(aes(ymin = lwr, ymax = upr, fill = model), colour = NA, alpha=0.1)
	+ scale_colour_viridis_d(limits = c("em", "em_zero", "base"))
	+ scale_fill_viridis_d(limits = c("em", "em_zero", "base"))
	+ labs(x = "x2", fill = "model", colour = "model")
	+ facet_wrap(~trans, scales = "free")
)
print(x2_plot)

## Zoom-in to emmeans only
pred_x2_zoomed <- (pred_x2
	%>% filter(grepl("em", model))
)

x2_plot_zoomed <- (ggplot(pred_x2_zoomed, aes(x = x, group = model))
	+ geom_line(aes(y = fit, colour = model), alpha=0.1)
	+ geom_ribbon(aes(ymin = lwr, ymax = upr, fill = model), colour = NA, alpha=0.1)
	+ scale_colour_viridis_d(limits = c("em", "em_zero"))
	+ scale_fill_viridis_d(limits = c("em", "em_zero"))
	+ labs(x = "x2", title = "Zoomed", fill = "model", colour = "model")
	+ facet_wrap(~trans, scales = "free_x")
)
print(x2_plot_zoomed)
