library(shellpipes)

commandEnvironments()

rsteps <- 50

## Marginal for x1
### Unscaled
x1_range <- range(sim_df$x1)
pred_df_x1 <- expand.grid(
	x1 = seq(x1_range[1], x1_range[2], length.out = rsteps)
	, x2 = mean(sim_df$x2)
)
head(pred_df_x1)

### Scaled
x1std_range <- range(sim_df$x1std)
pred_df_x1std <- expand.grid(
	x1std = seq(x1std_range[1], x1std_range[2], length.out = rsteps)
	, x2std = mean(sim_df$x2std)
)
head(pred_df_x1std)

## Marginal for x2
### Unscaled
x2_range <- range(sim_df$x2)
pred_df_x2 <- expand.grid(
	x1 = mean(sim_df$x1)
	, x2 = seq(x2_range[1], x2_range[2], length.out = rsteps)
)
head(pred_df_x2)

### Scaled
x2std_range <- range(sim_df$x2std)
pred_df_x2std <- expand.grid(
	x1std = mean(sim_df$x1std)
	, x2std = seq(x2std_range[1], x2std_range[2], length.out = rsteps)
)
head(pred_df_x2std)

saveVars(pred_df_x1, pred_df_x1std, pred_df_x2, pred_df_x2std)
