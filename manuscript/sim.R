library(shellpipes)
library(dplyr)

loadEnvironments()

set.seed(seed)

## Perfectly independent, normalized RVs (to perfectly match desired correlation, and probably partial correlations as well)
rx <- rnorm(N)
ry <- rnorm(N)
rw <- rnorm(N)
rz <- rnorm(N)

dy <- residuals(lm(ry~rx))
dw <- residuals(lm(rw~rx))

nx <- (rx-mean(rx))/sd(rx)
ny <- (dy-mean(ry))/sd(dy)
nw <- (dw-mean(rw))/sd(dw)
nz <- (rz-mean(rz))/sd(rz)

sim_df <- tibble(NULL
	, x = nx+xbar
	, y = rho*nx + sqrt(1-rho^2)*ny+ybar
	, w = nw
	, lc = b0 + bx*x + by*y + bw*w
	, z = lc+nz
)

summary(sim_df)

with(sim_df, cor.test(x, y))
with(sim_df, cor.test(x, w))

saveVars(sim_df)
