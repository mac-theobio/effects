library(ggplot2); theme_set(theme_bw())
library(ggpubfigs)
library(vareffects)
library(dplyr)

library(shellpipes)

## Notes
## Is it worth doing this?
## If so, figure out how to make plots comparable, and put them side-by-side
## Make w really orthogonal so that the fit is the same??

set.seed(2113)
N <- 25
xbar <- 0
ybar <- 0
sig <- 1
rhovals <- seq(0.1, 0.9, length.out=81)

## Perfectly independent, normalized RVs (to perfectly match desired correlation, and probably partial correlations as well)
rx <- rnorm(N)
ry <- rnorm(N)
rw <- rnorm(N)

dy <- residuals(lm(ry~rx))
dw <- residuals(lm(rw~rx))

dy <- sig*dw+dy

nx <- (rx-mean(rx))/sd(rx)
ny <- (dy-mean(dy))/sd(dy)
nw <- (dw-mean(dw))/sd(dw)

for (rho in rhovals){
	sim_df <- tibble(NULL
		, x = nx+xbar
		, y = rho*nx + sqrt(1-rho^2)*ny + ybar
		, w = nw
	)
	m <- lm(y ~ x, data=sim_df)
	mods <- (list(
		varpred(m, "x", modelname="effect")
		, varpred(m, "x", isolate=FALSE, modelname="pred")
	) %>% combinevarpred)
	print(plot(mods)
		+ scale_color_manual(
			values = c("black", friendly_pal("contrast_three")[2:3])
		)
		+ labs(title="uni", subtitle=rho)
	)
}

for (rho in rhovals){
	sim_df <- tibble(NULL
		, x = nx+xbar
		, y = rho*nx + sqrt(1-rho^2)*ny+ybar
		, w = nw
	)
	m <- lm(y ~ x+w, data=sim_df)
	mods <- (list(
		varpred(m, "x", modelname="effect")
		, varpred(m, "x", isolate=FALSE, modelname="pred")
	) %>% combinevarpred)
	print(plot(mods)
		+ scale_color_manual(
			values = c("black", friendly_pal("contrast_three")[2:3])
		)
		+ labs(title="multi", subtitle=rho)
	)
}

