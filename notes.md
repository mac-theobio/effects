
We'll discuss git_push/effects_writeup.pdf in effects repo and also jdeffects in the same repo.

To jdeffects
make build-package && install-package

2020 Nov 25 (Wed)
=================

- Unscaling parameters fitted on scaled predictors: considering a simple case where there are no interactions
	- If only the predictors are scaled
		- unscaled slopes = scaled parameter divided by the scale (scale here is the scale used in scaling the predictors)
		- unscaled intercept = scaled intercept minus the sum of (scaled slopes times the center used in scaling of the predictors)
	- If both the response and predictors are scaled
		- scaled slopes: perform the same operations as in the above case but also multiply by the scale of the response (scaling).
		- scaled intercept: similar to above (only scaled predictors) can but also multiply by the scale and add the center of the response (scaling).
	- We can write down the maths and scale unscaled parameters too

- Scaling covariance matrix: 
	- Let $f(\beta)$ be the transformation function and $\beta_0, \beta_1, \cdots, \beta_p$ be a vector of predictors. We want to compute a linear approximation of $f(\beta)$ around some value $\hat{\beta}$.
	- By treating the $\beta$s as random functions, we can compute partial derivatives of $f(\beta)$ at the point $\hat{\beta}$ and then take the variance of this approximation to estimate the variance of $\f{\beta}$.
- How does the variance-covariance matrix change if parameters are scaled?
	- Consider a linear transformation $AX + b$, where $A$ is the transformation matrix and $X$ is the variance-covariance matrix. Then
		- $var(AX + b) = A\Sigma A^T$. 
			- Steve: I can see that the intercept term vanishes here and I remember BB mentioned that it does not change. But from my simple experiment, I found that we can actually get the scaled values for the slope using this relation. However, the intercept did change. Did I miss anything?


