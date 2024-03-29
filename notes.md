2021 Sep 16 (Thu)
=================

- We should (or a attempt to) provide a clear definition of the following:
	- Input variables: Scientific variables underlying an inference or exploration. The focal predictor we use for effects or predictions is an input variable.
	- Model variables: Variables that represent columns in the model matrix. Each input variable will correspond to one or more model variables. In particular, variables with more than two categories, or variables modeled with a spline or polynomial response, will correspond to more than one model variables.
	- Model center: A point corresponding to a column-wise mean of the model matrix (the mean of one or more model variables). The center point for a set of model variables corresponding to an input variable may not represent a possible value of the input variable.
	- Reference point: Value or values chosen for _non-focal_ predictors}, when estimating effects. Typically the center point, but can instead be a population of quantiles or observations.
	- Anchor: The value chosen for the _focal predictor_ when estimating effect confidence intervals (anchor choice does not affect the estimate). Typically chosen as the center point of the model variables corresponding to the focal input variable.
- We can possible define macro in the text for future changes in the choice of terminology. We can edit **structure.md** for the definitions or maybe add them directly to the **variable_predictions.Rmd**

- We should document what the central line, the outer curves and the cross (or isolated) curves means.

- **BC** should provide a mathematical formulation of the model for Figure 3 together with the corresponding analytical `E(.)` to justify why or not the yellow and grey vertical lines should be different. Also, look at the back-end code for any _bugs_.

- In future, think about a clear example to justify our proposed method.





2021 Sep 16 (Thu)
=================

JD should edit structure.md. CB should do that, too!

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


