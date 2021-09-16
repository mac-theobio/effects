
Plan: start simple, make clear pictures and explain them as if we are teaching. This will help us notice when we reach a place that we don't understand.

# Predictors vs. variables

A predictor is what Ben would call an input variable; the model has predictors.

A predictor is associated with one or more _variables_ (polynomials, splines and multi-categories associate >1 variable with a predictor; the model _matrix_ has variables.

The model center is found by averaging the variables over the model matrix. The model center is a point in variable space, but is not necessarily a point in predictor space.

# Starting point: simple, multivariate regression

What do we need?
* A way of summarizing non-focal predictors
	* Average at the parameter level (probably bad)
	* Average at the variable level
	* Average outcomes over a population of points

* A range of the focal predictor (up to us, easy)

* A decision about what variation to incorporate
	* Traditional CIs show variation due to intercept term and other non-focal sources
	* We usually want to show focal effects only; this requires an _anchor_
		* The natural anchor is the model-center values of focal-predictor associated variables.
		* Can we explain this convincingly?
		* If we use the natural anchor, the effect CIs will come together for a simple focal predictor, but not for a structured focal predictor
		* Can we explain what our CIs _mean_ in the structured case?

# Model structure

* Interactions between non-focal predictors
	* This is probably trivial; it's just another column in the model matrix and we don't care

* Interactions involving the focal predictor (!!)

# Link functions

* Model-center non-focal parameters lead to correctable bias
	* Average over outcomes?
	* Second-order correction

* Are there issues arising from structured focal parameters that affect bias either in the predicted effect or in the CIs?
