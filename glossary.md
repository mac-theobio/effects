
scientific variables: "input variables"?

statistical variables: "model variables"?

**Anchor** The value of the focal variables (input or model) used to calculate CIs

Background: how we average over non-focal variables
* Reference point
* Quantiles
* Population average

Model center

## Simple linear models and GLMs

**Marginal effects** - Expected changes in the outcomes averaged across the population.

**Expected marginal means** - Predicted outcomes averaged across the population.

**Marginal predictions** - In simple or GLMs, these are predictions based on marginal means.

**Model averaging** - In linear models, averaging across the population is equivalent to making the appropriate _conditional_ predictions, i.e. at the model center or the average/margin of some level (e.g. the prediction for the average male, where we take either the center of the age distribution (unweighted average) or the mean age (weighted) [or median or some other 'center'].  The hard part is deciding what should be the 'model center'. In nonlinear models (in particular GLMs) need to decide whether you want the average on the link scale (log/logit/whatever) or the response scale (probability/counts/etc.) and adjust accordingly (**BMB**). ([Norton, Edward C., Bryan E. Dowd, and Matthew L. Maciejewski](https://doi.org/10.1001/jama.2019.1954))

## GLMMs

**Marginal coefficients (or effect  or predictions)** - Similar to GLMs but incorporates the variation across the random effects rather than in fixed effects. 

**Marginal vs conditional models** - Estimating by fitting
coefficients (and response curves) that condition on an individual
(conditional: "what is the expected increase in risk for an individual
in a particular city for an additional year of age?" vs marginal: "what
is the expected difference in risk from two individuals sampled from the
population who differ in age by one year"? ([Sergio de-Miguel, Lauri Mehtätalo, Zuheir Shater, Bassel Kraid, and Timo Pukkala](https://doi.org/10.1139/x2012-090))

**Marginal vs conditional predictions** - The main distinction between marginal and conditional predictions depends on whether the resulting predictions describe the individual's or the population-average response to changing covariates. In particular, conditional predictions results from conditional models which describe how the change in one covariate affects the outcome within a particular group, while holding the other covariates and all random effects constant. On the other hand, marginal predictions result from population-averaged models and are appropriate when predictions on the population-level is desired, irrespective of potential intergroup differences. For example marginal prediction for gender compares the mean of women to that of female, while conditional prediction compares the mean among female to that of women holding the same value that corresponds to each individual (random effect).

**Simple linear models case** - For a simple linear model, the choice between a marginal and conditional formulation is equivalent [Stefanie Muff  Leonhard Held  Lukas F. Keller](https://doi.org/10.1111/2041-210X.12623).


## More general

**Focal predictor** - A predictor whose predictions are computed, otherwise _non-focal_. The values of _non-focal_ predictors are held at typical values --- typically determined by averaging in some meaningful way.

**Zeroing-out variance-covariance matrix** - Eliminating uncertainties due to non-focal predictors.

**Variables vs model matrix** - Transformations (centering, scaling or both, or averaging) can be applied to either the observed variables or to the columns of the generated model matrix. If all the variables are continuous, transforming the observed variables is straightforward otherwise, in the presence of categorical variables, we need to first compute class proportions (yet to figure this out). Current implementation uses model matrix.

## References 

- [Sergio de-Miguel, Lauri Mehtätalo, Zuheir Shater, Bassel Kraid, and Timo Pukkala](https://doi.org/10.1139/x2012-090)

- [Stefanie Muff  Leonhard Held  Lukas F. Keller](https://doi.org/10.1111/2041-210X.12623)

- [Norton, Edward C., Bryan E. Dowd, and Matthew L. Maciejewski](https://doi.org/10.1001/jama.2019.1954)
