% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effectsfuns.R
\name{varpred}
\alias{varpred}
\title{Conditional and marginal predictions}
\usage{
varpred(
  mod,
  focal_predictors,
  x.var = NULL,
  type = c("response", "link"),
  isolate = FALSE,
  isolate.value = NULL,
  level = 0.95,
  steps = 101,
  at = list(),
  dfspec = 100,
  vcov. = NULL,
  internal = FALSE,
  avefun = mean,
  zero_out_interaction = FALSE,
  returnall = FALSE
)
}
\arguments{
\item{mod}{fitted model object. See details for supported class of models.}

\item{focal_predictors}{a character vector of one or more predictors. For models with interaction, the marginal predictions are obtained by specifying the corresponding predictors together. For example \code{~x1*x2} is specified as \code{c("x1", "x2")} to obtain the marginal prediction for \code{x1} with respect to \code{x2}. If no interactions are present in the model, specifying more than one predictors compares predictions between the predictors.}

\item{x.var}{a character specifying the predictor to define the x variable (horizontal axis on the plot). The default is \code{NULL}, of which the first predictor in \code{focal_predictors} is used.}

\item{type}{a character specifying the desired prediction. \code{type = "response"} applies inverse transformation, if exists. \code{type = "link"} requests the results as a linear predictor.}

\item{isolate}{logical. If \code{TRUE}, the \code{SEs} are centered around the mean value of \code{x.var}. By default, it is the deviation of the predictor value from its mean but other values can be specified through \code{isolate.value}.}

\item{isolate.value}{numeric (default \code{isolate.value = NULL}). If \code{isolate = TRUE}, otherwise ignored, is the deviation from the mean \code{x.var}. If \code{NULL} and \code{isolate = TRUE}, the resulting predictions are equivalent to that of model fitted with scaled predictor.}

\item{level}{desired confidence interval for computing marginal predictions. Default is \code{0.95}.}

\item{steps}{number of points to evaluate numerical predictors in \code{focal_predictors}. The default is \code{100}.}

\item{at}{default \code{NULL}. Otherwise, is a named \code{list} specifying points to evaluate \code{focal_predictors}. The names in the list should match the names used in \code{focal_predictors}.}

\item{dfspec}{default \code{100}. Specified degrees of freedom for model which do not return \code{df}. This is used in computation of confidence intervals.}

\item{vcov.}{a function or a matrix. If a function, it is used to compute the variance-covariance matrix of the model coefficients. The function should take model as it's first (or maybe only) argument. A matrix of variance-covariance matrix of the estimated coefficient can also be used. Otherwise \code{vcov(mod)} is used internally. Specifying \code{vcov.} Is important when "zeroed-out" predictions are required. However, with this approach, the predictors should be centered. {isolate=TRUE} marginalizes without requiring zeroing-out. See examples.}

\item{internal}{logical. If \code{TRUE}, the entries of the non-focal predictor (see x.var) in the variance-covariance matrix are internally zeroed-out using \code{\link[vareffects]{zero_vcov}}. Default is \code{FALSE}.}

\item{avefun}{the averaging scheme (function) to be used in conditioning non-focal predictors. Default is \code{mean}.}

\item{zero_out_interaction}{logical. If \code{TRUE} the uncertainty as a result of interaction terms are removed (set to zero) when \code{ignored if isolate = FALSE}. Only main effect predictions are computed.}

\item{returnall}{logical. If \code{TRUE}, all other named computed quantities are also returned. Otherwise, only predictions are returned.}
}
\description{
Computes conditional and marginal prediction for supported simple and generalized linear models.
}
\details{
The main distinction between the two predictions lies on how the standard errors (SEs) are computed. In conditional predictions, non-focal predictors are conditioned (by some meaningful averaging) and standard errors computed using "full" variance-covariance matrix. However, in marginal predictions, the uncertainities relating to non-focal predictors are isolated (removed) using some meaningful way -- discussed somewhere else.
}
\examples{

# Set theme for ggplot. Comment out if not needed
varefftheme()
set.seed(101)
N <- 100
x1_min <- 1
x1_max <- 9
b0 <- 0.3
b1 <- 0.1
b2 <- -0.6
b3 <- 0.01
x2_levels <- factor(c("A", "B", "D"))
df <- expand.grid(x1u = runif(n=N, min=x1_min, max=x1_max)
	, x2u = x2_levels
)
X <- model.matrix(~x1u + x2u, df)
betas <- c(b0, b1, b2, b3)
df$y <- rnorm(nrow(df), mean= X \%*\% betas, sd=1)
df2 <- df
df <- transform(df
	, x1c = drop(scale(x1u, scale=FALSE))
)
head(df)

# Unscaled model
m1u <- lm(y ~ x1u + x2u, df)
# Conditional prediction of x1u
pred1u <- varpred(m1u, "x1u")
plot(pred1u)

# Scaled model (x1 centered = x1 - mean(x1))
m1c <- lm(y ~ x1c + x2u, df)
# Conditional prediction of x1u
pred1c <- varpred(m1c, "x1c")
plot(pred1c)

# Marginal predictions
# We can get marginal predictions from unscaled model similar to m1c
# Using zero_vcov by specifying vcov.
vv <- zero_vcov(m1c, "x1c")
pred2c <- varpred(m1c, "x1c", vcov. = vv)
plot(pred2c)

# Using mean centering (isolate)
pred3c <- varpred(m1u, "x1u", isolate = TRUE)
plot(pred3c)
all.equal(pred2c$pred[,-1], pred3c$pred[,-1], check.attributes = FALSE)

# Compare across groups
pred4c <- varpred(m1c, c("x1c", "x2u"), x.var = "x1c", isolate = TRUE, zero_out_interaction = TRUE)
plot(pred4c)

}
\seealso{
\code{\link[vareffects]{plot.vareffects}}
}
