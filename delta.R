deltaapprox <- function(fun, var="x", z, ...) {
	ff <- as.expression(substitute(fun))
	der <- D(ff, var)
	x <- z
	mu <- eval(ff, list(x))
	x <- mean(z)
	der <- eval(der, list(x))
	y <- mu + der * (z - mean(z))
	return(y)
}

ss <- function (mu, sigma, abs.tol = 0, ...) {
    fExp <- function(x) plogis(x) * dnorm(x, mean = mu, sd = sigma)
    .exp <- integrate(fExp, -Inf, Inf, abs.tol = abs.tol, ...)$value
    fVar <- function(x) (plogis(x) - .exp)^2 * dnorm(x, mean = mu,
        sd = sigma)
    .var <- integrate(fVar, -Inf, Inf, abs.tol = abs.tol, ...)$value
    c(mean = .exp, var = .var)
}

taylorapprox <- function(fun, var="x", mu, sigma, ...) {
	ff <- as.expression(substitute(fun))
	x <- mu
	untrans <- eval(ff, list(x))
	der <- D(ff, var)
	der2 <- D(der, var)
	der2 <- eval(der2, list(x))
	corrected <- untrans + der2*sigma^2/2
	return(corrected)
}

z <- rnorm(100)
ss(mean(z), sd(z))
dd <- deltaapprox(fun=1/(1 + exp(-x)), z=z)
mean(dd)
mean(plogis(z))
plogis(mean(z))
emdbook::deltamethod(1/(1+exp(-x)), z)
logitnorm::momentsLogitnorm(mu= mean(z), sigma = sd(z))
taylorapprox(1/(1+exp(-x)), mu= mean(z), sigma = sd(z))
