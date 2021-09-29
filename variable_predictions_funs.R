
## Simulate data for simple lm or glm
### If link_scale=TRUE -> lm otherwise glm
simplesim <- function(N=1e4, beta0=1.5, beta1=1.0, beta2=2
		, x1_sd=1, x1_mean=0.2
		, x2_sd=1, x2_mean=0
		, link_scale = FALSE
	) {
	x1 <- rnorm(N, x1_mean, x1_sd)
	x2 <- rnorm(N, x2_mean, x2_sd)
	eta <- beta0 + beta1 * x1 + beta2 * x2
	if (link_scale) {
		eta <- eta + rnorm(N) 
	}
	sim_df <- data.frame(x1=x1, x2=x2, y=eta)
	if (!link_scale) {
		sim_df <- (sim_df
			%>% mutate(y = rbinom(N, 1, plogis(y)))
		)
	}
	return(sim_df)
}

## Simulate data for lm and glm with interactions
interactionsim <- function(N=1e4, beta0=1.5, beta1=1.0
		, beta2=2, beta3=1.5, beta23=1
		, x1_sd=1, x1_mean=0.2
		, x2_sd=1, x2_mean=0
		, x3_sd=1, x3_mean=0
		, link_scale = FALSE
		, form = ~ 1 + x1 + x2 + x3 + x2:x3
	) {
	betas <- c(beta0, beta1, beta2, beta3, beta23)
	x1 <- rnorm(N, x1_mean, x1_sd)
	x2 <- rnorm(N, x2_mean, x2_sd)
	x3 <- rnorm(N, x3_mean, x3_sd)
	df <- data.frame(x1=x1, x2=x2, x3=x3)
	X <- model.matrix(form, df)
	eta <- as.vector(X %*% betas)
	if (link_scale) {
		eta <- eta + rnorm(N) 
	}
	df$y <- eta 
	if (!link_scale) {
		df <- (df
			%>% mutate(y = rbinom(N, 1, plogis(y)))
		)
	}
	return(df)
}

## More general simulation function
### Default: Continuous outcome + 2 predictors
### form : specify model using formula
### addvars: add (matrix) customized predictors from other distributions
linearsim <- function(N=1000, form=NULL
	, betas=NULL, contvars=rnorm(N,0.2,1)
	, catvars=sample(c("A", "B"), N, c(0.5,0.5), replace=TRUE)
	, link_scale=FALSE, vnames=NULL) {
	if (is.null(form)) {
		nbetas <- NCOL(contvars) + NCOL(catvars) 
		form <- as.formula(paste0("~1+",paste0("x", 1:nbetas, collapse="+")))
	} else {
		Terms <- attr(terms(form), "term.labels")
		nbetas <- length(Terms)
	}
	contvars <- as.matrix(contvars, ncol=NCOL(contvars))
	catvars <- as.matrix(catvars, ncol=NCOL(catvars))
	X <- cbind.data.frame(contvars, catvars)
	if (is.null(vnames)) {
		colnames(X) <- all.vars(form)
	} else {
		colnames(X) <- vnames
	}
	
	## Model matrix
	df <- as.data.frame(X)
	X <- model.matrix(form, df)
	
	if (!is.null(betas) & NCOL(X) != length(betas))
		stop(paste0("betas should a vector of length ", NCOL(X)))
	if (is.null(betas)) betas <- log(runif(NCOL(X), 0, 1))

	eta <- as.vector(X %*% betas)
	if (link_scale) {
		df$y <- rnorm(N, mean=eta, sd=1) 
	} else {
		df$y <- rbinom(N, 1, plogis(eta))
	}
	return(list(data = df, betas = betas, formula=form))
}


## Generate binned observations
binfun <- function(mod, focal, non.focal, bins=50, ...) {
	mf <- model.frame(mod)
	mm <- (mf
		%>% select_at(c(focal, non.focal))
	)
	check_df <- (mf
		%>% arrange_at(focal)
		%>% mutate(bin=ceiling(row_number()*bins/nrow(.)))
		%>% group_by(bin)
		%>% summarise_all(mean)
		%>% mutate(model="binned")
	)
	return(check_df)
}

saveEnvironment()
