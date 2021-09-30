## More general simulation function
### Default: Continuous outcome 2 and 1 categorical predictors
### form : specify model using formula

linearsim <- function(N=1000, form=~1+x1+x2+x3
	, betas=NULL, pgausian=list(p=2, fun=rnorm, mean=0, sd=1)
	, pcat=list(p=1, fun=sample, nlevels=2, labels=NULL, prob=NULL, replace=TRUE)
	, vnames=NULL, link_scale=FALSE) {
	
	Terms <- attr(terms(form), "term.labels")
	nbetas <- length(Terms)
	if (!is.null(betas) & length(betas) != (nbetas+1))
		stop(paste0("betas should a vector of length ", nbetas+1))

	p <- pgausian$p + pcat$p
	nvars <- length(all.vars(form))

	if (p != nvars) {
		stop(paste0("\nNumber of predictors in form should be same as sum of p in pgausian and pcat: ", nvars, " != ", p))
	}

	mu <- pgausian$mean
	.sd <- pgausian$sd
	pg <- pgausian$p
	pgausian$p <- NULL
	gfun <- pgausian$fun
	pgausian$fun <- NULL
	pgausian$n <- N
	if (length(mu)==1) {
		X <- replicate(pg, do.call(gfun, pgausian))
	} else {
		X <- list()
		if (length(mu)>pg)stop("p in pgausian should be greater than or equals to the length of mean")
		for (m in 1:length(mu)) {
			if(length(.sd)<length(mu)).sd <- 1 else .sd <- .sd[[m]]
			pgausian$mean <- mu[[m]]
			pgausian$sd <- .sd
			X[[m]] <- do.call(gfun, pgausian)
		}
		X <- do.call("cbind", X)
	}
	
	cp <- pcat$p
	pcat$p <- NULL
	if (cp > 0) {
		nlevels <- pcat$nlevels
		if (length(nlevels)!=cp)
			stop("\n p in pcat must be equal to the length of nlevels")
		Xcat <- list()
		cfun <- pcat$fun
		for (nl in 1:cp) {
			if (nlevels[[nl]]<2)stop("Levels of categorical variable must be >1")
			labels <- pcat$labels
			if(is.list(labels)) labels <- labels[[nl]]
			if (is.null(labels)) {
				levs <- sample(LETTERS, nlevels[[nl]], replace=FALSE)
			} else {
				levs <- labels
			}
			if (length(levs) != nlevels[[nl]])
				stop("nlevels must be equal to the length of labels")
			pcat$labels <- NULL
			pcat$x <- levs
			prob <- pcat$prob
			if(is.list(prob)) prob <- prob[[nl]]
			if (is.null(prob)) {
				prob <- rep(1/nlevels[[nl]], nlevels[[nl]])
			}
			if (length(prob) != length(levs))
				stop("length of prob must be equal to the length of labels")

			pcat$fun <- NULL
			pcat$prob <- prob
			pcat$nlevels <- NULL
			pcat$size <- N
			Xcat[[nl]] <- do.call(cfun, pcat)
		}
		Xcat <- do.call("cbind", Xcat)
		X <- cbind.data.frame(X, Xcat)
	}
	colnames(X) <- all.vars(form)
	
	## Model matrix
	df <- as.data.frame(X)
	X <- model.matrix(form, df)
	
	if (is.null(betas)) betas <- log(runif(NCOL(X), 0, 1))

	eta <- as.vector(X %*% betas)
	if (link_scale) {
		df$y <- rnorm(N, mean=eta, sd=1) 
	} else {
		df$y <- rbinom(N, 1, plogis(eta))
	}
	
	if (is.null(vnames)) {
		colnames(df) <- c(all.vars(form), "y")
	} else {
		colnames(df) <- vnames
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
