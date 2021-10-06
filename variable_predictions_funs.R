## More general simulation function
### Default: Continuous outcome 2 and 1 categorical predictors
### form : specify model using formula

linearsim <- function(nHH=1000, perHH=1, form=~1+x1+x2+x3
	, hhSD=2, betas=NULL, pgausian=list(p=2, fun=rnorm, mean=0, sd=1)
	, pcat=list(p=1, fun=sample, nlevels=2, labels=NULL, prob=NULL, replace=TRUE)
	, noutcomes=1, vnames=NULL, link_scale=FALSE) {
	
	Terms <- attr(terms(form), "term.labels")
	nbetas <- length(Terms)
	if (!is.null(betas) & length(betas) != (nbetas+1))
		stop(paste0("betas should a vector of length ", nbetas+1))

	p <- pgausian$p + pcat$p
	nvars <- length(all.vars(form))

	if (p != nvars) {
		stop(paste0("\nNumber of predictors in form should be same as sum of p in pgausian and pcat: ", nvars, " != ", p))
	}

	if (noutcomes<1)stop("Number of outcome variables should be  >= 1")
	if (perHH < 1)stop("perHH >= 1")
	N <- nHH * perHH

	pg <- pgausian$p
	mu <- pgausian$mean
	if (is.null(mu)) mu <- rep(0, pg)
	.sd <- pgausian$sd
	if(is.null(.sd)) sd <- 1

	pgausian$p <- NULL
	gfun <- pgausian$fun
	if (is.null(gfun)) gfun <- rnorm
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
		if (is.null(nlevels)) {
			nlevels <- rep(2, cp)
			pcat$labels <- NULL
			pcat$prob <- NULL
			pcat$replace <- TRUE
		}
		if (length(nlevels)!=cp)
			stop("\n p in pcat must be equal to the length of nlevels")
		Xcat <- list()
		cfun <- pcat$fun
		if (is.null(cfun)) cfun <- sample
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
	
	## Generate random effects
	### Probably better way to do this in base R
	if (perHH==1) {
		hhid <- 1:N
		hhRE <- rep(0, N)
	} else {
		hhid <- rep(1:nHH, each=perHH)
		if (length(hhSD)==1) hhSD <- rep(hhSD, noutcomes)
		names(hhSD) <- paste0("hhRE", 1:noutcomes)
		df <- (df
			%>% mutate(hhid=hhid)
			%>% group_by(hhid)
			%>% mutate_(.dots=hhSD)
			%>% mutate_at(names(hhSD), function(x)rnorm(1, 0, x))
			%>% ungroup()
		)
		hhid <- pull(df, hhid)
		hhRE <- select(df, !!names(hhSD))
		df <- select(df, -!!c("hhid", names(hhSD)))
	}

	X <- model.matrix(form, df)
	
	if (is.null(betas)) betas <- log(runif(NCOL(X), 0, 1))

	lp <- as.vector(X %*% betas)
	if (noutcomes==1) {
		if (perHH==1) {
			hhRE <- hhRE 
		} else {
			hhRE <- pull(hhRE, "hhRE1")
		}
		eta <- lp + hhRE
		if (link_scale) {
			df$y <- rnorm(N, mean=eta, sd=1) 
		} else {
			df$y <- rbinom(N, 1, plogis(eta))
		}
		ynames <- "y"
	} else {
		out <- lapply(1:noutcomes, function(i){
			hhRE <- pull(hhRE, paste0("hhRE", i))
			eta <- lp + hhRE
			if (link_scale) {
				y <- rnorm(N, mean=eta, sd=1) 
			} else {
				y <- rbinom(N, 1, plogis(eta))
			}
			return(y)
		})
		out <- do.call("cbind", out)
		df <- cbind.data.frame(df, out)
		ynames <- paste0("y", 1:noutcomes)
	}
	
	
	if (is.null(vnames)) {
		colnames(df) <- c(all.vars(form), ynames)
	} else {
		colnames(df) <- vnames
	}
	df$hhid <- hhid
	
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

## Compare several varpred objects

comparevarpred <- function(vlist, lnames=NULL, plotit=FALSE, addmarginals=FALSE, margindex, ...) {
	if (!is.list(vlist))stop("vlist should be a list of objects of class varpred")
	nobjs <- length(vlist)
	if (!is.null(lnames)) {
		if (nobjs==1) lnames <- rep(lnames, nobjs)
	} 
	preds <- lapply(1:nobjs, function(v){
		pp <- vlist[[v]]$preds
		if (!is.null(lnames)) {
			pp$.varpred <- lnames[[v]]
		}
		return(pp)
	})
	preds <- do.call("rbind", preds)
	if (addmarginals) {
		if (missing(margindex))stop("Specify margindex of vlist objects to compute marginals")
		marg_df <- lapply(margindex, function(i){
			pp <- vlist[[i]]$preds
			df <- data.frame(muy=mean(pp$fit)
				, mux=mean(pp[[attr(pp, "x.var")]])
			)
			if (!is.null(lnames)) {
				df$.varpred <- lnames[[i]]
			}
			return(df)
		})
		marg_df <- do.call("rbind", marg_df)
	}
	
	out <- vlist[[1]]
	out$preds <- preds
	if (plotit) {
		add_args <- list(...)
		if (length(add_args)) {
			out <- list(out)
			out[names(add_args)] <- add_args
			p <- do.call(plot, out)
		} else {
			p <- plot(out)
		}
		if (addmarginals) {
			p <- (p
				+ geom_hline(data=marg_df, aes(yintercept=muy), lty=2, colour="grey")
				+ geom_vline(data=marg_df, aes(xintercept=mux), lty=2, colour="grey")
			)
		}
		return(p)
	} else {
		return(out)
	}
}
saveEnvironment()
