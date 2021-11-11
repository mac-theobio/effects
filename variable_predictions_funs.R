## More general simulation function
### Default: Continuous outcome 2 and 1 categorical predictors
### form : specify model using formula

linearsim <- function(nHH=1000, perHH=1, form=~1+x1+x2+x3
	, hhSD=2, betas=NULL, pgausian=list(p=2, fun=rnorm, mean=0, sd=1)
	, pcat=list(p=1, fun=sample, nlevels=2, labels=NULL, prob=NULL, replace=TRUE)
	, noutcomes=1, separatelatent=FALSE, blatent=1, mulatent=0
	, sdlatent=1, vnames=NULL, link_scale=FALSE) {
	
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
		labels_temp <- pcat$labels
		prob_temp <- pcat$prob
		for (nl in 1:cp) {
			if(is.list(labels)) {
				labels <- labels_temp[[nl]]
				prob <- prob_temp[[nl]]
			} else {
				labels <- labels_temp
				prob <- prob_temp
			}
			if (nlevels[[nl]]<2)stop("Levels of categorical variable must be >1")
			if (is.null(labels)) {
				levs <- sample(LETTERS, nlevels[[nl]], replace=FALSE)
			} else {
				levs <- labels
				if (is.list(levs)) levs <- levs[[nl]]
			}
			if (length(levs) != nlevels[[nl]])
				stop("nlevels must be equal to the length of labels")
			pcat$labels <- NULL
			pcat$x <- levs
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
		if (separatelatent) {
			if (length(mulatent)==1) mulatent <- rep(mulatent, noutcomes)
			if (length(sdlatent)==1) sdlatent <- rep(sdlatent, noutcomes)
			if (length(blatent)==1) blatent <- rep(blatent, noutcomes)
		}
		out <- lapply(1:noutcomes, function(i){
			if (separatelatent) {
				latentvar <- blatent[[i]]*rnorm(N, mulatent[[i]], sdlatent[[i]])
			} else {
				latentvar <- 0
			}
			hhRE <- pull(hhRE, paste0("hhRE", i))
			eta <- lp + hhRE + latentvar
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
binfun <- function(mod, focal, bins=50, groups=NULL, ...) {
	if (!is.null(groups)) {
		bins_all <- c(groups, "bin")
	} else {
		bins_all <- "bin"
	}
	mf <- model.frame(mod)
	check_df <- (mf
		%>% arrange_at(focal)
		%>% mutate(bin=ceiling(row_number()*bins/nrow(.)))
		%>% group_by_at(bins_all)
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


## Compare predictions from vareffects, emmeans, effects and margins
## funs: emmean; varpred; ...
combinepreds <- function(mod, funs, focal, x.var, x.var.factor=FALSE, plotit=TRUE, print.head=FALSE, ...){
	if (length(focal)>1) {
		focal <- union(focal[focal %in% x.var], focal)
		focal_temp <- c("xvar", focal[!focal %in% x.var])
		spec <- as.formula(paste0("~", paste0(focal, collapse=":")))
	} else {
		spec <- as.formula(paste0("~", focal))
		focal_temp <- "xvar"
	}
	args <- list(mod, spec=spec
		, focal.predictors=focal
		, focal_predictors=focal
		, x.var=x.var
	)
	add_args <- list(...)
	if (length(add_args)) args[names(add_args)] <- add_args

	out <- sapply(funs, function(f){
		est <- do.call(f, args)
		if (inherits(est, c("emmeans", "emmGrid"))) {
			type <- est@misc$predict.type
			est <- as.data.frame(est)
			if (length(type) && type=="response") {
				.nn <- "prob"
			} else {
				.nn <- "emmean"
			}
			oldn <- c(focal, .nn
				, grep("\\.CL|\\.LCL|\\.UCL", colnames(est), value=TRUE)
			)
			newn <- c(focal_temp, "fit", "lwr", "upr")
			colnames(est)[colnames(est) %in% oldn] <-  newn
		} else if (inherits(est, "eff")) {
			est <- as.data.frame(est)
			oldn <- c(focal, "lower", "upper")
			newn <- c(focal_temp, "lwr", "upr")
			colnames(est)[colnames(est) %in% oldn] <-  newn
		} else {
			est <- as.data.frame(est)
			colnames(est)[colnames(est) %in% x.var] <- "xvar"
		}
		est <- est[, c(focal_temp, c("fit", "lwr", "upr"))]
		est$model <- f
		return(est)
	}, simplify = FALSE)
	out <- do.call("rbind", out)
	rownames(out) <- NULL
	class(out) <- c("jdeffects", "data.frame")
	if (print.head){
		head(out)
	}
	if (plotit) {
		cols <- rainbow(length(funs))
#		colfun <- colorRampPalette(c("black", "red"))
#		cols <- colfun(length(funs))
		names(cols) <- funs
		out <- list(out)
		out[names(add_args)] <- add_args
		p1 <- (do.call(vareffects:::plot.vareffects, out)
			+ scale_colour_manual(breaks = funs, values=cols)
			+ labs(y="Predictions", x=x.var, colour="Method")
			+ theme(legend.position="bottom")
		)
		if (!x.var.factor) {
			pred_prop_df <- (out[[1]]
				%>% group_by_at(c("model", focal[!focal %in% x.var]))
				%>% summarize(fit=mean(fit))
				%>% data.frame()
			)
			print(pred_prop_df)
			out <- p1 + geom_hline(data=pred_prop_df, aes(yintercept=fit, colour=model), lty=2)
		} else {
			out <- p1
		}
	}
	return(out)
}


saveEnvironment()
