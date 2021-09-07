predictionfun <- function(mod, focal, level=0.95, steps=100, isolate=FALSE, include.nonfocal=c("jd", "bb"), modelname="none", ...) {
	
	include.nonfocal <- match.arg(include.nonfocal)
	mm <- model.matrix(mod)
	quant <- seq(0, 1, length.out=steps)
	xvals <- as.vector(quantile(mm[, focal], quant))
	vnames <- vareffects:::get_vnames(mod)$termnames
	check <- !vnames %in% focal
	non.focal.terms <- vnames[check]
	focal.terms <- vnames[!check]
	betas <- coef(mod)
	non.focal.coefs <- betas[check]
	focal.coefs <- betas[!check]
	
	lp.non.focal <- as.vector(as.matrix(mm[, non.focal.terms, drop=FALSE]) %*% non.focal.coefs)
	lp.focal <- as.vector(as.matrix(xvals) %*% focal.coefs)
	
	cutpts <- c(-Inf, (xvals[-1] + xvals[-steps])/2, Inf)
	lp.split <- split(lp.non.focal, cut(xvals, cutpts))
#	lp.split <- split(lp.non.focal, xvals)

	## Compute std error
	### Centre model matrix (around mean)
	col_mean <- apply(mm, 2, mean)
	mm2 <- mm
	if (isolate) {
		col_mean[check] <- 0
		mm2[] <- 0
#		mm2[, focal.terms] <- mm[, focal.terms]
		mm2[1:steps, focal.terms] <- xvals
	}
	z.val <- qnorm(1 - (1 - level)/2)
	pse_var <- stderrfun(mod, mm2, col_mean, isolate = isolate)
	pse_var <- z.val*pse_var
	
	
	est <- lapply(1:length(lp.split), function(i){
		if (!isolate) {
			mm2[, focal.terms] <- xvals[[i]]
			pse_var <- stderrfun(mod, mm2, col_mean, isolate = isolate)
			pse_var <- z.val*pse_var
		} else {
			pse_var <- pse_var[[i]]
		}
		
		## How to include non focal lp??
		if (include.nonfocal=="bb") {
			lp <- as.vector(lp.focal[[i]] + lp.split[[i]])
		} else {
			lp <- as.vector(lp.focal[[i]] + lp.non.focal)
		}
		
		lwr <- lp - pse_var
		upr <- lp + pse_var
		out <- data.frame(..xxx = xvals[[i]]
			, fit = mean(plogis(lp))
			, lwr = mean(plogis(lwr))
			, upr = mean(plogis(upr))
			, model = modelname
		)
		return(out)
	})
	est <- do.call("rbind", est)
	colnames(est) <- c(focal, "fit", "lwr", "upr", "model")
	return(est)
}

stderrfun <- function(mod, mm, col_mean, isolate, ...) {
	vc <- vcov(mod)
	if (isolate) {
		mm_mean <- t(replicate(NROW(mm), col_mean))
		mm <- mm - mm_mean
	}
	pse_var <- sqrt(rowSums(mm * t(tcrossprod(data.matrix(vc), mm))))
	return(pse_var)
}

saveEnvironment()
