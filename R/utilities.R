# Internal functions used to condition the model. Some of the condition code is based on R package effects (clean_model). Transfered and modified here because they are not exported in effects.

get_vnames <- function(mod){
	mat <- model.matrix(mod)
	coefnames <- colnames(mat)
	Terms <- terms(mod)
	vnames <- all.vars(delete.response(Terms)) 
	termnames <- attr(Terms, "term.labels") # Model based names
	termnames <- gsub(" ", "", termnames)
	assign <- attr(mat, "assign")
	if(any(assign==0)) {
		vnames <- c("(Intercept)", vnames)
		termnames <- c("(Intercept)", termnames)
		assign <- assign + 1
	}
	vnames <- setNames(vnames[assign], coefnames)
	termnames <- setNames(termnames[assign], coefnames)
	# In case there are interactions
	vnames[is.na(vnames)] <- termnames[is.na(vnames)]
	return(list(vnames = vnames, termnames = termnames, Terms = Terms))
}

get_xlevels <- function(mod){
	if (inherits(mod, "glmmTMB")) {
		xlevels <- .getXlevels(terms(mod), model.frame(mod))
	} else {
		xlevels <- mod$xlevels
	}
	return(xlevels)
}


check_numeric <- function(xvar, mod) {
	xlevels <- get_xlevels(mod)
	is.null(xlevels[[xvar]])
}

get_vcov <- function(mod){
	if (inherits(mod, "glmmTMB")) {
		vc <- vcov(mod)$cond
	} else if (inherits(mod, "clmm")){
		f <- c(names(mod$alpha)[[1]], names(mod$beta))
		vc <- vc[f, f]
	} else {
		vc <- vcov(mod)
	}
	return(vc)
}

get_degof <- function(mod, dfspec) {
	df <- ifelse(
		grepl("df.residual", paste(names(mod), collapse=""))
		, mod$df.residual, dfspec
	)
	return(df)
}

get_stats <- function(level, df, modfamily=NULL) {
	if (is.null(modfamily)){
		mult <- qt(1 - (1 - level)/2, df)
	}
	return(mult)
}

subscripts <- function(index, dims){
  subs <- function(dims, index){
    dim <- length(dims)
    if (dim == 0) return(NULL)
    cum <- c(1,cumprod(dims))[dim]
    i <- index %/% cum
    if (index %% cum != 0) i <- i + 1
    c(i, subs(dims[-dim], index - (i - 1)*cum))
  }
  rev(subs(dims, index))
}

check_intercept <- function(mod, ...) {
	if (inherits(mod, "glmmTMB"))  {
		any(names(fixef(mod)$cond)=="Intercept")	
	} else {
		any(names(coefficients(mod))=="(Intercept)")
	}
}

get_response <- function (mod, ...) deparse(attr(terms(mod), "variables")[[2]])

get_termnames <- function (mod, ...) {
  termnames <- gsub(" ", "", labels(terms(mod)))
  if (check_intercept(mod)) c("(Intercept)", termnames)
  else termnames
}

match_vnames <- function(name, expressions){
  scratch <- "zAMIjw4RN3" # randomly generated string
  name <- gsub("\\.", scratch, name)
  expressions <- gsub("\\.", scratch, as.character(expressions))
  a <- !grepl(paste("[.]+", name, sep=""), expressions)
  b <- !grepl(paste(name, "[.]+", sep=""), expressions)
  c <- grepl(paste("\\b", name, "\\b", sep=""), expressions)
  a & b & c
}

get_strangers <- function(mod, focal.predictors, excluded.predictors){
  names <- get_termnames(mod)
  if (check_intercept(mod)) names <- names[-1]
  sel <- apply(sapply(excluded.predictors, match_vnames, expressions=names), 1, any)
  (1:length(sel))[sel]
}

matrix.to.df <- function(matrix, colclasses){
  opt <- options(warn = -1)
  on.exit(options(opt))
  ncol <- ncol(matrix)
  colnames <- colnames(matrix)
  colclasses[sapply(colclasses, function(x) "integer" %in% x)] <- "numeric"
  result <- vector(mode="list", length=ncol)
  names(result) <- colnames
  for (j in 1:ncol){
    result[[j]] <- matrix[, j]
    class <- colclasses[[colnames[j]]]
    result[[colnames[j]]] <- if ("numeric" %in% class) {
      decChar <- getOption('OutDec')
      if (decChar == '.') as.numeric(result[[colnames[j]]])
      else as.numeric(gsub(decChar, '.', matrix[,j]))
    }
    else if ("ordered" %in% class) ordered(result[[colnames[j]]])
    else if ("factor" %in% class) factor(result[[colnames[j]]]) 
    else result[[colnames[j]]]
  }
  as.data.frame(result)
}

clean_model <- function(focal.predictors, mod, xlevels = list()
	, default.levels=NULL, formula.rhs, steps = 101, x.var=NULL, typical=mean){
  if ((!is.null(mod$nan.action)) && inherits(mod$na.action, "exclude"))
    class(mod$na.action) <- "omit"
  all.predictors <- all.vars(formula.rhs)
  check.vars <- !(focal.predictors %in% all.predictors)
  excluded.predictors <- setdiff(all.predictors, focal.predictors)
  number.bad <- sum(check.vars)
  if (any(check.vars)) {
    message <- if (number.bad == 1) paste("the following predictor is not in the model:", 
                                          focal.predictors[check.vars])
    else paste("the following predictors are not in the model:", 
               paste(focal.predictors[check.vars], collapse=", "))
    stop(message)
  }
  X.mod <- model.matrix(mod)
  cnames <- colnames(X.mod)
  factor.cols <- rep(FALSE, length(cnames))
  names(factor.cols) <- cnames
  for (name in all.predictors){
    if (!check_numeric(name, mod)) {
      factor.cols[grep(paste("^", name, sep=""), cnames)] <- TRUE
    }
  }
  factor.cols[grep(":", cnames)] <- FALSE   
  X <- recoverdata(mod, all.predictors)
  
  which.matrices <- sapply(X, function(x) is.matrix(x) && ncol(x) == 1)
  if (any(which.matrices)){
    nms <- names(which.matrices[which.matrices])
    msg <- if (length(nms) > 1){
      paste("the predictors", paste(nms, collapse=", "), "are one-column matrices that were converted to vectors")
    } else {
      paste("the predictor", nms, "is a one-column matrix that was converted to a vector")
    }
    warning(msg)
    for (nm in nms){
      X[, nm] <- as.vector(X[, nm])
    }
  }
  
  for (name in all.predictors){
    if (!check_numeric(name, mod) && is.null(xlevels[[name]])) {
      xlevels[[name]] <- levels(X[, name]) 
    }
  }
  bad <- sapply(X[, all.predictors, drop=FALSE], function(x) !(is.factor(x) || is.numeric(x)))
  if (any(bad)){
    message <- if (sum(bad) == 1) paste("the following predictor isn't a factor, logical, character, or numeric:", 
                                        all.predictors[bad])
    else paste("the following predictors aren't factors, logical, character, or numeric:", 
               paste(all.predictors[bad], collapse=", "))
    stop(message)
  }
  x <- list()
  factor.levels <- list()
  if(length(xlevels)==0 & length(default.levels) == 1L) xlevels <- default.levels
  if(is.numeric(xlevels) & length(xlevels) == 1L){
    levs <- xlevels
    for(name in focal.predictors) xlevels[[name]] <- levs
  }
  for (name in focal.predictors){
    levels <- mod$xlevels[[name]]
    if(is.null(levels)) levels <- mod$xlevels[[paste("factor(",name,")",sep="")]]
    fac <- !is.null(levels)
	 if (!fac) {
		levels <- if (is.null(xlevels[[name]])){
			 seq(min(X[, name]), max(X[, name]), length.out=steps)
		}
		else {
		  if(length(xlevels[[name]]) == 1L) { 
			 seq(min(X[, name]), max(X[,name]), length=xlevels[[name]])
			} else xlevels[[name]]
		}
	 }
	 else factor.levels[[name]] <- levels
	 x[[name]] <- list(name=name, is.factor=is.factor(X[, name]), levels=levels)
  }
  x.excluded <- list()
  for (name in excluded.predictors){
    levels <- mod$xlevels[[name]] 
    if (is.logical(X[, name])) levels <- c("FALSE", "TRUE")
    fac <- !is.null(levels)
    level <- if (fac) levels[1] else typical(X[, name])
    if (fac) factor.levels[[name]] <- levels
    x.excluded[[name]] <- list(name=name, is.factor=fac,
                               level=level)
  }
  dims <- sapply(x, function(x) length(x$levels))
  len <- prod(dims)
  n.focal <- length(focal.predictors)
  n.excluded <- length(excluded.predictors)
  n.vars <- n.focal + n.excluded
  predict.data <-matrix('', len, n.vars)
  excluded <- sapply(x.excluded, function(x) x$level)
  for (i in 1:len){
    subs <- subscripts(i, dims)
    for (j in 1:n.focal){
      predict.data[i,j] <- x[[j]]$levels[subs[j]]
    }
    if (n.excluded > 0)
      predict.data[i, (n.focal + 1):n.vars] <- excluded
  }
  colnames(predict.data) <- c(sapply(x, function(x) x$name),
                              sapply(x.excluded, function(x) x$name))
  colclasses <- lapply(X, class)
  colclasses[colclasses == "matrix"] <- "numeric"
  colclasses[colclasses == "array"] <- "numeric"
  predict.data <-  matrix.to.df(predict.data, colclasses=colclasses)
  list(predict.data=predict.data, 
       factor.levels=factor.levels, 
       factor.cols=factor.cols, focal.predictors=focal.predictors, n.focal=n.focal,
       excluded.predictors=excluded.predictors, n.excluded=n.excluded,
       x=x, X.mod=X.mod, cnames=cnames, X=X, x.var=x.var)  
}


get_model_matrix <- function(mod, mod.matrix, mod.matrix.all, X.mod,
                               factor.cols, cnames, focal.predictors, excluded.predictors,
                               typical, apply.typical.to.factors=FALSE){
  attr(mod.matrix, "assign") <- attr(mod.matrix.all, "assign")
  if (length(excluded.predictors) > 0){
    strangers <- get_strangers(mod, focal.predictors, excluded.predictors)
    stranger.cols <-
      apply(outer(strangers, attr(mod.matrix,'assign'), '=='), 2, any)
  }
  else stranger.cols <- rep(FALSE, ncol(mod.matrix))
  if (check_intercept(mod)) stranger.cols[1] <- TRUE
  if (any(stranger.cols)) {
    facs <- factor.cols & stranger.cols
    covs <- (!factor.cols) & stranger.cols
    if (check_intercept(mod)) covs[1] <- FALSE
    if (any(facs)){
      mod.matrix[,facs] <-  matrix(apply(as.matrix(X.mod[,facs]), 2,
                                         if (apply.typical.to.factors) typical else mean),
                                   nrow=nrow(mod.matrix), ncol=sum(facs), byrow=TRUE)
    }
    for (name in cnames){
      components <- unlist(strsplit(name, ':'))
      components <- components[components %in% cnames]
      if (length(components) > 1) {
        mod.matrix[,name] <- apply(mod.matrix[,components], 1, prod)
      }
    }
  }
  mod.matrix
}
