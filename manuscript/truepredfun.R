library(shellpipes)
library(pacman)

loadEnvironments()

## Load all (including internal) functions of vareffects

hidden <- setdiff(p_funs("vareffects", TRUE), p_funs("vareffects"))
invisible(lapply(hidden, function(x) {

	 a <- strtrim(x, 1) == "%" 
	 b <- substring(x, nchar(x)) == "%"

	 if (a && b) {
		  x2 <- paste0("`", x, "`")
	 } else {
		  x2 <- x
	 }

	 assign(x, eval(parse(text=paste0("vareffects", ":::", x2))), 
		  envir = .GlobalEnv)
}))

truepredfun <- function(mod, focal_predictors, x.var=NULL, true_betas, include.re=TRUE, df=NULL, modelname="truth") {

	mod <- prepmod(mod)
	vareff_objects <- vareffobj(mod)
	betahat <- true_betas
	mod_names <-get_vnames(mod)
	vnames <- mod_names$vnames
	vnames_all <- mod_names$vnames_all
	termnames <- mod_names$termnames
	Terms <- mod_names$Terms
	focal.predictors <- NULL
	rTerms <- delete.response(Terms)
	for (focal in focal_predictors){
		if (any(vnames_all %in% focal)) {
			check_vars <- vnames_all %in% focal
		} else {
			check_vars <- termnames %in% focal
		}
		if (!any(check_vars)) stop(paste0(focal, " not in the model"))
		focal.predictors[[focal]] <- unique(vnames_all[check_vars])
	}

	if (!is.null(x.var) & !any(focal.predictors %in% x.var) & length(focal.predictors)>1L)
		stop(paste0(x.var, " not in ", focal.predictors))

	n.focal <- length(focal.predictors)
	if (is.null(x.var) & n.focal>1L) {
		x.var <- focal.predictors[[2]]
		message(paste0("x.var was not specified, ", x.var, " is used instead."))
	} else if (is.null(x.var)) {
		x.var <- focal.predictors[[1]]
	}

	contr <- vareff_objects$contrasts

	model_frame_objs <- clean_model(focal.predictors=focal.predictors
		, mod = mod
		, xlevels=list()
		, default.levels=NULL
		, formula.rhs=rTerms
		, steps=100
		, x.var=x.var
		, typical=avefun
		, vnames=vnames
		, bias.adjust = "population"
	)	

	formula.rhs <- formula(vareff_objects)[c(1,3)]
	excluded.predictors <- model_frame_objs$excluded.predictors
	predict.data <- model_frame_objs$predict.data
	x.var <- model_frame_objs$x.var
   factor.levels <- model_frame_objs$factor.levels
	.linkinv <- vareff_objects$link$linkinv

	
	x.focal <- predict.data$focal
	x.excluded <- predict.data$excluded
	focal_mf <- model.frame(mod)
	pred_list <- list()
	offs <- NULL

	if (!is.null(x.excluded)) {
		nM <- NROW(x.excluded)
	} else {
		nM <- NROW(focal_mf)
	}
	if (include.re) {
		re <- includeRE(mod)	
	} else {
		re <- 0
	}
	
	# TODO: eliminate loop if there is no x.excluded 
	for (i in 1:NROW(x.focal)) {
		focal_i <- x.focal[i, ,drop=FALSE]
		focal_i[1:nM, ] <- focal_i
		mf_i <- if(!is.null(x.excluded)) cbind.data.frame(focal_i, x.excluded) else focal_i
		mf_i <- model.frame(rTerms, mf_i, xlev=factor.levels, na.action=NULL)
		mm_i <- model.matrix(formula.rhs, data = mf_i, contrasts.arg = contr)

		off <- get_offset(offset, mf_i)
		offs[i] <- off
		pred_list[[i]] <- cbind.data.frame(focal_i[1, ,drop=FALSE]
			, fit=mean(.linkinv(off + as.vector(mm_i %*% betahat) + re))
			, model=modelname
		)
	}
	pred_df <- do.call("rbind", pred_list)
	return(pred_df)
}


saveEnvironment()
