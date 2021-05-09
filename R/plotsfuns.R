#' Plot  predictions 
#' 
#' Plots estimated conditional or marginal predictions. 
#' @param x \code{\link[vareffects]{varpred}} object
#' @param ... for future implementations
#' @param xlabs x-axis label. If \code{NULL}, default, \code{x.var} is used.
#' @param ylabs y-axis label. If \code{NULL}, default, the response label is used.
#' @param pos spacing between labels of categorical variable on the plot. 
#'
#' @return a \code{\link[ggplot2]{ggplot}} object.
#'
#' @seealso
#'\code{\link[vareffects]{varpred}}
#'
#' @examples
#' set.seed(4567)
#' x <- rnorm(100, 3, 5)
#' y <- 0.4 + 0.7*x + rnorm(100)
#' df <- data.frame(y = y, x = x)
#' m1 <- lm(y ~ x, df)
#'
#' pred1 <- varpred(m1, "x")
#' plot(pred1)
#'
#' @import ggplot2
#' @export

plot.vareffects <- function(x, ..., xlabs = NULL, ylabs = NULL, pos = 0.5, facet_scales = "fixed", facet_ncol = NULL){

	lwr <- upr <- NULL
	df <- x$preds
	if (is.null(df)) df <- x
	focal <- attr(df, "focal")
	if (!is.null(focal))	n.focal <- length(focal) else {n.focal <- 1L; focal <- "xvar"}
	x.var <- attr(df, "x.var")
	if (is.null(x.var)) x.var <- focal[[1]]
	if (n.focal>1L) {
		non.focal <- focal[!focal %in% x.var]
		df[, non.focal] <- sapply(non.focal, function(x){
			xx <- df[[x]]
			xx <- if (class(xx) %in% c("numeric", "integer")) round(xx,1) else xx
			ll <- paste0(x, ": ", xx)
			return(ll)
		})	
		gform <- as.formula(paste0(".~", paste0(non.focal, collapse="+")))
	}
	if (is.null(xlabs)) xlabs <- x.var
	if (is.null(ylabs)) ylabs <- attr(df, "response")
	pos <- position_dodge(pos)
	nn <- unique(df$model)
	if (!(length(nn)>1)){
		p1 <- (ggplot(df, aes_string(x = x.var, y = "fit"), colour="black", alpha = 0.2)
			+ guides(fill = FALSE)
			+ theme(legend.position = "none")
		)
	} else {
		p1 <- (ggplot(df, aes_string(x = x.var, y = "fit", colour ="model"), alpha = 0.2)
			+ guides(fill = FALSE)
			+ theme(legend.position = "right")
		)
	}
	p1 <- p1 + labs(x = xlabs, y = ylabs)
	if (class(df[[x.var]]) %in% c("numeric", "integer")) {
		p2 <- (p1
			+ geom_line()
			+ geom_line(aes(y = lwr), lty = 2)
			+ geom_line(aes(y = upr), lty = 2)
			+ scale_colour_viridis_d(option = "plasma")
		)
	} else {
		if (length(nn) > 1) {
			p2 <- (p1 
#				+ geom_point(aes(colour=model), position = pos, size = 0.6)
				+ geom_pointrange(aes(ymin = lwr, ymax = upr, colour=model), size=0.2, position = pos)
			)
		} else {
			p2 <- (p1 
#				+ geom_point(position = pos, size = 0.6, colour="black")
				+ geom_pointrange(aes(ymin = lwr, ymax = upr), colour = "black", size=0.2, position = pos)
			)
		}
	}

	if (n.focal>1L) {
		p2 <- p2 + facet_wrap(gform, scales=facet_scales, ncol=facet_ncol)#, labeller = label_parsed)
	}
	return(p2)
}

#' Customized theme for vareffects plots
#'
#' Sets a theme for vareffects and other ggplot objects
#'
#' @examples
#' library(ggplot2)
#' varefftheme()
#' set.seed(4567)
#' x <- rnorm(100, 3, 5)
#' y <- 0.4 + 0.7*x + rnorm(100)
#' df <- data.frame(y = y, x = x)
#' m1 <- lm(y ~ x, df)
#'
#' pred1 <- varpred(m1, "x")
#' plot(pred1)
#'
#' @import ggplot2
#' @export

varefftheme <- function(){
   theme_set(theme_bw() +
      theme(panel.spacing = grid::unit(0,"lines")
      	, plot.title = element_text(hjust = 0.5)
			, legend.position = "bottom"
			, axis.ticks.y = element_blank()
			, axis.text.x = element_text(size = 12)
			, axis.text.y = element_text(size = 12)
			, axis.title.x = element_text(size = 12)
			, axis.title.y = element_text(size = 12)
			, legend.title = element_text(size = 13, hjust = 0.5)
			, legend.text = element_text(size = 13)
			, panel.grid.major = element_blank()
			, legend.key.size = unit(0.8, "cm")
			, legend.key = element_rect(fill = "white")
			, panel.spacing.y = unit(0.3, "lines")
			, panel.spacing.x = unit(1, "lines")
			, strip.background = element_blank()
			, panel.border = element_rect(colour = "grey"
				, fill = NA
				, size = 0.8
			)
			, strip.text.x = element_text(size = 11
				, colour = "black"
				, face = "bold"
			)
      )
   )
}

