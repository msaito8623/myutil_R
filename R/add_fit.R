#' Add predicted values to a dataframe
#'
#' This function takes a dataframe and a GAM model and adds predicted values from the GAM model to the dataframe. The terms to be used for prediction can be set by the arguments "pterm" and "sterm" for parametric and smooth terms each. The current version of this function assumes the input model is a GAM model (mgcv::gam/bam) and the model was fitted with the response variable being a normal distribution.
#'
#' @param dat A dataframe. It is usually a "new" dataframe for prediction, which usually has all the combination of predictors, created by expand.grid.
#' @param mdl A mgcv::gam/bam model. The response variable should be a normal distribution.
#' @param pterm A numeric vector to indicate which parametric terms to be used for prediction. This number can be known by base::summary (mgcv::summary.gam). If the parametric term shows up in the first position except the intercept term, this number is 1. If it is the second, the number is 2, and so on. However, ignore duplicated factor terms with different levels, to count this number. Please see the example below for details.
#' @param sterm A numeric vector to indicate which smooth terms to be used for prediction. This number is the same as where the smooth term shows up in mgcv::summary.gam.
#' @return A dataframe, which has predicted values.
#' @import mgcv
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' set.seed(10)
#' dat = gamSim(n=300)
#' set.seed(10)
#' dat$f = factor(sample(letters[1:3],300,replace=TRUE))
#' set.seed(10)
#' dat$g = factor(sample(LETTERS[1:3],300,replace=TRUE))
#' mdl = gam(y ~ f + g + s(x0, by=f) + te(x1, x2, by=g), data=dat)
#' vclist = list(f=list(), x0=list(edge=0, len.out=10),
#' 	      x1=list(edge=0, len.out=10),
#' 	      x2=list(edge=0, len.out=10))
#' ndat = create_ndat(mdl$model, vclist)
#' ndat = add_fit(ndat, mdl, pterm=2, sterm=5)
#' @export
#'
add_fit = function (dat, mdl, pterm=NULL, sterm=NULL) {
	if ( any(!sapply(list(pterm,sterm),is.null)) ) {
		pterm = attr(mdl$pterms, 'term.labels')[pterm]
		sterm = sapply(mdl$smooth[sterm], function(x){x$label})
		print(sprintf('Selected pterm: %s', pterm))
		print(sprintf('Selected sterm: %s', sterm))
		prd = predict.gam(mdl, newdata=dat, type='terms', terms=c(pterm,sterm), se=1)
		dat$fit = unname(apply(prd$fit,1,sum))
		dat$se  = unname(apply(prd$se.fit,1,sum))
	} else {
		dat$fit = predict.gam(mdl, newdata=dat, se.fit=1)$fit
		dat$se  = predict.gam(mdl, newdata=dat, se.fit=1)$se.fit
	}
	dat$upr = dat$fit + 1.98*dat$se
	dat$lwr = dat$fit - 1.98*dat$se
	return(dat)
}
