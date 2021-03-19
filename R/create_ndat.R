#' Create a dataframe for prediction.
#'
#' To get predicted values from a regression model, a new dataframe is often needed, where all the interested combinations of predictors are supplied. This function automates this process of making a new dataframe for prediction.
#'
#' @param dat A dataframe, from which a new dataframe is generated. Usually, dat is the dataframe used to fit a regression model.
#' @param varycollist A list to indicate which columns (variables) to vary how. It should be given in the form of list(foo=c(10,20), bar=c('a','b','c')). For a continuous variable, a sublist can be used to tell how many data points to generate and also to determine the end values, e.g. list(foo=list(edge=0.1, len.out=100)) where the predictor "foo" will have 100 different values with its min and max being its quantiles of 0.1 and 0.9. The predictors not mentioned in this argument will have only one value.
#' @return A dataframe, where all the combinations of values specified by varycollist.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' set.seed(10)
#' x = rnorm(100)
#' f = rep(c(0,1),each=50)
#' y = x + f + x*f + rnorm(100, mean=0, sd=0.1)
#' original_dat = data.frame('y'=y, 'x'=x, 'f'=factor(f))
#' mdl = lm(y ~ x*f, data=original_dat)
#' vclist = list(x=list(edge=0.05, len.out=100), f=c('1'))
#' new_dat = create_ndat(original_dat, vclist)
#' new_dat$fit = predict(mdl, newdata=new_dat)
#' plot(new_dat$x, new_dat$fit)
#' @export
#'
create_ndat = function(dat, varycollist=list()){
	res = list()
	if (length(varycollist)>0) {
		aaa = dat[, colnames(dat)%in%names(varycollist),drop=FALSE]
		dat = dat[,!colnames(dat)%in%names(varycollist),drop=FALSE]
		for ( i in seq_along(varycollist) ) {
			ccol = names(varycollist)[i]
			if (is.list(varycollist[[ccol]])) {
				cvec = aaa[,ccol]
				if (any(class(cvec)%in%c('numeric'))) {
					cedge = varycollist[[ccol]]$edge
					edges = sort(c(cedge, abs(1 - cedge)))
					clen = varycollist[[ccol]]$len.out
					quan = quantile(cvec, probs=edges)
					res[[ccol]] = seq(min(quan), max(quan), length.out=clen)
				} else if (any(class(cvec)%in%c('factor'))) {
					res[[ccol]] = sort(unique(cvec))
				}
			} else {
				res[[ccol]] = varycollist[[ccol]]
			}
		}
	}
	for ( i in colnames(dat) ) {
		cvec = dat[,i]
		if (any(class(cvec)%in%c('numeric'))) {
			res[[i]] = median(cvec)
		} else if (any(class(cvec)%in%c('factor'))) {
			res[[i]] = names(sort(table(cvec),decreasing=TRUE))[1]
		}
	}
	return(expand.grid(res))
}
