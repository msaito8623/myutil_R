#' Cutting off outliers
#'
#' Cut off outliers by some different criteria. With type='sd', the values that lie in the outside of 2*sd(v) are removed. With type='quantile', the values are removed if they lie out of the quantiles defined by p.
#'
#' @param vec Variable as a vector, whose outliers to be cut off.
#' @param type Criterion to be used to define outliers.
#' @param p Quantile point. The values dropping out of the first p and the last p quantiles are removed.
#' @param value Return the remaining values if value=TRUE, else corresponding booleans are returned.
#' @return Vector of booleans in the same length as the original vector if value=FALSE. If value=TRUE, outliers are removed and a new vector without the outliers is returned.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' v = rchisq(n=1000, df=3)
#' v = list(v)
#' v[[2]] = cutoff_outliers(vec=v[[1]], type='sd', value=TRUE)
#' v[[3]] = cutoff_outliers(vec=v[[1]], type='quantile', value=TRUE)
#' par(mfrow=c(1,3))
#' for (i in v) {
#'     boxplot(i)
#' }
#' dev.off()
#' @export
#'
cutoff_outliers <-
function ( vec, type='sd', p=0.1, value=FALSE )
{
	if (type=='sd') {
		res = abs(vec) <= sd(vec)*2
	} else if (type=='quantile'){
		rng = quantile(vec, p=c(p, 1-p))
		res = vec>rng[1] & vec<rng[2]
	} else {
		stop('type must be sd or quantile.')
	}
	if (value) {
		res = vec[res]
	}
	return(res)
}
