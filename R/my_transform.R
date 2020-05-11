#' Simple variable transformation
#'
#' Transform a variable by the BoxCox transformation or taking its log values.
#'
#' @importFrom car powerTransform bcPower
#' @param vec Variable as a vector, which to be transformed.
#' @param type Which transformation method to use. Log- and BoxCox-transformations are implemented so far.
#' @return Vector of transformed values.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' v = rchisq(n=1000, df=3)
#' v = list(v)
#' v[[2]] = my_transform(vec=v[[1]], type='log')
#' v[[3]] = my_transform(vec=v[[1]], type='boxcox')
#' names(v) = c('Original', 'Log', 'BoxCox')
#' par(mfrow=c(1,3))
#' for (i in seq_along(v)) {
#'     hist(v[[i]], main=names(v)[i], xlab='Values')
#' }
#' dev.off()
#' @export
#'
my_transform <-
function ( vec, type='log' )
{
	if (type=='log') {
		vec = log(vec)
	} else if (type=='boxcox'){
		p1 = powerTransform(vec)
		vec = bcPower(vec, p1$lambda)
	} else {
		stop('type must be log or boxcox.')
	}
	return(vec)
}

