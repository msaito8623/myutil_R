#' Normalization of a vector into an arbitrary range
#'
#' This function normalizes a vector of numerics to an arbitrary range. The range is [0,1] as default.
#'
#' @param x A numeric vector, which are normalized.
#' @param norm.range A numeric vector, representing the range to which the vector (x) is normalized. Its length must be 2.
#' @return A new numeric vector, whose values are now normalized to the new range defined by norm.range.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' set.seed(0)
#' vec00 = rnorm(100)
#' vec01 = normalize(vec00)
#' vec02 = normalize(vec00, norm.range=c(-0.5,0.5))
#' par(mfrow=c(1,3))
#' rng00 = round(range(vec00), 2)
#' rng01 = round(range(vec01), 2)
#' rng02 = round(range(vec02), 2)
#' ttl00 = paste(as.character(rng00),collapse=', ')
#' ttl01 = paste(as.character(rng01),collapse=', ')
#' ttl02 = paste(as.character(rng02),collapse=', ')
#' hist(vec00, main=sprintf('vec00, range=[%s]', ttl00))
#' hist(vec01, main=sprintf('vec01, range=[%s]', ttl01))
#' hist(vec02, main=sprintf('vec02, range=[%s]', ttl02))
#' @export
#'
normalize = function ( x, norm.range=c(0,1)) {
	a = norm.range[1]
	b = norm.range[2]
	norm.values = a + ( ( (x-min(x,na.rm=TRUE)) * (b-a) ) / ( max(x,na.rm=TRUE) - min(x,na.rm=TRUE) ) )
	return(norm.values)
}
