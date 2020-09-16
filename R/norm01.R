#' Normalization of a vector between 0 and 1
#'
#' This function normalizes a vector, so the whole range of the vector fits between 0 and 1.
#'
#' @param x A numeric vector, which you want to make into the range from 0 to 1.
#' @return A new numeric vector, whose values are now normalized between 0 and 1.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' set.seed(0)
#' vec = rnorm(10)
#' vec = norm01(vec)
#' print(vec)
#' @export
#'
norm01 = function ( x ) {
	(x-min(x))/(max(x)-min(x))
}
