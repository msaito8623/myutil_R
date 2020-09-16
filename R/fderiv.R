#' Centered difference approxmation
#'
#' Calculates an approximated derivative value from pairs of x and y, based on finite differences.
#'
#' @param xxx A vector of numerics, whose length is exactly 3.
#' @param yyy A vector of numerics, whose length is exactly 3 and each of which corresponds to each of values given by xxx.
#' @param odr Order of derivative. Currently only 1 or 2 is acceptable.
#' @return A single value that is an approximated derivative value at the given point (middle pair of xxx and yyy).
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' library(Deriv)
#' func = function (x) { 2 * x^3 }
#' xxx = c(2,3,4)
#' yyy = func(xxx) + rnorm(3)
#' approx_yyy1 = fderiv(xxx, yyy, 1)
#' approx_yyy2 = fderiv(xxx, yyy, 2)
#' true_yyy1 = Deriv(func, 'x', nderiv=1)(xxx[2])
#' true_yyy2 = Deriv(func, 'x', nderiv=2)(xxx[2])
#' txt = list()
#' txt[[1]] = '###\n'
#' txt[[2]] = sprintf('xxx: %s\n', paste(xxx, collapse=', '))
#' txt[[3]] = sprintf('yyy: %s\n', paste(yyy, collapse=', '))
#' txt[[4]] = sprintf('Approximated first derivative at x=%d: %s\n', xxx[2], approx_yyy1)
#' txt[[5]] = sprintf('Correct first derivative at x=%d: %s\n', xxx[2], true_yyy1)
#' txt[[6]] = sprintf('Approximated second derivative at x=%d: %s\n', xxx[2], approx_yyy2)
#' txt[[7]] = sprintf('Correct second derivative at x=%d: %s\n', xxx[2], true_yyy2)
#' txt[[8]] = '###\n'
#' cat(do.call(paste, c(txt, list(sep=''))))
#' func01 = function ( x ) 2*x^2
#' func02 = function ( x ) 0.5*x^2
#' print(curvature(func01, 0))
#' print(curvature(func02, 0))
#' @export
#'
fderiv = function ( xxx, yyy, odr = 1 ) {
	deriv1 = ( yyy[3] - yyy[1] ) / ( xxx[3] - xxx[1] )
	deriv2 = ( yyy[3] - 2*yyy[2] + yyy[1] ) / ( (xxx[3]-xxx[1])/2 )^2
	res = ifelse(odr==1, deriv1, deriv2)
	return(res)
}
