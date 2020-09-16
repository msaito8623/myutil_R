#' Curvature index for a given point
#'
#' Calculates how pointy a given function is at a given point.
#'
#' @importFrom Deriv Deriv
#' @param func A callable function, on which you want to get a curvature index. It has "x" as an argument.
#' @param x A numeric, whose length is 1. You get a curvature index on this point.
#' @return A single value that represents how sharply a given function bends at a given point (i.e. a curvature index).
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' func01 = function ( x ) 2*x^2
#' func02 = function ( x ) 0.5*x^2
#' print(curvature(func01, 0))
#' print(curvature(func02, 0))
#' @export
#'
curvature <- function ( func, x ) {
	deriv1 = Deriv(func, 'x')
	deriv2 = Deriv(deriv1, 'x')
	k_value = deriv2(x)/(1+(deriv1(x))^2)^(3/2)
	return(k_value)
}
