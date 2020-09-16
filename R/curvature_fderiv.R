#' Curvature index for a given point by finite differences
#'
#' Calculates how pointy the middle of the three point pairs (i.e. x and y).
#'
#' @param xxx A numeric vector, whose length is exactly 3 from lower to higher values.
#' @param yyy A numeric vector, which corresponds to xxx.
#' @return A single value that represents how much sharp point the three x-y pairs make in their middle (i.e. an approximated curvature index at the middle point).
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' func01 = function (x) { 2 * x^3 }
#' func02 = function (x) { 0.5 * x^3 }
#' eps = 1e-5
#' cntr = 0.5
#' xxx = c(cntr-eps, cntr, cntr+eps)
#' approx_k01 = curvature_fderiv(xxx, func01(xxx))
#' true_k01 = curvature(func01, cntr)
#' approx_k02 = curvature_fderiv(xxx, func02(xxx))
#' true_k02 = curvature(func02, cntr)
#' txt = list()
#' txt[[1]] = sprintf('Correct K for 2*x^3 at 0.5:\t\t%s\n', true_k01)
#' txt[[2]] = sprintf('Approximated K for 2*x^3 at 0.5:\t%s\n', approx_k01)
#' txt[[3]] = sprintf('Correct K for 0.5*x^3 at 0.5:\t\t%s\n', true_k02)
#' txt[[4]] = sprintf('Approximated K for 0.5*x^3 at 0.5:\t%s\n', approx_k02)
#' cat(do.call(paste, c(txt, list(sep=''))))
#' @export
#'
curvature_fderiv <- function ( xxx, yyy ) {
	deriv1 = fderiv(xxx, yyy, 1)
	deriv2 = fderiv(xxx, yyy, 2)
	k_value = deriv2/(1+(deriv1)^2)^(3/2)
	return(k_value)
}
