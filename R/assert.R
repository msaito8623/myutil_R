#' The R equivalent of the assert function in python
#'
#' If the only argument "x" is TRUE, this function does nothing. Otherwise, it gives a pseudo AssersionError.
#'
#' @param bool A boolean. It must be TRUE or FALSE.
#' @return This function returns nothing.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' assert(TRUE)
#' assert(FALSE)
#' @export
#'
assert = function (bool) {
	if (!bool) {
		stop('Assertion Error')
	}
}
