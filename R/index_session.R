#' Indexing each session in time-series data
#'
#' Time-series data usually have a column telling the beginning of each time-series event, e.g. AR.start. Based on this column, the present function gives indices for each time-series event. So, simply, c(TRUE, FALSE, FALSE, TRUE, FALSE) is converted to c(1,1,1,2,2).
#'
#' @param bool.vec A boolean vector, which represents the beginnings of each time-series session, e.g. AR.start.
#' @return A numeric vector in the same length as bool.vec, which stands for indices of time-series sessions.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' vec = c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
#' ind = index_session(vec)
#' print(data.frame('AR.start'=vec, 'AR.index'=ind))
#' @export
#'
index_session <-
function ( bool.vec )
{
	.temp = function (cpos, total.len) {
		vec = rep(0, total.len)
		vec[cpos:length(vec)] = 1
		return(vec)
	}
	vec = bool.vec
	res = Reduce('+', lapply(which(vec), .temp, total.len=length(vec)))
	return(res)
}
