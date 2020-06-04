#' Multiple value replacement in a vector
#'
#' Replaces multiple values in a vector according to pairs provided.
#'
#' @param vec A vector to which you want to execute replacements.
#' @param from A vector of strings which you want to replace for something else.
#' @param to A vector of strings with the same length as 'from'. Target strings specified by 'from' are replaced by the corresponding values specified by this variable ('to').
#' @return A new vector with new replacement values.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' vvv = LETTERS[1:5]
#' fff = c('B', 'D')
#' ttt = c('XX', 'YY')
#' vvv = replace_values(vec=vvv, from=fff, to=ttt)
#' print(vvv)
#' @export
#'
replace_values = function ( vec, from=c('aː', 'a', 'iː', 'ɪ', 'uː', 'ʊ'), to=c('a',  'a', 'i',  'i', 'u',  'u') ) {
	if (is.factor(vec)) {
		vec = as.character(vec)
	}
	for ( i in seq_along(from) ) {
		vec[vec==from[i]] = to[i]
	}
	return(vec)
}

