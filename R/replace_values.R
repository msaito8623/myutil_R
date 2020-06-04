#' Value replacement in a dataframe
#'
#' Replaces multiple values in a dataframe according to pairs provided.
#'
#' @param dat A dataframe to which you want to execute replacements.
#' @param from A vector of strings which you want to replace for something else.
#' @param to A vector of strings with the same length as 'from'. Target strings specified by 'from' are replaced by the corresponding values specified by this variable ('to').
#' @param clm A string, which gives the name of the column whose values you want to replace.
#' @return A new dataframe, whose column with the name specified by 'clm' now contains new replaced values.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' ddd = data.frame('letter'=LETTERS[1:5], 'value'=seq(10,50,10))
#' fff = c('B', 'D')
#' ttt = c('XX', 'YY')
#' dat = replace_values(dat=ddd, from=fff, to=ttt, clm='letter')
#' print(dat)
#' @export
#'
replace_values = function ( dat, from=c('aː', 'a', 'iː', 'ɪ', 'uː', 'ʊ'), to=c('a',  'a', 'i',  'i', 'u',  'u'), clm='Segment') {
	if (is.factor(dat[[clm]])) {
		dat[[clm]] = as.character(dat[[clm]])
	}
	for ( i in seq_along(from) ) {
		dat[[clm]][dat[[clm]]==from[i]] = to[i]
	}
	return(dat)
}

