#' Selective conversion of factor to character columns
#'
#' Converts only factor columns into character columns.
#'
#' @param dat A dataframe, some of whose columns you would like to convert to character columns.
#' @return A new dataframe, where the columns which were originally factors are characters now.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' dat1 = data.frame(label=LETTERS[1:3], value=seq(10,30,10), other=letters[1:3])
#' dat2 = as_character_df(dat1)
#' str(dat1)
#' str(dat2)
#' @export
#'
as_character_df = function ( dat ) {
	pos = which(sapply(dat, is.factor))
	for ( i in pos ) {
		dat[,i] = as.character(dat[,i])
	}
	return(dat)
}
