#' Generating a file name
#'
#' Generates a file name with a prefix, a middle part, and a suffix.
#'
#' @param prefix A character string, which is attached to the beginning of the generated file name with '_' after it.
#' @param middle A character string or vector, which is inserted between prefix and suffix with '_' between each element if middle is a vector of strings.
#' @param suffix A character string, which is attached to the end of the generated file name with '.' before it.
#' @param sep A character string, which is used to concatenate all the elements from prefix, middle, and suffix. It is '_' as default.
#' @return A concatenated string from the arguments.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' pfx = 'plot'
#' mid = c('foo', 'bar', 'qux')
#' sfx = 'png'
#' fn01 = file_name(prefix=pfx, middle=mid, suffix=sfx)
#' fn02 = file_name(prefix=pfx, middle=mid, suffix=sfx, sep='-')
#' print(fn01)
#' print(fn02)
#' @export
#'
gen_filename = function ( prefix='', middle=c(), suffix='', sep='_' ) {
	if (prefix!='' && !is.null(middle)) {
		prefix = paste(prefix, sep, sep='')
	}
	mid = paste(unname(middle), collapse=sep)
	suf = ifelse(suffix=='' || substr(suffix,1,1)=='.', suffix, paste('.',suffix,sep=''))
	fname = sprintf('%s%s%s', prefix, mid, suf)
	return(fname)
}
