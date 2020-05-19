#' Vector to a nested list with a name
#'
#' Convert each element of a vector to a nested list with a name.
#'
#' @param vect A vector, which you want to convert to a nested list.
#' @param subname The name of each element of the vector in each nested list.
#' @return A nested list with its element being each element of the vector with its name as specified.
#' @author Motoki Saito, \email{motoki.saito@uni-tuebingen.de}
#' @keywords utilities
#' @examples
#' abc = LETTERS[1:3]
#' lst = sublist(vect=abc, subname='alphabet')
#' print(lst)
#' @export
#'
sublist <-
function ( vect, subname=NULL )
{

	tolist_elm = function ( elm, subname=NULL ) {
		lst = list(elm)
		if (!is.null(subname)) {
			names(lst) = subname
		}
		return(lst)
	}

	lst = as.list(vect)
	lst = lapply(lst, tolist_elm, subname)
	return(lst)

}
