#' Constructor to create a list of adoubles
#'
#' \code{adolc_createList} creates a list of adoubles of
#' length count. Each element is initialized to a provided value.
#'
#' @param count number of elements of the list
#' @param val   value to be initialize elements of the list
#'
#' @return None
#'
#' @examples
#' x <- adolc_createList(2,0.0)
#'
#'@seealso{\code{\link{trace_off}}, \code{\link{badouble_declareIndependent}}, 
#'         \code{\link{badouble_declareDependent}}}
#'         
#'@keywords{autodiffadolc}
#'
#' @export
adolc_createList <- function(count, val){
    if(!is.numeric(count)){
        stop("adolc_createList(): count must be numeric.")
    }
    if(!is.numeric(val)){
        stop("adolc_createList(): val must be numeric.")
    }
    if(count <= 0){
        stop("adolc_createList(): Unhandled case of count <=0")
    }
    ret <- c(adouble(val))
    for (i in 2:count){
        ret <- c(ret, adouble(0.1))
    }
    ret
}