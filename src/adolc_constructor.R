# Name: adolc_createList creates a list of adoubles of
# length count. Each element is initialized to a provided value
# Arguments:
#    count : positive numeric value
#    val   : value to be initialize elements of the list
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