adolc_createList <- function(count, val){
    if(count <= 0){
        stop("adolc_createList(): Unhandled case of count <=0")
    }
    ret <- c(adouble(val))
    for (i in 2:count){
        ret <- c(ret, adouble(0.1))
    }
    ret
}