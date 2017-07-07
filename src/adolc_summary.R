# Start of sum

`sumvec` = function(s_arg1)
{
    initflag = 1
    for(index in seq(1,length(s_arg1),1)){
      if(length(s_arg1[index][1])!=1)
          stop("length(s_arg1[index][1])!=1")
      if(initflag == 1){
          sumvecans = s_arg1[index][1]
          initflag = 0
      } else {
          sumvecans = sumvecans + s_arg1[index][1]
      }
    }
    return(sumvecans)
}


`oldsum` <- `sum`

`sum` = function(..., na.rm = FALSE)
{
    dots <- list(...);
    ndots <- length(dots)
    argtypes <- get_argtype(...);
     if (extends(argtypes[1], '_p_badouble')){
        initflag <- 0
        for(index in seq(1,ndots,1)) {
            if(is.list(dots[[index]])) {
                if(initflag == 0){
                  ans =  sumvec(dots[[index]])
                  initflag = 1
                } else {
                   ans = ans + sumvec(dots[[index]])
                }
            } else if(is.vector(dots[[index]])) {
                if(initflag == 0){
                    ans =  sumvec(dots[[index]])
                    initflag = 1
                } else {
                    ans = ans + sumvec(dots[[index]])
                }
            } else if(is.array(dots[[index]]) || is.matrix(dots[[index]])) {
                stop("sum(): Unhandled case of matrix/ array argument")
            } else {
                if(initflag == 0){
                  ans = dots[index]
                  initflag = 1
                } else {
                  ans = ans + dots[index]
                }
            }
        }
     } else {
         ans <- oldsum(..., na.rm)
     }
    ans
}