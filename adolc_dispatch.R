
`get_argtype` <- function(...) {
        argtypes <- mapply(class, list(...));
        argv <- list(...);
        argc <- length(argtypes);
        if(is.list(argv[1])){
            loclist<-argv[[1]]
            if(is.list(loclist)){
                argtypes[1] <- get_argtype(loclist[[1]])
            }
        } else if(is.matrix(argv[1])){
            locmatrix<-argv[[1]]
            if(is.matrix(locmatrix)){
              argtypes[1] <- get_argtype(locmatrix[1,1])
            }
        }
        if(argc>1 && is.list(argv[2])){
            loclist<-argv[[2]]
            if(is.list(loclist)){
                argtypes[2] <- get_argtype(loclist[[1]])
            }
        } else if(argc>1 && is.matrix(argv[2])){
            locmatrix<-argv[[2]]
            if(is.matrix(locmatrix)){
                argtypes[2] <- get_argtype(locmatrix[1,1])
            }
        }
        argtypes
}

`adolc_dispatch` = function(s_arg1, f_name_passive, f_name) {
    argtype <- get_argtype(s_arg1);
    if (!extends(argtype, '_p_badouble')) {
        ans <- f_name_passive(s_arg1)
    } else if(is.matrix(s_arg1)) {
        ans <- adolc_mat_dispatch_unary(s_arg1, f_name)
    } else {
        if (inherits(s_arg1, "ExternalReference")) s_arg1 = slot(s_arg1,"ref")
        ;ans = .Call(f_name, s_arg1, PACKAGE='adolc');
        ans <- new("_p_adub", ref=ans) ;
    }
    ans
}

`adolc_operator_dispatch` = function(..., f) {
    argtypes <- get_argtype(...);
    argv <- list(...);
    argc <- length(argtypes);
    if (argc == 1) {
        if(is.list(argv[[1]]) && extends(argtypes[1], '_p_badouble')) {
            return(f( (argv[[1]])[[1]]))
        } else {
            return(f(...));
        }
    } else if (argc == 2) {
        if(is.matrix(argv[[1]]) || is.matrix(argv[[2]])) {
            return(adolc_mat_dispatch(argv[[1]], argv[[2]], f))
        }
        if(length(argv[[1]])!=1 && length(argv[[2]])!=1 && length(argv[[1]])!=length(argv[[2]])){
            stop("adolc_operator_dispatch: Unhandled case: (length(argv[[1]])!=1 && length(argv[[2]])!=1 && length(argv[[1]])!=length(argv[[2]])) ")
        }
        if((is.list(argv[[1]]) || is.vector(argv[[1]])) && extends(argtypes[1], '_p_badouble') && (is.list(argv[[2]]) || is.vector(argv[[2]])) && extends(argtypes[2], '_p_badouble')) {
            return(adolc_vec_dispatch(argv[[1]], argv[[2]], f))
        } else if ((is.list(argv[[1]]) || is.vector(argv[[1]])) && extends(argtypes[1], '_p_badouble')) {
            if(is.vector(argv[[2]])) {
                if(length(argv[[1]])!=1 && length(argv[[2]])!=1 && length(argv[[1]])!=length(argv[[2]])){
                    stop("adolc_operator_dispatch: Unhandled case: (length(argv[[1]])!=1 && length(argv[[2]])!=1 && length(argv[[1]])!=length(argv[[2]])) ")
                }
                return(adolc_vec_dispatch(argv[[1]], argv[[2]], f))
            } else {
                for(index in seq(1,length(argv[[1]]),1)){
                    if(index == 1)
                      ans =(f(argv[[1]][[index]], argv[[2]]))
                    else
                      ans = c(f(argv[[1]][[index]], argv[[2]]), ans)
                }
                return(ans)
            }
        } else if ((is.list(argv[[2]]) || is.vector(argv[[2]])) && extends(argtypes[2], '_p_badouble')) {
            if(is.vector(argv[[1]])){
                if(length(argv[[1]])!=1 && length(argv[[2]])!=1 && length(argv[[1]])!=length(argv[[2]])){
                    stop("adolc_operator_dispatch: Unhandled case: (length(argv[[1]])!=1 && length(argv[[2]])!=1 && length(argv[[1]])!=length(argv[[2]])) ")
                }
                return(adolc_vec_dispatch(argv[[1]], argv[[2]], f))
            } else {
                for(index in seq(1,length(argv[[2]]),1)){
                    if(index == 1)
                      ans =(f(argv[[1]], argv[[2]][[index]]))
                    else
                      ans = c(f(argv[[1]], argv[[2]][[index]]), ans)
                }
                return(ans)
            }
        } else {
            return(f(...));
        }
    }
    stop("adolc_operator_dispatch: Unhandled case argc!=1 && argc!=2 ")
}

`adolc_vec_dispatch` = function(s_arg1, s_arg2, f) {
    
    if(length(s_arg1)!=1 && length(s_arg2)!=1 && length(s_arg1)!=length(s_arg2)){
        stop("adolc_vec_dispatch: Unhandled case: (length(s_arg1)!=1 && length(s_arg2)!=1 && length(s_arg1)!=length(s_arg2)) ")
    }
    argtypes1 = get_argtype(s_arg1)
    argtypes2 = get_argtype(s_arg2)
    if(length(s_arg1)!=1 && length(s_arg1)==length(s_arg2)){
        for(index in seq(1,length(s_arg1),1)){
            if (extends(argtypes1, '_p_badouble'))
                l_sarg1 <- s_arg1[[index]]
            else
                l_sarg1 <- s_arg1[index]
            if (extends(argtypes2, '_p_badouble'))
                l_sarg2 <- s_arg2[[index]]
            else
                l_sarg2 <- s_arg2[index]
            if(index == 1)
              ans =(f(l_sarg1,l_sarg2))
            else
              ans = c(f(l_sarg1,l_sarg2), ans)
            
        }
        return(ans)
    } else if(length(s_arg1)!=1){
        if (extends(argtypes2, '_p_badouble'))
          l_sarg2 <- s_arg2[[1]]
        else
          l_sarg2 <- s_arg2[1]
        for(index in seq(1,length(s_arg1),1)){
            if (extends(argtypes1, '_p_badouble'))
              l_sarg1 <- s_arg1[[index]]
            else
              l_sarg1 <- s_arg1[index]
            if(index == 1)
              ans =(f(l_sarg1,l_sarg2))
            else
              ans = c(f(l_sarg1,l_sarg2), ans)
        }
        return(ans)
    } else if(length(s_arg2)!=1){
        if (extends(argtypes1, '_p_badouble'))
          l_sarg1 <- s_arg1[[1]]
        else
          l_sarg1 <- s_arg1[1]
        for(index in seq(1,length(s_arg2),1)){
            if (extends(argtypes2, '_p_badouble'))
              l_sarg2 <- s_arg2[[index]]
            else
              l_sarg2 <- s_arg2[index]
            if(index == 1)
              ans =(f(l_sarg1,l_sarg2))
            else
              ans = c(f(l_sarg1,l_sarg2), ans)
        }
        return(ans)
    }
    if (extends(argtypes1, '_p_badouble'))
      l_sarg1 <- s_arg1[[1]]
    else
      l_sarg1 <- s_arg1[1]
    if (extends(argtypes2, '_p_badouble'))
      l_sarg2 <- s_arg2[[1]]
    else
      l_sarg2 <- s_arg2[1]
    return(f(l_sarg1,l_sarg2))
}

`adolc_mat_dispatch_unary` = function(s_arg1, f_name) {
    argtypes1 = get_argtype(s_arg1)
    ans <- NULL
    for(index1 in 1:nrow(s_arg1)){
        myrow <- NULL
        for(index2 in 1:ncol(s_arg1)){
            elem <- s_arg1[index1,index2]
            if(is.list(elem))
              elem <-- elem[[1]]
            if (inherits(elem, "ExternalReference")){
              elem = slot(elem,"ref")
            }
            ;obj = .Call(f_name, elem, PACKAGE='adolc');
            obj <- new("_p_adub", ref=obj) ;
            myrow = cbind(myrow, obj)
        }
        ans <- rbind(ans, myrow)
    }
    ans
}

`adolc_mat_dispatch` = function(s_arg1, s_arg2, f) {
    dim_arg1_1 = nrow(s_arg1)
    dim_arg1_2 = ncol(s_arg1)
    dim_arg2_1 = nrow(s_arg2)
    dim_arg2_2 = ncol(s_arg2)
    argtypes1 = get_argtype(s_arg1)
    argtypes2 = get_argtype(s_arg2)
    if(is.matrix(s_arg1) && extends(argtypes1, '_p_badouble') && is.matrix(s_arg2) && extends(argtypes2, '_p_badouble')) {
        if((dim_arg1_1 != dim_arg2_1) || (dim_arg1_2 != dim_arg2_2)){
            stop("adolc_mat_dispatch: Unhandled case: (dim_arg1_1 != dim_arg2_1 || dim_arg1_2 != dim_arg2_2) ")
        }
        ans <- NULL
        for(index1 in 1:nrow(s_arg2)){
            myrow <- NULL
            for(index2 in 1:ncol(s_arg2)){
                l_arg1 <- s_arg1[index1,index2]
                l_arg2 <- s_arg2[index1,index2]
                if(is.list(l_arg1))
                  l_arg1 <- l_arg1[[1]]
                if(is.list(l_arg2))
                  l_arg2 <- l_arg2[[1]]

                if(index2 == 1)
                myrow <- f(l_arg1, l_arg2)
                else
                myrow <- c(myrow, f(l_arg1, l_arg2))
            }
            if(index1 == 1)
            ans <- myrow
            else
            ans <- c(ans, myrow)
        }
        dim(ans) <- c(nrow(s_arg2),ncol(s_arg2))
        return(ans)
    }
    if (is.matrix(s_arg1) && (extends(argtypes1, '_p_badouble') || extends(argtypes2, '_p_badouble'))) {
        if(is.vector(s_arg2) || is.list(s_arg2)) {
            stop("adolc_mat_dispatch: Unhandled case: (is.matrix(s_arg1) && extends(argtypes1, '_p_badouble')) AND  (is.vector(s_arg2) || is.list(s_arg2))")
        } else {
            ans = matrix(0,dim(s_arg1)[1],dim(s_arg1)[2])
            for(index1 in seq(1,dim(s_arg1)[1],1)){
                for(index2 in seq(1,dim(s_arg1)[2],1)){
                    ans[index1,index2] = f(s_arg1[index1,index2], s_arg2)
                }
            }
        }
        return(ans)
    }
    if (is.matrix(s_arg2) && (extends(argtypes1, '_p_badouble') || extends(argtypes2, '_p_badouble'))) {
        if(is.vector(s_arg1) || is.list(s_arg1)) {
            if(length(s_arg1)==1){
                myrow <- NULL
                for(index1 in 1:nrow(s_arg2)){
                    for(index2 in 1:ncol(s_arg2)){
                        obj <- NULL
                        l_arg1 <- NULL
                        if (extends(argtypes1, '_p_badouble'))
                          l_arg1 <- s_arg1[[1]]
                        else
                          l_arg1 <- s_arg1[1]
                        l_arg2 <- s_arg2[index1,index2]
                        if(is.list(l_arg1))
                          l_arg1 <- l_arg1[[1]]
                        if(is.list(l_arg2))
                          l_arg2 <- l_arg2[[1]]
                        obj <- f(l_arg1, l_arg2)
                        if(index2 == 1)
                            myrow <- obj
                        else
                            myrow <- c(myrow, obj)
                    }
                    if(index1 == 1)
                      ans <- myrow
                    else
                      ans <- c(ans, myrow)
                }
                dim(ans) <- c(nrow(s_arg2),ncol(s_arg2))
                return(ans)
            } else {
                stop("adolc_mat_dispatch: Unhandled case: (is.matrix(s_arg2) && extends(argtypes2, '_p_badouble')) AND  (is.vector(s_arg1) || is.list(s_arg1))")
            }
        } else {
            ans <- NULL
            for(index1 in 1:nrow(s_arg2)){
                myrow <- NULL
                for(index2 in 1:ncol(s_arg2)){
                    l_arg2 <- s_arg2[index1,index2]
                    if(is.list(l_arg2))
                      l_arg2 <- l_arg2[[1]]
                    if(index2 == 1)
                      myrow <- f(s_arg1, l_arg2)
                    else
                      myrow <- c(myrow, f(s_arg1, l_arg2))
                }
                if(index1 == 1)
                ans <- myrow
                else
                ans <- c(ans, myrow)
            }
            dim(ans) <- c(nrow(s_arg2),ncol(s_arg2))
            return(ans)
        }
        return(ans)
    }
    ans = f(s_arg1, s_arg2)
    ans
}

