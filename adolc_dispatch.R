
`adolc_dispatch` = function(s_arg1, f_name_passive, f_name) {
    argtype <- class(s_arg1);
    if(is.list(s_arg1)){
        if(is.list(s_arg1[1])){
            loclist<-s_arg1[1]
            argtype <- class(s_arg1[[1]])
        }
    }
    if (!extends(argtype, '_p_badouble')) {
        ans <- f_name_passive(s_arg1)
    } else {
        if (inherits(s_arg1, "ExternalReference")) s_arg1 = slot(s_arg1,"ref")
        ;ans = .Call(f_name, s_arg1, PACKAGE='adolc');
        ans <- new("_p_adub", ref=ans) ;
    }
    ans
    
}

`adolc_operator_dispatch` = function(..., f) {
    argtypes <- mapply(class, list(...));
    argv <- list(...);
    argc <- length(argtypes);
    if(is.list(argv[1])){
        if(is.list(argv[[1]])){
            loclist<-argv[[1]]
            argtypes[1] <- class(loclist[[1]])
        }
    }
    if(argc>1 && is.list(argv[2])){
        loclist<-argv[[2]]
        if(is.list(loclist)){
            argtypes[2] <- class(loclist[[1]])
        }
    }
    
    if (argc == 1) {
        if(is.list(argv[[1]]) && extends(argtypes[1], '_p_badouble')) {
            return(f( (argv[[1]])[[1]]))
        } else {
            return(f(...));
        }
    } else if (argc == 2) {
        if(length(argv[[1]])!=1 && length(argv[[2]])!=1) {
            stop("adolc_operator_dispatch: Unhandled case: (length(argv[[1]])!=1 && length(argv[[2]])!=1) ")
        }
        if(is.list(argv[[1]]) && extends(argtypes[1], '_p_badouble') && is.list(argv[[2]]) && extends(argtypes[2], '_p_badouble')) {
            return(adolc_vec_dispatch(argv[[1]], argv[[2]], f))
        } else if (is.list(argv[[1]]) && extends(argtypes[1], '_p_badouble')) {
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
        } else if (is.list(argv[[2]]) && extends(argtypes[2], '_p_badouble')) {
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
    if (is.list(s_arg1) && is.list(s_arg2))
      return(adolc_list_list_dispatch(s_arg1,s_arg2,f))
    if (is.list(s_arg1) && is.vector(s_arg2))
      return(adolc_list_vec_dispatch(s_arg1,s_arg2,f))
    if (is.vector(s_arg1) && is.list(s_arg2))
      return(adolc_vec_list_dispatch(s_arg1,s_arg2,f))
    if (is.vector(s_arg1) && is.vector(s_arg2))
      return(adolc_vec_vec_dispatch(s_arg1,s_arg2,f))
    stop("adolc_vec_dispatch: Unhandled case ")
 
}


`adolc_list_list_dispatch` = function(s_arg1, s_arg2, f) {
    if(length(s_arg1)!=1 && length(s_arg2)!=1 && length(s_arg1)!=length(s_arg2)){
        stop("adolc_list_list_dispatch: Unhandled case: (length(s_arg1)!=1 && length(s_arg2)!=1 && length(s_arg1)!=length(s_arg2)) ")
    }
    if(length(s_arg1)!=1 && length(s_arg1)==length(s_arg2)){
        #print("adolc_list_list_dispatch: CASE1 ")
        for(index in seq(1,length(s_arg1),1)){
            if(index == 1)
            ans =(f(s_arg1[[index]],s_arg2[[index]]))
            else
            ans = c(f(s_arg1[[index]],s_arg2[[index]]), ans)
            
        }
        return(ans)
    } else if(length(s_arg1)!=1){
        #print("adolc_list_list_dispatch: CASE2 ")
        for(index in seq(1,length(s_arg1),1)){
            if(index == 1)
            ans =(f(s_arg1[[index]], s_arg2[[1]]))
            else
            ans = c(f(s_arg1[[index]], s_arg2[[1]]), ans)
        }
        return(ans)
    } else if(length(s_arg2)!=1){
        #print("adolc_list_list_dispatch: CASE3 ")
        for(index in seq(1,length(s_arg2),1)){
            if(index == 1)
            ans =(f(s_arg1[[1]], s_arg2[[index]]))
            else
            ans = c(f(s_arg1[[1]], s_arg2[[index]]), ans)
        }
        return(ans)
    }
    #print("adolc_list_list_dispatch: CASE4 ")
    return(f(s_arg1[[1]],s_arg2[[1]]))
}

`adolc_vec_list_dispatch` = function(s_arg1, s_arg2, f) {
    if(length(s_arg1)!=1 && length(s_arg2)!=1 && length(s_arg1)!=length(s_arg2)){
        stop("adolc_vec_list_dispatch: Unhandled case: (length(s_arg1)!=1 && length(s_arg2)!=1 && length(s_arg1)!=length(s_arg2)) ")
    }
    if(length(s_arg1)!=1 && length(s_arg1)==length(s_arg2)){
        #print("adolc_vec_list_dispatch: CASE1 ")
        for(index in seq(1,length(s_arg1),1)){
            if(index == 1)
            ans =(f(s_arg1[index],s_arg2[[index]]))
            else
            ans = c(f(s_arg1[index], s_arg2[[index]]), ans)
            
        }
        return(ans)
    } else if(length(s_arg1)!=1){
        #print("adolc_vec_list_dispatch: CASE2 ")
        for(index in seq(1,length(s_arg1),1)){
            if(index == 1)
              ans =(f(s_arg1[index], s_arg2[[1]]))
            else
              ans = c(f(s_arg1[index], s_arg2[[1]]), ans)
        }
        return(ans)
    } else if(length(s_arg2)!=1){
        #print("adolc_vec_list_dispatch: CASE3 ")
        for(index in seq(1,length(s_arg2),1)){
            if(index == 1)
            ans =(f(s_arg1[1], s_arg2[[index]]))
            else
            ans = c(f(s_arg1[1], s_arg2[[index]]), ans)
        }
        return(ans)
    }
    #print("adolc_vec_list_dispatch: CASE4 ")
    return(f(s_arg1[1],s_arg2[[1]]))
}

`adolc_list_vec_dispatch` = function(s_arg1, s_arg2, f) {
    if(length(s_arg1)!=1 && length(s_arg2)!=1 && length(s_arg1)!=length(s_arg2)){
        stop("adolc_list_vec_dispatch: Unhandled case: (length(s_arg1)!=1 && length(s_arg2)!=1 && length(s_arg1)!=length(s_arg2)) ")
    }
    if(length(s_arg1)!=1 && length(s_arg1)==length(s_arg2)){
        #print("adolc_list_vec_dispatch: CASE1 ")
        for(index in seq(1,length(s_arg1),1)){
            if(index == 1)
              ans =(f(s_arg1[[index]],s_arg2[index]))
            else
              ans = c(f(s_arg1[[index]],s_arg2[index]), ans)
            
        }
        return(ans)
    } else if(length(s_arg1)!=1){
        #print("adolc_list_vec_dispatch: CASE2 ")
        for(index in seq(1,length(s_arg1),1)){
            if(index == 1)
            ans =(f(s_arg1[[index]], s_arg2[1]))
            else
            ans = c(f(s_arg1[[index]], s_arg2[1]), ans)
        }
        return(ans)
    } else if(length(s_arg2)!=1){
        #print("adolc_list_vec_dispatch: CASE3 ")
        for(index in seq(1,length(s_arg2),1)){
            if(index == 1)
            ans =(f(s_arg1[[1]], s_arg2[index]))
            else
            ans = c(f(s_arg1[[1]], s_arg2[index]), ans)
        }
        return(ans)
    }
    #print("adolc_list_vec_dispatch: CASE4 ")
    return(f(s_arg1[[1]],s_arg2[1]))
}

`adolc_vec_vec_dispatch` = function(s_arg1, s_arg2, f) {
    if(length(s_arg1)!=1 && length(s_arg2)!=1 && length(s_arg1)!=length(s_arg2)){
        stop("adolc_vec_vec_dispatch: Unhandled case: (length(s_arg1)!=1 && length(s_arg2)!=1 && length(s_arg1)!=length(s_arg2)) ")
    }
    if(length(s_arg1)!=1 && length(s_arg1)==length(s_arg2)){
        #print("adolc_vec_vec_dispatch: CASE1 ")
        for(index in seq(1,length(s_arg1),1)){
            if(index == 1)
            ans =(f(s_arg1[index],s_arg2[index]))
            else
            ans = c(f(s_arg1[index],s_arg2[index]), ans)
            
        }
        return(ans)
    } else if(length(s_arg1)!=1){
        #print("adolc_vec_vec_dispatch: CASE2 ")
        for(index in seq(1,length(s_arg1),1)){
            if(index == 1)
            ans =(f(s_arg1[index], s_arg2[1]))
            else
            ans = c(f(s_arg1[index], s_arg2[1]), ans)
        }
        return(ans)
    } else if(length(s_arg2)!=1){
        #print("adolc_vec_vec_dispatch: CASE3 ")
        for(index in seq(1,length(s_arg2),1)){
            if(index == 1)
            ans =(f(s_arg1[1], s_arg2[index]))
            else
            ans = c(f(s_arg1[1], s_arg2[index]), ans)
        }
        return(ans)
    }
    #print("adolc_vec_vec_dispatch: CASE4 ")
    return(f(s_arg1[1],s_arg2[1]))
}

