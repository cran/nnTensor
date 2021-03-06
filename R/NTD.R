NTD <- function(X, M=NULL, initS=NULL, initA=NULL, fixS=FALSE, fixA=FALSE, rank = c(3, 3, 3), modes = 1:3, algorithm = c("Frobenius", "KL", "IS", "Pearson", "Hellinger", "Neyman", "HALS", "Alpha", "Beta"), init = c("NMF", "ALS", "Random"),
    Alpha = 1, Beta = 2, thr = 1e-10, num.iter = 100, viz = FALSE,
    figdir = NULL, verbose = FALSE){
    # Argument check
    if(!is.array(X@data)){
        stop("input X@data must be specified as a array!")
    }
    if(!is.null(M)){
        if(!identical(dim(X), dim(M))){
            stop("Please specify the dimensions of X and M are same")
        }
    }else{
        M <- X
        M@data[] <- 1
    }
    if(!is.null(initS)){
        dimS <- as.numeric(dim(initS)[modes])
        if(!identical(rank, dimS)){
            stop("Please specify the rank and dim(S) are same")
        }        
    }
    if(!is.null(initA)){
        nrowA <- as.numeric(unlist(lapply(initA, nrow)))[modes]
        if(!identical(rank, nrowA)){
            stop("Please specify the rank and nrow(A[[k]]) are same")
        }
    }
    if(!is.logical(fixS)){
        if(!"Tensor" %in% is(X)){
            stop("Please specify the fixS as a logical or a Tensor object")
        }else{
            if(!identical(rank, dim(fixS))){
                stop("Please specify the dimensions of fixS same as the rank")
            }
        }
    }
    if(!is.logical(fixA)){
        if(!is.vector(fixA)){
            stop("Please specify the fixA as a logical or a logical vector such as c(TRUE, FALSE, TRUE)")
        }else{
            if(length(modes) != length(fixA)){
                stop("Please specify the length of fixA same as length(modes)")
            }
        }
    }
    tmp <- rep(FALSE, length=length(dim(X)))
    tmp[modes] <- fixA
    fixA <- tmp

    if(!is.numeric(rank)){
        stop("Please specify rank as numeric vector!")
    }
    if(!is.numeric(modes)){
        stop("Please specify modes as numeric vector!")
    }
    algorithm <- match.arg(algorithm)
    init <- match.arg(init)
    if(!is.numeric(Alpha)){
        stop("Please specify Alpha as numeric!")
    }
    if(!is.numeric(Beta)){
        stop("Please specify Beta as numeric!")
    }
    if(!is.numeric(thr)){
        stop("Please specify thr as numeric!")
    }
    if(!is.numeric(num.iter)){
        stop("Please specify num.iter as numeric!")
    }
    if(!is.logical(viz)){
        stop("Please specify the viz as a logical")
    }
    if(!is.character(figdir) && !is.null(figdir)){
        stop("Please specify the figdir as a string or NULL")
    }
    if(!is.logical(verbose)){
        stop("Please specify the verbose as a logical")
    }

    if (verbose) {
        cat("Initialization step is running...\n")
    }
    modes <- unique(modes)
    modes <- modes[order(modes)]
    if(length(modes) != length(rank)){
        stop("Please the length(modes) and length(rank) as same")
    }
    X <- .pseudocount(X)
    M <- .pseudocount(M)
    N <- length(dim(X))

    # Initialization of An and S
    A <- list()
    length(A) <- N
    Iposition <- setdiff(seq_len(N), modes)
    rank <- .insertNULL(rank, Iposition, N)
    if(is.null(initA)){
        if (init == "NMF") {
            sapply(modes, function(n) {
                Xn <- cs_unfold(X, m = n)@data
                An <- t(NMF(Xn, J = rank[n], algorithm = "KL")$V)
                A[[n]] <<- t(apply(An, 1, function(x) {
                    x/norm(as.matrix(x), "F")
                }))
            })
        } else if (init == "ALS") {
            sapply(modes, function(n) {
                Xn <- cs_unfold(X, m = n)@data
                An <- .positive(svd(Xn)$u[1:rank[n], ])
                if(is.vector(An)){
                    An <- as.matrix(An)
                }
                A[[n]] <<- t(apply(An, 1, function(x) {
                    x/norm(as.matrix(x), "F")
                }))
            })
        } else if (init == "Random") {
            sapply(modes, function(n) {
                A[[n]] <<- matrix(runif(rank[n] * dim(X)[n]),
                    nrow = rank[n], ncol = dim(X)[n])
            })
        }
        sapply(Iposition, function(n){
            A[[n]] <<- diag(dim(X)[n])
        })        
    }else{
        A <- initA
    }
    names(A)[modes] <- paste0("A", modes)
    names(A)[Iposition] <- paste0("I", seq_along(Iposition))

    if(is.null(initS)){
        S <- recTensor(S=X, A=A, idx=modes, reverse = TRUE)
    }else{
        S <- initS
    }

    RecError = c()
    TrainRecError = c()
    TestRecError = c()
    RelChange = c()

    RecError[1] <- thr * 10
    TrainRecError[1] <- thr * 10
    TestRecError[1] <- thr * 10
    RelChange[1] <- thr * 10

    iter <- 1
    J_hat <- NULL
    if (algorithm == "HALS") {
        E <- X - recTensor(S=S, A=A, idx=modes)
        eval(parse(text=.HALSCMD1(N)))
    }
    if (algorithm == "Frobenius") {
        Beta = 2
        algorithm = "Beta"
    }
    if (algorithm == "KL") {
        Alpha = 1
        algorithm = "Alpha"
    }
    if (algorithm == "IS") {
        Beta = 0
        algorithm = "Beta"
    }
    if (algorithm == "Pearson") {
        Alpha = 2
        algorithm = "Alpha"
    }
    if (algorithm == "Hellinger") {
        Alpha = 0.5
        algorithm = "Alpha"
    }
    if (algorithm == "Neyman") {
        Alpha = -1
        algorithm = "Alpha"
    }
    if (verbose) {
        cat("Iterative step is running...\n")
    }
    while ((RecError[iter] > thr) && (iter <= num.iter)) {
        X_bar <- recTensor(S=S, A=A, idx=modes)
        pre_Error <- .recError(X, X_bar)

        # Update An
        for (n in modes) {
            if(!fixA[n]){
                if (algorithm == "Alpha") {
                    S_A <- t(cs_unfold(S, m = n)@data) %*% kronecker_list(sapply(rev(setdiff(1:N,
                      n)), function(x) {
                      A[[x]]
                    }, simplify = FALSE))
                    Xn <- cs_unfold(X, m = n)@data
                    Mn <- cs_unfold(M, m = n)@data
                    numer <- S_A %*% (Mn * (Xn/t(t(A[[n]]) %*% S_A)))^Alpha
                    denom <- t(as.matrix(rep(1, dim(X)[n]) %*% t(rowSums(S_A))))
                    A[[n]] <- A[[n]] * (numer/denom)^(1/Alpha)
                }
                else if (algorithm == "Beta") {
                    S_A <- t(cs_unfold(S, m = n)@data) %*% kronecker_list(sapply(rev(setdiff(1:N,
                      n)), function(x) {
                      A[[x]]
                    }, simplify = FALSE))
                    Xn <- cs_unfold(X, m = n)@data
                    Mn <- cs_unfold(M, m = n)@data
                    Xn_bar <- cs_unfold(recTensor(S=S, A=A), m = n)@data
                    numer <- S_A %*% ((Mn * Xn) * (Mn * Xn_bar^(Beta - 1)))
                    denom <- S_A %*% (t(S_A) %*% A[[n]])^Beta
                    A[[n]] <- A[[n]] * numer/denom
                }
                else if (algorithm == "HALS") {
                    X_bar <- recTensor(S=S, A=A, idx = setdiff(1:N, n))
                    for (jn in 1:nrow(A[[n]])) {
                      X_barkn <- .slice(X_bar, mode = n, column = jn)
                      wjn <- fnorm(X_barkn)^2
                      ajn <- .positive(A[[n]][jn, ] + .contProd(E,
                        X_barkn, mode = n)/wjn)
                      E <- E + ttm(X_barkn, as.matrix(A[[n]][jn,
                        ] - ajn), m = n)
                      A[[n]][jn, ] <- ajn / norm(as.matrix(ajn), "F")
                    }
                }
                else {
                    stop("Please specify the appropriate algorithm\n")
                }
            }
        }
        #
        # Normalization of factor matrices
        #
        for (n in modes) {        
            A[[n]] <- t(apply(A[[n]], 1, function(x) {
                x/norm(as.matrix(x), "F")
            }))
        }
        #
        # Update Core tensor
        #
        if(!fixS){
            if (algorithm == "Alpha") {
                S <- .positive(recTensor(S=X, A=A, idx=modes, reverse = TRUE))
                numer <- M * (X/recTensor(S=S, A=A, idx=modes))^Alpha
                denom <- X
                cmd <- paste0("denom[",
                    paste(rep("", length=length(dim(X))), collapse=","), "] <- 1")
                eval(parse(text=cmd))
                denom <- M * denom
                for (n in 1:N) {
                    numer <- ttm(numer, A[[n]], m = n)
                    denom <- ttm(denom, A[[n]], m = n)
                }
                S <- S * (numer/denom)^(1/Alpha)
            }
            else if (algorithm == "Beta") {
                X_bar <- recTensor(S=S, A=A, idx=modes)
                numer <- M * X * X_bar^(Beta - 1)
                denom <- M * X_bar^Beta
                for (n in 1:N) {
                    numer <- ttm(numer, A[[n]], m = n)
                    denom <- ttm(denom, A[[n]], m = n)
                }
                S <- S * numer/denom
            }
            else if (algorithm == "HALS") {
                for (j_ijk in 1:length(J_hat)) {
                    eval(parse(text=.HALSCMD2(N)))
                    eval(parse(text=.HALSCMD3(N)))
                    eval(parse(text=.HALSCMD4(N)))
                    eval(parse(text=.HALSCMD5(N)))
                    eval(parse(text=.HALSCMD6(N)))
                    eval(parse(text=.HALSCMD7(N)))
                }
            }
            else {
                stop("Please specify the appropriate algorithm\n")
            }            
        }

        # NaN
        for (n in modes) {
            if (any(is.infinite(A[[n]])) || any(is.nan(A[[n]]))) {
                stop("Inf or NaN is generated!\n")
            }
        }
        # After Update U, V
        iter <- iter + 1
        X_bar <- recTensor(S=S, A=A, idx=modes)

        RecError[iter] <- .recError(X, X_bar)

        TrainRecError[iter] <- .recError(M*X, M*X_bar)

        TestRecError[iter] <- .recError((1-M)*X, (1-M)*X_bar)

        RelChange[iter] <- abs(pre_Error - RecError[iter]) / RecError[iter]

        if (viz && !is.null(figdir) && N == 3) {
            png(filename = paste0(figdir, "/", iter, ".png"))
            plotTensor3D(X_bar)
            dev.off()
        }
        if (viz && is.null(figdir) && N == 3) {
            plotTensor3D(X_bar)
        }
        if (verbose) {
            cat(paste0(iter-1, " / ", num.iter, " |Previous Error - Error| / Error = ",
                RelChange[iter], "\n"))
        }
        if (is.nan(RelChange[iter])) {
            stop("NaN is generated. Please run again or change the parameters.\n")
        }
    }
    if (viz && !is.null(figdir) && N == 3) {
        png(filename = paste0(figdir, "/finish.png"))
        plotTensor3D(X_bar)
        dev.off()
        png(filename = paste0(figdir, "/original.png"))
        plotTensor3D(X)
        dev.off()
    }
    if (viz && is.null(figdir) && N == 3) {
        plotTensor3D(X_bar)
    }
    names(RecError) <- c("offset", 1:(iter-1))
    names(TrainRecError) <- c("offset", 1:(iter-1))
    names(TestRecError) <- c("offset", 1:(iter-1))
    names(RelChange) <- c("offset", 1:(iter-1))

    return(list(S = S, A = A,
        RecError = RecError,
        TrainRecError = TrainRecError,
        TestRecError = TestRecError,
        RelChange = RelChange))
}
