siNMF <- function(X, M=NULL, initW=NULL, initH=NULL, fixW=FALSE, fixH=FALSE, J = 3, w=NULL, algorithm = c("Frobenius", "KL", "IS", "PLTF"), p=1,
    thr = 1e-10, num.iter = 100,
    viz = FALSE, figdir = NULL, verbose = FALSE) {
    # Argument check
    if(!is.list(X)){
        stop("input X must be specified as a list!")
    }
    if(length(X) < 2){
        stop("input list X must have at least two datasets!")
    }
    if(!is.null(M)){
        dimX <- as.vector(unlist(lapply(X, function(x){dim(x)})))
        dimM <- as.vector(unlist(lapply(M, function(x){dim(x)})))
        if(!identical(dimX, dimM)){
            stop("Please specify the dimensions of X and M are same")
        }
    }else{
        M <- X
        for(i in seq(length(X))){
            M[[i]][] <- 1
        }
    }
    if(!is.null(initW)){
        if(!identical(nrow(X[[1]]), nrow(initW))){
            stop("Please specify nrow(X[[1]]) and nrow(initW) same")
        }
    }
    if(!is.null(initH)){
        ncolX <- as.vector(unlist(lapply(X, ncol)))
        nrowH <- as.vector(unlist(lapply(initH, nrow)))
        if(!identical(ncolX, nrowH)){
            stop("Please specify all the ncol(initH[[k]]) are same as ncol(X[[k]]) (k=1,2,...)")
        }
    }
    if(!is.logical(fixW)){
        stop("Please specify the fixW as a logical")
    }
    if(!is.logical(fixH)){
        if(!is.vector(fixH)){
            stop("Please specify the fixH as a logical or a logical vector such as c(TRUE, FALSE, TRUE)")
        }else{
            if(length(X) != length(fixH)){
                stop("Please specify the length of fixH same as the length of X")
            }
        }
    }else{
        fixH <- rep(fixH, length=length(X))
    }

    if(!is.numeric(J)){
        stop("Please specify J as numeric!")
    }
    if(is.null(w)){
        w <- rep(1, length=length(X))
    }else{
        if(length(X) != length(w)){
            stop("The length of weight vector must be same as that of input list X!")
        }else{
            w <- w / sum(w)
        }
    }
    algorithm <- match.arg(algorithm)
    if(!is.numeric(p)){
        stop("Please specify p as numeric!")
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

    K <- length(X)
    lapply(seq_along(X), function(x){
        X[[x]][which(X[[x]] == 0)] <<- 1e-10
        M[[x]][which(M[[x]] == 0)] <<- 1e-10
    })

    # Initialization of W, H
    if(is.null(initW)){
        W <- matrix(runif(nrow(X[[1]])*J), nrow=nrow(X[[1]]), ncol=J)
        W <- .columnNorm(W * W)        
    }else{
        W <- initW
    }
    if(is.null(initH)){
        H <- lapply(seq_along(X), function(x){
            tmp <- matrix(runif(ncol(X[[x]])*J),
                nrow=ncol(X[[x]]), ncol=J)
            .columnNorm(tmp * tmp)
        })        
    }else{
        H <- initH
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
    # Algorithm
    if (algorithm == "Frobenius") {
        p = 0
    }
    if (algorithm == "KL") {
        p = 1
    }
    if (algorithm == "IS") {
        p = 2
    }
    if (algorithm == "PLTF") {
        p = p
    }
    if (verbose) {
        cat("Iterative step is running...\n")
    }
    while ((RecError[iter] > thr) && (iter <= num.iter)) {
        # Before Update W, H_k
        X_bar <- lapply(H, function(h){
            .recMatrix(W, h)
        })
        pre_Error <- sqrt(sum(unlist(lapply(seq_along(X), function(x){
            .recError(X[[x]], X_bar[[x]], notsqrt=TRUE)
        }))))

        # Update W
        if(!fixW){            
            W_numer <- matrix(0, nrow=nrow(W), ncol=ncol(W))
            W_denom <- matrix(0, nrow=nrow(W), ncol=ncol(W))
            for(k in seq_len(K)){
                W_numer <- W_numer + w[k] * (M[[k]]*X[[k]] * (M[[k]]*X_bar[[k]])^(-p)) %*% H[[k]]
                W_denom <- W_denom + w[k] * (M[[k]] * W %*% t(H[[k]]))^(1-p) %*% H[[k]]
            }
            W <- .columnNorm(.positive(W * W_numer / W_denom))
        }

        # Update H_k
        for(k in seq_len(K)){
            if(!fixH[k]){
                Hk_numer <- (t(M[[k]]*X[[k]]) * t(M[[k]]*X_bar[[k]])^(-p)) %*% W
                Hk_denom <- t(M[[k]] * W %*% t(H[[k]]))^(1-p) %*% W
                H[[k]] <- H[[k]] * Hk_numer / Hk_denom                    
            }
        }

        # After Update W, H_k
        iter <- iter + 1
        X_bar <- lapply(H, function(h){
            .recMatrix(W, h)
        })
        RecError[iter] <- sqrt(sum(unlist(lapply(seq_along(X), function(x){
            .recError(X[[x]], X_bar[[x]], notsqrt=TRUE)
        }))))

        TrainRecError[iter] <- sqrt(sum(unlist(lapply(seq_along(X), function(x){
            .recError(M[[x]]*X[[x]], M[[x]]*X_bar[[x]], notsqrt=TRUE)
        }))))

        TestRecError[iter] <- sqrt(sum(unlist(lapply(seq_along(X), function(x){
            .recError((1-M[[x]])*X[[x]], (1-M[[x]])*X_bar[[x]], notsqrt=TRUE)
        }))))

        RelChange[iter] <- abs(pre_Error - RecError[iter]) / RecError[iter]
        if (viz && !is.null(figdir)) {
            png(filename = paste0(figdir, "/", iter-1, ".png"))
            .imageplot_siNMF(X, W, H)
            dev.off()
        }
        if (viz && is.null(figdir)) {
        .imageplot_siNMF(X, W, H)
        }
        if (verbose) {
            cat(paste0(iter-1, " / ", num.iter, " |Previous Error - Error| / Error = ",
                RelChange[iter], "\n"))
        }
        if (is.nan(RelChange[iter])) {
            stop("NaN is generated. Please run again or change the parameters.\n")
        }
    }
    if (viz && !is.null(figdir)) {
        png(filename = paste0(figdir, "/finish.png"))
        .imageplot_siNMF(X, W, H)
        dev.off()
    }
    if (viz && is.null(figdir)) {
        .imageplot_siNMF(X, W, H)
    }
    names(RecError) <- c("offset", 1:(iter-1))
    names(TrainRecError) <- c("offset", 1:(iter-1))
    names(TestRecError) <- c("offset", 1:(iter-1))
    names(RelChange) <- c("offset", 1:(iter-1))
    return(list(W = W, H = H,
        RecError = RecError,
        TrainRecError = TrainRecError,
        TestRecError = TestRecError,
        RelChange = RelChange))
}

.imageplot_siNMF <- function(X, W, H){
    l <- length(X)
    layout(rbind(seq_len(l), seq_len(l)+l))
    lapply(seq_along(X), function(x){
        image.plot(t(X[[l]]))
    })
    lapply(seq_along(X), function(x){
        image.plot(t(W %*% t(H[[x]])))
    })
}