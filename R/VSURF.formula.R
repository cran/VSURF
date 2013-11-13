VSURF.formula <- function(formula, data, ..., na.action = na.fail) {
    ### formula interface for VSURF.
    ### code gratefully stolen from svm.formula (package e1071).
    ###
    if (!inherits(formula, "formula"))
        stop("method is only for formula objects")
    m <- match.call(expand.dots = FALSE)
    ## Catch xtest and ytest in arguments.
    if (any(c("xtest", "ytest") %in% names(m)))
        stop("xtest/ytest not supported through the formula interface")
    names(m)[2] <- "formula"
    if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m$... <- NULL
    m$na.action <- na.action
    m[[1]] <- as.name("model.frame")
    m <- eval(m, parent.frame())
    y <- model.response(m)
    Terms <- attr(m, "terms")
    attr(Terms, "intercept") <- 0
    ## Drop any "negative" terms in the formula.
    ## test with:
    ## randomForest(Fertility~.-Catholic+I(Catholic<50),data=swiss,mtry=2)
    m <- model.frame(terms(reformulate(attributes(Terms)$term.labels)),
                     data.frame(m))
    ## if (!is.null(y)) m <- m[, -1, drop=FALSE]
    for (i in seq(along=ncol(m))) {
        if (is.ordered(m[[i]])) m[[i]] <- as.numeric(m[[i]])
    }
    ret <- VSURF(x=m, y=y, ...)
    cl <- match.call()
    cl[[1]] <- as.name("VSURF")
    ret$call <- cl
    ret$terms <- Terms
    if (!is.null(attr(m, "na.action")))
        ret$na.action <- attr(m, "na.action")
    class(ret) <- c("VSURF.formula", "VSURF")
    warning(
        "VSURF with a formula-type call outputs selected variables
  which are indices of the input matrix based on the formula:
  you may reorder these to get indices of the original data")
    return(ret)
}
