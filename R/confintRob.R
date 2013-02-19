##' Compute robust confidence interval
##'
##'
##' @title Compute robust confidence interval
##' @param object
##' @param parm
##' @param level
##' @param vcov
##' @param ...
##' @return confint
##' @export
##' @author ahmadou
confintRob <- function(object, parm, level = 0.95, vcov = NULL, ...) {
    if (is.null(vcov)) vcov <- vcov(object)
    cf <- coef(object)
    pnames <- names(cf)
    if (missing(parm))
        parm <- pnames
    else if (is.numeric(parm))
        parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- stats:::format.perc(a, 3)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
        pct))
    ses <- sqrt(diag(vcov))[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}
