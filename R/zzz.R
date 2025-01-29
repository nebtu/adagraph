.onLoad <- function(libname, pkgname) {
    op <- options()
    op_adagraph <- list(
        adagraph.miwa_maxval = 1e3,
        adagraph.miwa_steps = 128,
        adagraph.precision = 1e-6
    )
    toset <- !(names(op_adagraph) %in% names(op))
    if (any(toset)) options(op_adagraph[toset])

    invisible()
}