.onLoad <- function(libname, pkgname) {
  op <- options()
  op_adagraph <- list(
    adagraph.miwa_maxval = 1e3,
    adagraph.miwa_steps = 128,
    adagraph.precision = 1e-6
  )
  toset <- !(names(op_adagraph) %in% names(op))
  if (any(toset)) options(op_adagraph[toset])

  # Note that will not work well with parallelization, but provides a nice speedup for now
  get_cer <<- memoise::memoise(get_cer)
  cer_prep_bounds <<- memoise::memoise(cer_prep_bounds)

  invisible()
}
