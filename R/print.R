#' @export
print.cer_design <- function(design) {
  k <- attr(design, "k")
  cli::cat_line(
    "A CER Design object, for testing ",
    k,
    " hypotheses at FWER ",
    design$alpha,
    "."
  )
  cli::cat_line()
  invisible(design)

  cli::cat_rule("Inital design specification")
  cli::cat_line()

  cli::cat_line("Hypotheses weights")
  cli::cat_print(design$weights)
  cli::cat_line()

  cli::cat_line("Graph Transition Matrix")
  cli::cat_print(design$test_m)
  cli::cat_line()

  if (!all(is.na(design$correlation))) {
    cli::cat_line("Correlation for parametric test")
    cli::cat_print(design$correlation)
    cli::cat_line()
  }

  cli::cat_line("Interim test is planned at time fraction ", design$t)
  cli::cat_line()

  if (design$interim_test) {
    cli::cat_rule("Interim test result")
    cli::cat_line("")

    cli::cat_line("P-values of interim test are:")
    cli::cat_print(design$p_values_interim)
    if (any(design$rej_interim)) {
      cli::cat_line(
        "Hypotheses rejected at the interim: ",
        which(design$rej_interim)
      )
    } else {
      cli::cat_line("No Hypotheses were rejected at the interim")
    }
    cli::cat_line("")
  }

  if (design$adaptions) {
    cli::cat_rule("Adaptions from inital specification")
    cli::cat_line()
    if (!identical(design$ad_weights, design$weights)) {
      cli::cat_line("New hypotheses weights")
      cli::cat_print(design$ad_weights)
      cli::cat_line()
    }
    if (!identical(design$ad_test_m, design$test_m)) {
      cli::cat_line("New graph Transition Matrix")
      cli::cat_print(design$ad_test_m)
      cli::cat_line()
    }
    if (!identical(design$ad_correlation, design$correlation)) {
      cli::cat_line("New correlation for parametric test")
      cli::cat_print(design$ad_correlation)
      cli::cat_line()
    }
    if (!identical(design$ad_t, design$t)) {
      cli::cat_line("New time fractions for the hypotheses")
      cli::cat_print(design$ad_t)
      cli::cat_line()
    }
  }

  if (design$final_test) {
    cli::cat_rule("Final test result")
    cli::cat_line("")

    cli::cat_line("Overall p-values of the hypotheses are:")
    cli::cat_print(design$p_values_final)
    if (any(design$rej)) {
      cli::cat_line(
        "Hypotheses rejected: ",
        which(design$rej)
      )
    } else {
      cli::cat_line("No Hypotheses were rejected")
    }
  }
}
