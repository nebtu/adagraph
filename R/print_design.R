#' Shared internal helper used by S3 print methods
#'
#' @param x design to be printed
#' @param header_label label describing the type of design being printed
#' @param hooks list containing functions for printing additional information, should
#'   have names out of c("after_initial_spec", "after_adaptions"). Each function
#'   should only take x as argument
#'
#' @noRd
print_design_common <- function(x, header_label = "CER", hooks = list()) {
  k <- attr(x, "k")
  cli::cat_line(
    "A ",
    header_label,
    " Design object, for testing ",
    k,
    " hypotheses at FWER ",
    x[["alpha"]],
    "."
  )
  cli::cat_line()

  # Allow class-specific additions inside the initial spec section
  if (is.function(hooks$after_initial_spec)) {
    hooks$after_initial_spec(x)
  }

  # Interim results (only if flagged and fields available)
  if (isTRUE(x[["interim_test"]])) {
    cli::cat_rule("An interim test has been performed.")
    if (any(x[["rej_interim"]])) {
      rej <- x[["names"]][x[["rej_interim"]]]
      cli::cat_line(
        cli::format_inline(
          "Hypotheses rejected at the interim: {rej}"
        )
      )
    } else {
      cli::cat_line("No Hypotheses were rejected at the interim.")
    }
  } else {
    cli::cat_rule("No interim test has been performed yet.")
  }

  # Adaptations section (print only available changes)
  if (isTRUE(x[["adaptions"]])) {
    cli::cat_rule("The following characteristics have been adapted:")
    if (
      !is.null(x[["ad_weights"]]) &&
        !identical(x[["ad_weights"]], x[["weights"]])
    ) {
      cli::cat_line(cli::format_bullets_raw(c("*" = "Hypotheses weights")))
    }
    if (
      !is.null(x[["ad_test_m"]]) && !identical(x[["ad_test_m"]], x[["test_m"]])
    ) {
      cli::cat_line(cli::format_bullets_raw(c("*" = "Graph Transition Matrix")))
    }
    if (
      !is.null(x[["ad_correlation"]]) &&
        !identical(x[["ad_correlation"]], x[["correlation"]])
    ) {
      cli::cat_line(cli::format_bullets_raw(c(
        "*" = "Correlation for parametric test"
      )))
    }
    if (!is.null(x[["ad_t"]]) && !identical(x[["ad_t"]], x[["t"]])) {
      cli::cat_line(cli::format_bullets_raw(c(
        "*" = "Time fractions for the hypotheses"
      )))
    }
  } else {
    cli::cat_rule("No adaptions have been performed yet")
  }

  if (is.function(hooks$after_adaptions)) {
    hooks$after_adaptions(x)
  }

  # Final results if available
  if (isTRUE(x[["final_test"]])) {
    cli::cat_rule("Final test result")
    if (any(x[["rej"]])) {
      rej <- x[["names"]][x[["rej"]]]
      cli::cat_line(cli::format_inline(
        "Hypotheses rejected: {rej}"
      ))
    } else {
      cli::cat_line("No Hypotheses were rejected")
    }
  } else {
    cli::cat_rule("No final test has been performed yet")
  }

  invisible(x)
}

#' @export
print.multiarm_cer_design <- function(x, ...) {
  hooks <- list(
    after_initial_spec = function(x) {
      # Multi-arm specifics within the initial specification
      if (!is.null(x[["controls"]])) {
        cli::cat_line(
          cli::format_inline(
            "There are {x[[\"controls\"]]} control groups for a total of {attr(x, \"k\")} hypotheses."
          )
        )
      }
      cli::cat_line()
    }
  )
  print_design_common(x, header_label = "Multi-arm", hooks = hooks)
}

#' @export
print.cer_design <- function(x, ...) {
  print_design_common(x, header_label = "CER")
}

#' @export
print.adagraph_design <- function(x, ...) {
  # Generic graph design printer using the common helper
  print_design_common(x, header_label = "Adagraph")
}
