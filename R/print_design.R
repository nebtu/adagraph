#' Capture print output as cli verbatim text
#'
#' Helper to avoid repeating `cli::cli_verbatim(utils::capture.output(print(...)))`
#' throughout the format methods. Captures the printed representation of an object
#' and emits it as pre-formatted cli output.
#'
#' @param x object to print
#' @param ... additional arguments passed to [print()]
#'
#' @noRd
cli_print <- function(x, ...) {
  cli::cli_verbatim(utils::capture.output(print(x, ...)))
}

#' Shared internal helper used by S3 format methods
#'
#' @param x design to be formatted
#' @param header_label label describing the type of design being formatted
#' @param hooks list containing functions for formatting additional information, should
#'   have names out of c("after_initial_spec", "after_adaptations"). Each function
#'   should take x and ... as arguments and use cli semantic functions.
#' @param ... additional arguments passed to print calls (e.g. digits)
#'
#' @noRd
format_design_common <- function(x, header_label = "CER", hooks = list(), ...) {
  k <- attr(x, "k")
  cli::cli_format_method({
    cli::cli_h1("{header_label} Design")
    cli::cli_text(
      "Testing the {k} hypotheses {x[[\"names\"]]} at FWER {x[[\"alpha\"]]}."
    )

    # Allow class-specific additions inside the initial spec section
    if (is.function(hooks[["after_initial_spec"]])) {
      hooks[["after_initial_spec"]](x, ...)
    }

    # Interim results (only if flagged and fields available)
    if (isTRUE(x[["interim_test"]])) {
      cli::cli_h2("Interim test")
      if (any(x[["rej_interim"]])) {
        rej <- x[["names"]][x[["rej_interim"]]]
        cli::cli_text(
          "{.strong Hypotheses rejected:} {rej}"
        )
      } else {
        cli::cli_text("No hypotheses were rejected at the interim.")
      }
    } else {
      cli::cli_h2("No interim test performed")
    }

    # Adaptations section (print only available changes)
    if (isTRUE(x[["adaptations"]])) {
      cli::cli_h2("Adaptations")
      items <- character()
      if (
        !is.null(x[["ad_weights"]]) &&
          !identical(x[["ad_weights"]], x[["weights"]])
      ) {
        items <- c(items, "Hypotheses weights")
      }
      if (
        !is.null(x[["ad_test_m"]]) &&
          !identical(x[["ad_test_m"]], x[["test_m"]])
      ) {
        items <- c(items, "Graph Transition Matrix")
      }
      if (
        !is.null(x[["ad_correlation"]]) &&
          !identical(x[["ad_correlation"]], x[["correlation"]])
      ) {
        items <- c(items, "Correlation for parametric test")
      }
      if (!is.null(x[["ad_t"]]) && !identical(x[["ad_t"]], x[["t"]])) {
        items <- c(items, "Time fractions for the hypotheses")
      }
      if (length(items) > 0) {
        cli::cli_text("The following characteristics have been adapted:")
        cli::cli_ul(items)
      }
    } else {
      cli::cli_h2("No adaptations performed")
    }

    if (is.function(hooks[["after_adaptations"]])) {
      hooks[["after_adaptations"]](x, ...)
    }

    # Final results if available
    if (isTRUE(x[["final_test"]])) {
      cli::cli_h2("Final test result")
      if (any(x[["rej"]])) {
        rej <- x[["names"]][x[["rej"]]]
        cli::cli_text(
          "{.strong Hypotheses rejected:} {rej}"
        )
      } else {
        cli::cli_text("No hypotheses were rejected.")
      }
    } else {
      cli::cli_h2("No final test performed")
    }
  })
}

#' @export
format.multiarm_cer_design <- function(x, ...) {
  hooks <- list(
    after_initial_spec = function(x, ...) {
      if (!is.null(x[["controls"]])) {
        cli::cli_text(
          "There are {x[[\"controls\"]]} control groups for a total of {attr(x, \"k\")} hypotheses."
        )
      }
    }
  )
  format_design_common(x, header_label = "Multi-arm", hooks = hooks, ...)
}

#' @export
print.multiarm_cer_design <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.cer_design <- function(x, ...) {
  format_design_common(x, header_label = "CER", ...)
}

#' @export
print.cer_design <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.adagraph_design <- function(x, ...) {
  format_design_common(x, header_label = "Adagraph", ...)
}

#' @export
print.adagraph_design <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.trial_design <- function(x, ...) {
  hooks <- list(
    after_initial_spec = function(x, ...) {
      arms <- x[["arms"]]
      endpoints <- x[["endpoints"]]
      subgroups <- x[["subgroups"]]
      if (arms > 1) {
        names_arms_str <- cli::format_inline(" ({x[[\"names_arms\"]]})")
      } else {
        names_arms_str <- ""
      }
      if (endpoints > 1) {
        names_ep_str <- cli::format_inline(" ({x[[\"names_endpoints\"]]})")
      } else {
        names_ep_str <- ""
      }
      if (subgroups > 0) {
        names_sg_str <- cli::format_inline(" ({x[[\"names_subgroups\"]]})")
      } else {
        names_sg_str <- ""
      }
      cli::cli_text(
        "There are {arms} arm{?s}{names_arms_str}, {endpoints} endpoint{?s}{names_ep_str} and {cli::no(subgroups)} subgroup{?s}{names_sg_str}."
      )
      cli::cli_text("The first stage sample size per arm/group is:")
      cli_print(x[["n_table"]], row.names = FALSE, ...)
    },
    after_adaptations = function(x, ...) {
      if (!is.null(x[["ad_n_table"]])) {
        cli::cli_text("The second stage sample size per arm/group is:")
        cli_print(x[["ad_n_table"]], row.names = FALSE, ...)
      }
    }
  )

  format_design_common(x, header_label = "Trial", hooks = hooks, ...)
}

#' @export
print.trial_design <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
