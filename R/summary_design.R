#' @export
summary.adagraph_design <- function(object, ...) {
  summary_list <- list(
    alpha = object[["alpha"]],
    weights = object[["weights"]],
    test_m = object[["test_m"]],
    correlation = object[["correlation"]],
    t = object[["t"]],
    interim_test = object[["interim_test"]],
    adaptations = object[["adaptations"]],
    final_test = object[["final_test"]],
    names = object[["names"]],
    k = attr(object, "k")
  )

  if (isTRUE(object[["interim_test"]])) {
    summary_list <- c(
      summary_list,
      list(
        p_values_interim = object[["p_values_interim"]],
        rej_interim = object[["rej_interim"]]
      )
    )
  }

  if (isTRUE(object[["adaptations"]])) {
    summary_list <- c(
      summary_list,
      list(
        ad_weights = object[["ad_weights"]],
        ad_test_m = object[["ad_test_m"]],
        ad_correlation = object[["ad_correlation"]],
        ad_t = object[["ad_t"]]
      )
    )
  }

  if (isTRUE(object[["final_test"]])) {
    summary_list <- c(
      summary_list,
      list(
        p_values_final = object[["p_values_final"]],
        rej = object[["rej"]]
      )
    )
  }

  class(summary_list) <- "summary.adagraph_design"

  summary_list
}

#' @export
summary.cer_design <- function(object, ...) {
  summary_list <- summary.adagraph_design(object)

  class(summary_list) <- c("summary.cer_design", class(summary_list))

  summary_list
}

#' @export
summary.multiarm_cer_design <- function(object, ...) {
  summary_list <- summary.cer_design(object)

  summary_list <- c(
    summary_list,
    list(
      controls = object[["controls"]],
      treatment_assoc = object[["treatment_assoc"]],
      n_controls = object[["n_controls"]],
      n_treatments = object[["n_treatments"]]
    )
  )

  if (isTRUE(object[["adaptations"]])) {
    summary_list <- c(
      summary_list,
      list(
        n_cont_2 = object[["n_cont_2"]],
        n_treat_2 = object[["n_treat_2"]],
        ad_n_controls = object[["ad_n_controls"]],
        ad_n_treatments = object[["ad_n_treatments"]]
      )
    )
  }

  class(summary_list) <- c("summary.multiarm_cer_design", class(summary_list))

  summary_list
}

#' @export
summary.trial_design <- function(object, ...) {
  summary_list <- summary.cer_design(object)

  hyp_assoc <- cbind(hypothesis = object[["names"]], object[["hyp_assoc"]])
  summary_list <- c(
    summary_list,
    list(
      arms = object[["arms"]],
      endpoints = object[["endpoints"]],
      subgroups = object[["subgroups"]],
      names_arms = object[["names_arms"]],
      names_endpoints = object[["names_endpoints"]],
      names_subgroups = object[["names_subgroups"]],
      n_table = object[["n_table"]],
      hyp_assoc = hyp_assoc
    )
  )

  if (isTRUE(object[["adaptations"]])) {
    summary_list <- c(
      summary_list,
      list(
        ad_n_table = object[["ad_n_table"]]
      )
    )
  }

  class(summary_list) <- c("summary.trial_design", class(summary_list))

  summary_list
}

#' Shared internal helper used by S3 summary format methods
#'
#' @param x summary object to be formatted
#' @param header_label label describing the type of design being formatted
#' @param hooks list containing functions for formatting additional information, should
#'   have names out of c("after_introduction", "after_initial_spec", "after_adaptations").
#'   Each function should take x and ... as arguments and use cli semantic functions.
#' @param ... additional arguments passed to print calls (e.g. digits)
#'
#' @noRd
format_design_summary <- function(x, header_label, hooks = list(), ...) {
  cli::cli_format_method({
    cli::cli_h1("{header_label} Design")
    cli::cli_text(
      "Testing the {x[[\"k\"]]} hypotheses {x[[\"names\"]]} at FWER {x[[\"alpha\"]]}."
    )

    # Allow class-specific additions after the introduction
    if (is.function(hooks[["after_introduction"]])) {
      hooks[["after_introduction"]](x, ...)
    }

    cli::cli_h2("Initial design specification")

    cli::cli_h3("Hypotheses weights")
    cli_print(x[["weights"]], ...)

    cli::cli_h3("Graph Transition Matrix")
    cli_print(x[["test_m"]], ...)

    if (!all(is.na(x[["correlation"]]))) {
      cli::cli_h3("Correlation for parametric test")
      cli_print(x[["correlation"]], ...)
    }

    # Allow class-specific additions inside the initial spec section
    if (is.function(hooks[["after_initial_spec"]])) {
      hooks[["after_initial_spec"]](x, ...)
    }

    # Planned interim information fraction if available
    if (!is.null(x[["t"]])) {
      cli::cli_text("Interim test is planned at time fraction {x[[\"t\"]]}")
    }

    # Interim results (only if flagged and fields available)
    if (isTRUE(x[["interim_test"]])) {
      cli::cli_h2("Interim test result")

      cli::cli_text("P-values of interim test are:")
      cli_print(x[["p_values_interim"]], ...)
      if (any(x[["rej_interim"]])) {
        rej <- x[["names"]][x[["rej_interim"]]]
        cli::cli_text(
          "{.strong Hypotheses rejected at the interim:} {rej}"
        )
      } else {
        cli::cli_text("No hypotheses were rejected at the interim.")
      }
    }

    # Adaptations section (print only available changes)
    if (isTRUE(x[["adaptations"]])) {
      cli::cli_h2("Adaptations from initial specification")

      if (
        !is.null(x[["ad_weights"]]) &&
          !identical(x[["ad_weights"]], x[["weights"]])
      ) {
        cli::cli_h3("New hypotheses weights")
        cli_print(x[["ad_weights"]], ...)
      }
      if (
        !is.null(x[["ad_test_m"]]) &&
          !identical(x[["ad_test_m"]], x[["test_m"]])
      ) {
        cli::cli_h3("New graph Transition Matrix")
        cli_print(x[["ad_test_m"]], ...)
      }
      if (
        !is.null(x[["ad_correlation"]]) &&
          !identical(x[["ad_correlation"]], x[["correlation"]])
      ) {
        cli::cli_h3("New correlation for parametric test")
        cli_print(x[["ad_correlation"]], ...)
      }
      if (!is.null(x[["ad_t"]]) && !identical(x[["ad_t"]], x[["t"]])) {
        cli::cli_h3("New time fractions for the hypotheses")
        cli_print(x[["ad_t"]], ...)
      }
    }

    if (is.function(hooks[["after_adaptations"]])) {
      hooks[["after_adaptations"]](x, ...)
    }

    if (isTRUE(x[["final_test"]])) {
      cli::cli_h2("Final test result")

      cli::cli_text("Overall p-values of the hypotheses are:")
      cli_print(x[["p_values_final"]], ...)
      if (any(x[["rej"]])) {
        rej <- x[["names"]][x[["rej"]]]
        cli::cli_text(
          "{.strong Hypotheses rejected:} {rej}"
        )
      } else {
        cli::cli_text("No hypotheses were rejected.")
      }
    }
  })
}

#' @export
format.summary.adagraph_design <- function(x, ...) {
  format_design_summary(x, header_label = "Adagraph", ...)
}

#' @export
print.summary.adagraph_design <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.summary.cer_design <- function(x, ...) {
  format_design_summary(x, header_label = "CER", ...)
}

#' @export
print.summary.cer_design <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.summary.multiarm_cer_design <- function(x, ...) {
  hooks <- list(
    after_initial_spec = function(x, ...) {
      if (!is.null(x[["controls"]])) {
        cli::cli_text("Number of control groups:")
        cli_print(x[["controls"]], ...)
      }
      if (!is.null(x[["treatment_assoc"]])) {
        cli::cli_text("Treatment-to-control assignments (per treatment arm):")
        cli_print(x[["treatment_assoc"]], ...)
      }
      if (!is.null(x[["n_controls"]])) {
        cli::cli_text("Planned sample sizes per control group:")
        cli_print(x[["n_controls"]], ...)
      }
      if (!is.null(x[["n_treatments"]])) {
        cli::cli_text("Planned sample sizes per treatment group:")
        cli_print(x[["n_treatments"]], ...)
      }
    },
    after_adaptations = function(x, ...) {
      if (!is.null(x[["n_cont_2"]])) {
        cli::cli_text("Second-stage sample sizes (controls):")
        cli_print(x[["n_cont_2"]], ...)
      }
      if (!is.null(x[["n_treat_2"]])) {
        cli::cli_text("Second-stage sample sizes (treatments):")
        cli_print(x[["n_treat_2"]], ...)
      }
      if (!is.null(x[["ad_n_controls"]])) {
        cli::cli_text("Total planned sample sizes after adaptation (controls):")
        cli_print(x[["ad_n_controls"]], ...)
      }
      if (!is.null(x[["ad_n_treatments"]])) {
        cli::cli_text(
          "Total planned sample sizes after adaptation (treatments):"
        )
        cli_print(x[["ad_n_treatments"]], ...)
      }
    }
  )
  format_design_summary(x, header_label = "Multi-arm", hooks = hooks, ...)
}

#' @export
print.summary.multiarm_cer_design <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.summary.trial_design <- function(x, ...) {
  hooks <- list(
    after_introduction = function(x, ...) {
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
    },
    after_initial_spec = function(x, ...) {
      cli::cli_text(
        "Association between hypotheses and arms/endpoints/subgroups:"
      )
      cli_print(x[["hyp_assoc"]], ...)

      cli::cli_text("First stage sample size per arm/group")
      cli_print(x[["n_table"]], row.names = FALSE, ...)
    },
    after_adaptations = function(x, ...) {
      if (!is.null(x[["ad_n_table"]])) {
        cli::cli_text("The second stage sample size per arm/group is:")
        cli_print(x[["ad_n_table"]], row.names = FALSE, ...)
      }
    }
  )
  format_design_summary(x, header_label = "Trial", hooks = hooks, ...)
}

#' @export
print.summary.trial_design <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
