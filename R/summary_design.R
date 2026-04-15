#' @export
summary.adagraph_design <- function(object, ...) {
  summary_list <- list(
    alpha = object[["alpha"]],
    weights = object[["weights"]],
    test_m = object[["test_m"]],
    correlation = object[["correlation"]],
    t = object[["t"]],
    interim_test = object[["interim_test"]],
    adaptions = object[["adaptions"]],
    final_test = object[["final_test"]],
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

  if (isTRUE(object[["adaptions"]])) {
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

  class(summary_list) <- "summary.cer_design"

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

  if (isTRUE(object[["adaptions"]])) {
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

#' Shared internal helper used by S3 summary print methods
#'
#' @param x design to be printed
#' @param header_label label describing the type of design being printed
#' @param hooks list containing functions for printing additional information, should
#'   have names out of c("after_initial_spec", "after_adaptions"). Each function
#'   should only take x as argument
#'
#' @noRd
print_design_summary <- function(x, header_label, hooks = list()) {
  cli::cat_line(
    "A ",
    header_label,
    " Design object, for testing ",
    x[["k"]],
    " hypotheses at FWER ",
    x[["alpha"]],
    "."
  )
  cli::cat_line()

  cli::cat_rule("Inital design specification")
  cli::cat_line()

  cli::cat_line("Hypotheses weights")
  cli::cat_print(x[["weights"]])
  cli::cat_line()

  cli::cat_line("Graph Transition Matrix")
  cli::cat_print(x[["test_m"]])
  cli::cat_line()

  if (!all(is.na(x[["correlation"]]))) {
    cli::cat_line("Correlation for parametric test")
    cli::cat_print(x[["correlation"]])
    cli::cat_line()
  }

  # Allow class-specific additions inside the initial spec section
  if (is.function(hooks$after_initial_spec)) {
    hooks$after_initial_spec(x)
  }

  # Planned interim information fraction if available
  if (!is.null(x[["t"]])) {
    cli::cat_line("Interim test is planned at time fraction ", x[["t"]])
    cli::cat_line()
  }

  # Interim results (only if flagged and fields available)
  if (isTRUE(x[["interim_test"]])) {
    cli::cat_rule("Interim test result")
    cli::cat_line("")

    cli::cat_line("P-values of interim test are:")
    cli::cat_print(x[["p_values_interim"]])
    if (any(x[["rej_interim"]])) {
      cli::cat_line(
        "Hypotheses rejected at the interim: ",
        which(x[["rej_interim"]])
      )
    } else {
      cli::cat_line("No Hypotheses were rejected at the interim")
    }
    cli::cat_line("")
  }

  # Adaptations section (print only available changes)
  if (isTRUE(x[["adaptions"]])) {
    cli::cat_rule("Adaptions from inital specification")
    cli::cat_line()
    if (
      !is.null(x[["ad_weights"]]) &&
        !identical(x[["ad_weights"]], x[["weights"]])
    ) {
      cli::cat_line("New hypotheses weights")
      cli::cat_print(x[["ad_weights"]])
      cli::cat_line()
    }
    if (
      !is.null(x[["ad_test_m"]]) && !identical(x[["ad_test_m"]], x[["test_m"]])
    ) {
      cli::cat_line("New graph Transition Matrix")
      cli::cat_print(x[["ad_test_m"]])
      cli::cat_line()
    }
    if (
      !is.null(x[["ad_correlation"]]) &&
        !identical(x[["ad_correlation"]], x[["correlation"]])
    ) {
      cli::cat_line("New correlation for parametric test")
      cli::cat_print(x[["ad_correlation"]])
      cli::cat_line()
    }
    if (!is.null(x[["ad_t"]]) && !identical(x[["ad_t"]], x[["t"]])) {
      cli::cat_line("New time fractions for the hypotheses")
      cli::cat_print(x[["ad_t"]])
      cli::cat_line()
    }
  }

  if (is.function(hooks$after_adaptions)) {
    hooks$after_adaptions(x)
  }

  # Final results if available
  if (isTRUE(x[["final_test"]])) {
    cli::cat_rule("Final test result")
    cli::cat_line()

    cli::cat_line("Overall p-values of the hypotheses are:")
    cli::cat_print(x[["p_values_final"]])
    if (any(x[["rej"]])) {
      cli::cat_line("Hypotheses rejected: ")
      cli::cat_print(which(x[["rej"]]))
    } else {
      cli::cat_line("No Hypotheses were rejected")
    }
  }

  invisible(x)
}

#' @export
print.summary.adagraph_design <- function(x, ...) {
  print_design_summary(x, header_label = "Adagraph")
}

#' @export
print.summary.cer_design <- function(x, ...) {
  print_design_summary(x, header_label = "CER")
}

#' @export
print.summary.multiarm_cer_design <- function(x, ...) {
  hooks <- list(
    after_initial_spec = function(x) {
      # Multi-arm specifics within the initial specification
      if (!is.null(x[["controls"]])) {
        cli::cat_line("Number of control groups:")
        cli::cat_print(x[["controls"]])
        cli::cat_line()
      }
      if (!is.null(x[["treatment_assoc"]])) {
        cli::cat_line("Treatment-to-control assignments (per treatment arm):")
        cli::cat_print(x[["treatment_assoc"]])
        cli::cat_line()
      }
      if (!is.null(x[["n_controls"]])) {
        cli::cat_line("Planned sample sizes per control group:")
        cli::cat_print(x[["n_controls"]])
        cli::cat_line()
      }
      if (!is.null(x[["n_treatments"]])) {
        cli::cat_line("Planned sample sizes per treatment group:")
        cli::cat_print(x[["n_treatments"]])
        cli::cat_line()
      }
    },
    after_adaptions = function(x) {
      # Show multi-arm sample size details introduced by adaptations, if available
      if (!is.null(x[["n_cont_2"]])) {
        cli::cat_line("Second-stage sample sizes (controls):")
        cli::cat_print(x[["n_cont_2"]])
        cli::cat_line()
      }
      if (!is.null(x[["n_treat_2"]])) {
        cli::cat_line("Second-stage sample sizes (treatments):")
        cli::cat_print(x[["n_treat_2"]])
        cli::cat_line()
      }
      if (!is.null(x[["ad_n_controls"]])) {
        cli::cat_line("Total planned sample sizes after adaptation (controls):")
        cli::cat_print(x[["ad_n_controls"]])
        cli::cat_line()
      }
      if (!is.null(x[["ad_n_treatments"]])) {
        cli::cat_line(
          "Total planned sample sizes after adaptation (treatments):"
        )
        cli::cat_print(x[["ad_n_treatments"]])
        cli::cat_line()
      }
    }
  )
  print_design_summary(x, header_label = "Multi-arm", hooks = hooks)
}
