#' Get information about the intersection hypothesis of a design
#'
#' For given design, this returns for each intersection hypothesis the weights
#' and boundaries for rejecting. After adaptations, it also includes
#' the updated weights and boundaries.
#'
#' @param design A cer_design object
#' @param ... additional parameters, not used
#'
#' @return A dataframe of class `intersection_hypotheses`, where each row
#' corresponds to an intersection hypothesis. This class has a custom [print()]
#' function, but works like a data frame in any other aspect. Use
#' [as.data.frame()] to see the dataframe structure directly.
#'
#' @export
intersection_hypotheses <- function(design, ...) {
  UseMethod("intersection_hypotheses")
}

#' @rdname intersection_hypotheses
#' @export
intersection_hypotheses.cer_design <- function(design, ...) {
  active_hyp <- data.frame(design[["hyp_matrix"]] == 1)
  hyp_names <- apply(active_hyp, 1, \(x) {
    paste(names(x[x]), collapse = "\u2229")
  })
  weights <- data.frame(design[["weights_matrix"]])
  weights[!active_hyp] <- NA
  colnames(weights) <- paste0("weights_", colnames(weights))
  bounds_1 <- data.frame(design[["bounds_1"]])
  bounds_1[!active_hyp] <- NA
  colnames(bounds_1) <- paste0("bound_interim_", colnames(bounds_1))
  bounds_2 <- data.frame(design[["bounds_2"]])
  bounds_2[!active_hyp] <- NA
  colnames(bounds_2) <- paste0("bound_final_", colnames(bounds_2))
  df <- data.frame(hyp_names, active_hyp, weights, bounds_1, bounds_2)
  if (isTRUE(design[["interim_test"]])) {
    interim_rej <- data.frame(design[["intersection_rej_interim"]])
    colnames(interim_rej) <- "interim_rej"
    df <- cbind(df, interim_rej)
  }
  if (isTRUE(design[["adaptations"]])) {
    ad_weights <- data.frame(design[["ad_weights_matrix"]])
    ad_weights[!active_hyp] <- NA
    colnames(ad_weights) <- paste0("ad_weights_", colnames(ad_weights))
    ad_bounds_2 <- data.frame(design[["ad_bounds_2"]])
    ad_bounds_2[!active_hyp] <- NA
    colnames(ad_bounds_2) <- paste0("ad_bound_final_", colnames(ad_bounds_2))

    df <- cbind(df, ad_weights, ad_bounds_2)
  }
  if (isTRUE(design[["final_test"]])) {
    final_rej <- data.frame(design[["intersection_rej"]] == 1)
    colnames(final_rej) <- "rej"
    df <- cbind(df, final_rej)
  }

  #order by number of hypothesis (decreasing) and then by name
  n_active_hyp <- apply(active_hyp, 1, sum)
  df <- df[order(-n_active_hyp, hyp_names), ]

  class(df) <- c("intersection_hypotheses", "data.frame")
  df
}

#' @export
print.intersection_hypotheses <- function(
  x,
  digits = getOption("digits"),
  ...
) {
  format_nums <- function(num) {
    paste(
      vapply(num[!is.na(num)], format, character(1), digits = 3, trim = TRUE),
      collapse = ", "
    )
  }
  weights <- apply(x[, grep("^weights", names(x))], 1, \(row) {
    format_nums(row)
  })
  bounds_1 <- apply(x[, grep("^bound_interim", names(x))], 1, \(row) {
    format_nums(row)
  })
  bounds_2 <- apply(x[, grep("^bound_final", names(x))], 1, \(row) {
    format_nums(row)
  })
  df <- data.frame(
    `Hypotheses` = x[["hyp_names"]],
    `Weights` = weights,
    `Interim Bounds` = bounds_1,
    `Final Bounds` = bounds_2,
    check.names = FALSE
  )

  if (any(grep("^interim_rej", names(x)))) {
    df <- cbind(df, `Rejected at Interim` = x[["interim_rej"]])
    cli::cat_line("Initial specification and interim result")
  } else {
    cli::cat_line("Initial specification")
  }

  print.data.frame(df, row.names = FALSE, ...)
  cli::cat_line("")

  if (any(grep("^ad", names(x)))) {
    ad_weights <- apply(x[, grep("^ad_weights", names(x))], 1, \(row) {
      format_nums(row)
    })
    ad_bounds_2 <- apply(x[, grep("^ad_bound_final", names(x))], 1, \(row) {
      format_nums(row)
    })
    ad_df <- data.frame(
      `Hypotheses` = x[["hyp_names"]],
      `Adapted Weights` = ad_weights,
      `Adapted Final Bounds` = ad_bounds_2,
      check.names = FALSE
    )
    if (any(grep("^rej", names(x)))) {
      print(x[["rej"]])
      ad_df <- cbind(ad_df, `Rejected` = x[["rej"]])
      cli::cat_line("After adaptation and final result")
    } else {
      cli::cat_line("After adaptation")
    }

    print.data.frame(ad_df, row.names = FALSE, ...)
  }

  invisible(x)
}

#' @export
as.data.frame.intersection_hypotheses <- function(x, ...) {
  class(x) <- "data.frame"
  x
}
