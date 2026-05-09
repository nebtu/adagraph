#' Resolve hypothesis names from available sources
#'
#' Determines hypothesis names using the following priority:
#' 1. Explicit `names` parameter if provided
#' 2. Names from the `weights` vector if available
#' 3. Auto-generated names: H1, H2, ...
#'
#' @param names Explicit names (character vector or NULL)
#' @param weights Weights vector (may have names attribute)
#' @param k Number of hypotheses
#'
#' @return Character vector of hypothesis names
#' @noRd
resolve_hypothesis_names <- function(names, weights, k) {
  if (!is.null(names)) {
    return(names)
  }
  if (!is.null(names(weights))) {
    return(names(weights))
  }
  paste0("H", as.character(1:k))
}

#' Standardize a named vector to match canonical hypothesis order
#'
#' If the vector is named, reorder it to match `expected_names`.
#' If unnamed, check that its length matches and return as-is.
#'
#' @param x A vector (numeric, logical, etc.)
#' @param expected_names Character vector of canonical hypothesis names
#' @param arg_name Name of the argument, for error messages
#' @param allow_scalar If TRUE, a scalar value passes through without name checking
#' @param call Caller environment for error reporting
#'
#' @return The vector, potentially reordered
#' @noRd
standardize_named_vector <- function(
  x,
  expected_names,
  arg_name,
  allow_scalar = FALSE,
  call = rlang::caller_env()
) {
  k <- length(expected_names)

  if (allow_scalar && length(x) == 1 && is.null(names(x))) {
    return(x)
  }

  if (is.null(names(x))) {
    if (length(x) != k) {
      cli::cli_abort(
        c(
          "{.arg {arg_name}} must have length {k} (one per hypothesis).",
          "x" = "{.arg {arg_name}} has length {length(x)}.",
          "i" = "Hypotheses are: {.val {expected_names}}."
        ),
        class = "adagraph_standardize_length",
        call = call
      )
    }
    return(x)
  }

  # Named vector: validate names match expected set
  x_names <- names(x)

  if (anyDuplicated(x_names)) {
    cli::cli_abort(
      c(
        "{.arg {arg_name}} has duplicate names.",
        "x" = "Duplicated: {.val {x_names[duplicated(x_names)]}}."
      ),
      class = "adagraph_standardize_names",
      call = call
    )
  }

  if (any(x_names == "")) {
    cli::cli_abort(
      c(
        "{.arg {arg_name}} has a mix of named and unnamed elements.",
        "i" = "Either name all elements or none."
      ),
      class = "adagraph_standardize_names",
      call = call
    )
  }

  missing_names <- setdiff(expected_names, x_names)
  extra_names <- setdiff(x_names, expected_names)

  if (length(missing_names) > 0 || length(extra_names) > 0) {
    msg <- "{.arg {arg_name}} names do not match hypothesis names."
    details <- character()
    if (length(extra_names) > 0) {
      details <- c(details, "x" = "Unexpected: {.val {extra_names}}.")
    }
    if (length(missing_names) > 0) {
      details <- c(details, "x" = "Missing: {.val {missing_names}}.")
    }
    details <- c(details, "i" = "Expected: {.val {expected_names}}.")
    cli::cli_abort(
      c(msg, details),
      class = "adagraph_standardize_names",
      call = call
    )
  }

  x[expected_names]
}

#' Standardize a named matrix to match canonical hypothesis order
#'
#' If the matrix has row/column names, reorder both dimensions to match
#' `expected_names`. If unnamed, check dimensions and return as-is.
#'
#' @param m A matrix
#' @param expected_names Character vector of canonical hypothesis names
#' @param arg_name Name of the argument, for error messages
#' @param call Caller environment for error reporting
#'
#' @return The matrix, potentially reordered
#' @noRd
standardize_named_matrix <- function(
  m,
  expected_names,
  arg_name,
  call = rlang::caller_env()
) {
  if (!is.matrix(m)) {
    return(m) # let downstream validator catch non-matrix
  }

  k <- length(expected_names)
  m_names <- rownames(m) %||% colnames(m)

  if (is.null(m_names)) {
    if (nrow(m) != k || ncol(m) != k) {
      cli::cli_abort(
        c(
          "{.arg {arg_name}} must be a {k} x {k} matrix (one row/column per hypothesis).",
          "x" = "{.arg {arg_name}} has dimensions {nrow(m)} x {ncol(m)}.",
          "i" = "Hypotheses are: {.val {expected_names}}."
        ),
        class = "adagraph_standardize_length",
        call = call
      )
    }
    return(m)
  }

  # Named matrix: validate names match expected set
  if (anyDuplicated(m_names)) {
    cli::cli_abort(
      c(
        "{.arg {arg_name}} has duplicate row/column names.",
        "x" = "Duplicated: {.val {m_names[duplicated(m_names)]}}."
      ),
      class = "adagraph_standardize_names",
      call = call
    )
  }

  if (any(m_names == "")) {
    cli::cli_abort(
      c(
        "{.arg {arg_name}} has a mix of named and unnamed rows/columns.",
        "i" = "Either name all rows/columns or none."
      ),
      class = "adagraph_standardize_names",
      call = call
    )
  }

  missing_names <- setdiff(expected_names, m_names)
  extra_names <- setdiff(m_names, expected_names)

  if (length(missing_names) > 0 || length(extra_names) > 0) {
    msg <- "{.arg {arg_name}} row/column names do not match hypothesis names."
    details <- character()
    if (length(extra_names) > 0) {
      details <- c(details, "x" = "Unexpected: {.val {extra_names}}.")
    }
    if (length(missing_names) > 0) {
      details <- c(details, "x" = "Missing: {.val {missing_names}}.")
    }
    details <- c(details, "i" = "Expected: {.val {expected_names}}.")
    cli::cli_abort(
      c(msg, details),
      class = "adagraph_standardize_names",
      call = call
    )
  }

  # Ensure both dimensions are named before reordering
  rownames(m) <- m_names
  colnames(m) <- m_names
  m[expected_names, expected_names]
}
