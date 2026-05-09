# Unit tests for standardize_named_vector ====

test_that("standardize_named_vector passes unnamed vectors of correct length", {
  result <- standardize_named_vector(c(0.5, 0.5), c("H1", "H2"), "weights")
  expect_equal(result, c(0.5, 0.5))
})

test_that("standardize_named_vector reorders named vectors", {
  result <- standardize_named_vector(
    c(H2 = 0.3, H1 = 0.7), c("H1", "H2"), "weights"
  )
  expect_equal(result, c(H1 = 0.7, H2 = 0.3))
})

test_that("standardize_named_vector passes correctly ordered named vectors", {
  result <- standardize_named_vector(
    c(H1 = 0.7, H2 = 0.3), c("H1", "H2"), "weights"
  )
  expect_equal(result, c(H1 = 0.7, H2 = 0.3))
})

test_that("standardize_named_vector errors on wrong length unnamed vector", {
  expect_error(
    standardize_named_vector(c(0.5, 0.3, 0.2), c("H1", "H2"), "weights"),
    class = "adagraph_standardize_length"
  )
})

test_that("standardize_named_vector errors on mismatched names", {
  expect_error(
    standardize_named_vector(c(H1 = 0.5, H3 = 0.5), c("H1", "H2"), "weights"),
    class = "adagraph_standardize_names"
  )
})

test_that("standardize_named_vector errors on duplicate names", {
  expect_error(
    standardize_named_vector(c(H1 = 0.5, H1 = 0.5), c("H1", "H2"), "weights"),
    class = "adagraph_standardize_names"
  )
})

test_that("standardize_named_vector errors on partial naming", {
  expect_error(
    standardize_named_vector(c(H1 = 0.5, 0.5), c("H1", "H2"), "weights"),
    class = "adagraph_standardize_names"
  )
})

test_that("standardize_named_vector allow_scalar passes scalar through", {
  result <- standardize_named_vector(0.5, c("H1", "H2"), "t", allow_scalar = TRUE)
  expect_equal(result, 0.5)
})

test_that("standardize_named_vector allow_scalar still reorders vectors", {
  result <- standardize_named_vector(
    c(H2 = 0.6, H1 = 0.4), c("H1", "H2"), "t", allow_scalar = TRUE
  )
  expect_equal(result, c(H1 = 0.4, H2 = 0.6))
})

test_that("standardize_named_vector works with logical vectors", {
  result <- standardize_named_vector(
    c(H2 = FALSE, H1 = TRUE), c("H1", "H2"), "hypotheses"
  )
  expect_equal(result, c(H1 = TRUE, H2 = FALSE))
})

# Unit tests for standardize_named_matrix ====

test_that("standardize_named_matrix passes unnamed matrix of correct size", {
  m <- matrix(c(0, 1, 1, 0), nrow = 2)
  result <- standardize_named_matrix(m, c("H1", "H2"), "test_m")
  expect_equal(result, m)
})

test_that("standardize_named_matrix reorders named matrix", {
  m <- rbind(H2 = c(0, 1), H1 = c(1, 0))
  colnames(m) <- c("H2", "H1")
  result <- standardize_named_matrix(m, c("H1", "H2"), "test_m")
  expected <- rbind(H1 = c(0, 1), H2 = c(1, 0))
  colnames(expected) <- c("H1", "H2")
  expect_equal(result, expected)
})

test_that("standardize_named_matrix uses rownames when colnames absent", {
  m <- rbind(H2 = c(0, 1), H1 = c(1, 0))
  # rownames are set, colnames are not
  result <- standardize_named_matrix(m, c("H1", "H2"), "test_m")
  expect_equal(rownames(result), c("H1", "H2"))
})

test_that("standardize_named_matrix errors on wrong dimensions", {
  m <- matrix(0, nrow = 3, ncol = 3)
  expect_error(
    standardize_named_matrix(m, c("H1", "H2"), "test_m"),
    class = "adagraph_standardize_length"
  )
})

test_that("standardize_named_matrix errors on mismatched names", {
  m <- rbind(H1 = c(0, 1), H3 = c(1, 0))
  expect_error(
    standardize_named_matrix(m, c("H1", "H2"), "test_m"),
    class = "adagraph_standardize_names"
  )
})

test_that("standardize_named_matrix returns non-matrix as-is", {
  result <- standardize_named_matrix("not a matrix", c("H1", "H2"), "test_m")
  expect_equal(result, "not a matrix")
})

# Unit tests for resolve_hypothesis_names ====

test_that("resolve_hypothesis_names uses explicit names", {
  result <- resolve_hypothesis_names(c("A", "B"), c(x = 0.5, y = 0.5), 2)
  expect_equal(result, c("A", "B"))
})

test_that("resolve_hypothesis_names uses weights names", {
  result <- resolve_hypothesis_names(NULL, c(x = 0.5, y = 0.5), 2)
  expect_equal(result, c("x", "y"))
})

test_that("resolve_hypothesis_names generates default names", {
  result <- resolve_hypothesis_names(NULL, c(0.5, 0.5), 2)
  expect_equal(result, c("H1", "H2"))
})

# Integration tests: named inputs in design creation ====

test_that("cer_design accepts shuffled named weights and test_m", {
  as <- function(x, t) 2 - 2 * pnorm(qnorm(1 - x / 2) / sqrt(t))
  corr <- rbind(c(1, NA), c(NA, 1))

  # Correct order
  design1 <- cer_design(
    weights = c(2 / 3, 1 / 3),
    test_m = rbind(c(0, 1), c(1, 0)),
    correlation = corr,
    alpha = 0.05,
    alpha_spending_f = as,
    t = 0.5,
    names = c("H1", "H2")
  )

  # Shuffled named inputs with explicit names to force reordering
  design2 <- cer_design(
    weights = c(H2 = 1 / 3, H1 = 2 / 3),
    test_m = rbind(H2 = c(0, 1), H1 = c(1, 0)),
    correlation = rbind(H2 = c(1, NA), H1 = c(NA, 1)),
    alpha = 0.05,
    alpha_spending_f = as,
    t = 0.5,
    names = c("H1", "H2")
  )

  expect_equal(design1[["weights"]], design2[["weights"]])
  expect_equal(design1[["test_m"]], design2[["test_m"]])
  expect_equal(design1[["bounds_1"]], design2[["bounds_1"]])
  expect_equal(design1[["bounds_2"]], design2[["bounds_2"]])
})

# Integration tests: named p_values in cer_interim_test ====

test_that("cer_interim_test reorders named p_values", {
  design <- make_example_design()

  # Correct order
  design1 <- cer_interim_test(design, c(0.001, 0.02, 0.5, 0.5))

  # Shuffled named p_values
  design2 <- cer_interim_test(
    design,
    c(H3 = 0.5, H1 = 0.001, H4 = 0.5, H2 = 0.02)
  )

  expect_equal(design1[["rej_interim"]], design2[["rej_interim"]])
  expect_equal(design1[["cer_vec"]], design2[["cer_vec"]])
})

# Integration tests: named p_values in cer_final_test ====

test_that("cer_final_test reorders named p_values", {
  design <- make_example_design()
  design <- cer_interim_test(design, c(0.001, 0.02, 0.5, 0.5))
  design <- cer_drop_hypotheses(design, 1)

  # Correct order
  design1 <- cer_final_test(design, c(NA, 0.01, 0.03, 0.04))

  # Shuffled
  design2 <- cer_final_test(
    design,
    c(H4 = 0.04, H2 = 0.01, H1 = NA, H3 = 0.03)
  )

  expect_equal(design1[["rej"]], design2[["rej"]])
})

# Integration tests: named inputs in cer_adapt ====

test_that("cer_adapt reorders named weights and test_m", {
  design <- make_example_design()
  design <- cer_interim_test(design, c(0.001, 0.02, 0.5, 0.5))

  # Correct order
  design1 <- cer_adapt(
    design,
    weights = c(0, 1 / 2, 1 / 4, 1 / 4)
  )

  # Shuffled
  design2 <- cer_adapt(
    design,
    weights = c(H3 = 1 / 4, H1 = 0, H4 = 1 / 4, H2 = 1 / 2)
  )

  expect_equal(design1[["ad_weights"]], design2[["ad_weights"]])
  expect_equal(design1[["ad_weights_matrix"]], design2[["ad_weights_matrix"]])
})

test_that("cer_adapt handles scalar t with allow_scalar", {
  design <- make_example_design()
  design <- cer_interim_test(design, c(0.001, 0.02, 0.5, 0.5))

  # Scalar t should pass through without error
  design_adapted <- cer_adapt(design, t = 0.6)
  expect_equal(design_adapted[["ad_t"]], 0.6)
})

test_that("cer_adapt handles named vector t", {
  design <- make_example_design()
  design <- cer_interim_test(design, c(0.001, 0.02, 0.5, 0.5))

  # Named vector t in shuffled order
  design1 <- cer_adapt(design, t = c(0.4, 0.5, 0.6, 0.7))
  design2 <- cer_adapt(
    design,
    t = c(H3 = 0.6, H1 = 0.4, H4 = 0.7, H2 = 0.5)
  )

  expect_equal(unname(design1[["ad_t"]]), unname(design2[["ad_t"]]))
})

# Integration tests: named hypotheses in cer_alt_drop_hypotheses ====

test_that("cer_alt_drop_hypotheses reorders named logical vector", {
  design <- make_example_design()
  design <- cer_interim_test(design, c(0.001, 0.02, 0.5, 0.5))

  # Correct order
  design1 <- cer_alt_drop_hypotheses(design, c(TRUE, FALSE, FALSE, FALSE))

  # Shuffled
  design2 <- cer_alt_drop_hypotheses(
    design,
    c(H3 = FALSE, H1 = TRUE, H4 = FALSE, H2 = FALSE)
  )

  expect_equal(design1[["ad_weights"]], design2[["ad_weights"]])
  expect_equal(design1[["ad_weights_matrix"]], design2[["ad_weights_matrix"]])
})

# Error message quality ====

test_that("standardize errors include hypothesis names in message", {
  expect_error(
    standardize_named_vector(c(H1 = 0.5, H3 = 0.5), c("H1", "H2"), "weights"),
    "H3" # extra name should appear
  )
  expect_error(
    standardize_named_vector(c(H1 = 0.5, H3 = 0.5), c("H1", "H2"), "weights"),
    "H2" # missing name should appear
  )
})
