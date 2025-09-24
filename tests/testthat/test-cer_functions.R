test_that("get_cer works", {
  # examples from documentation
  expect_gte(
    get_cer(
      c(0.01, 0.01, 0.9, 0.9),
      c(1, 1, 0, 0),
      0.05,
      matrix(rep(NA, 16), nrow = 4),
      0.5
    ),
    1
  )
  expect_equal(
    get_cer(
      c(0.01, 0.01, 0.9, 0.9),
      c(0, 0, 1, 1),
      0.05,
      matrix(rep(NA, 16), nrow = 4),
      0.5
    ),
    0.000308892594752086
  )

  expect_equal(
    get_cer(
      c(0.5, 0.5),
      c(1, 1),
      0.05,
      matrix(rep(NA, 4), nrow = 2),
      0.5
    ),
    0.020009254
  )

  expect_equal(
    get_cer(
      0.5,
      1,
      0.5,
      matrix(NA),
      0.5
    ),
    0.5
  )

  #check versus root-finding algorithm
  p_comb <- function(p1, p2, t) {
    1 - pnorm(sqrt(t) * qnorm(1 - p1) + sqrt(1 - t) * qnorm(1 - p2))
  }
  expect_equal(
    get_cer(
      0.1,
      1,
      0.1,
      matrix(NA),
      0.5
    ),
    uniroot(\(x) p_comb(0.1, x, 0.5) - 0.1, c(0, 1), tol = 1e-32)$root
  )
  expect_equal(
    get_cer(
      c(0.1, 0.1),
      c(1, 1),
      0.1,
      matrix(rep(NA, 4), nrow = 2),
      0.5
    ),
    2 * uniroot(\(x) p_comb(0.1, x, 0.5) - 0.1, c(0, 1), tol = 1e-32)$root
  )
})

test_that("cer_prep_bounds works", {
  expect_equal(
    cer_prep_bounds(
      correlation = rbind(c(1, 0), c(0, 1)),
      weights = c(0.5, 0.5),
      alpha = c(0.001525323, 0.025),
      t = 0.5
    ),
    list(
      bounds_1 = c(
        0.000762952950,
        0.000762952950
      ),
      bounds_2 = c(
        0.012272899,
        0.012272899
      ),
      cJ1 = 0.00152590590,
      cJ2 = 0.02454579800
    )
  )

  expect_equal(
    cer_prep_bounds(
      correlation = rbind(c(1, 0.5), c(0.5, 1)),
      weights = c(2 / 3, 1 / 3),
      alpha = c(0.001525323, 0.025),
      t = 0.5
    ),
    list(
      bounds_1 = c(
        0.0010404644153,
        0.0005202322077
      ),
      bounds_2 = c(
        0.01743004176,
        0.00871502088
      ),
      cJ1 = 0.00156069662,
      cJ2 = 0.02614506264
    )
  )
})
