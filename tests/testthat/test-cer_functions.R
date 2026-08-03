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

test_that("cer is correctly calculated", {
  #calculate cer using a manual MC approach
  mc_cer <- function(p, bounds, corr, t, n = 1e6) {
    k <- length(p)
    z1 <- qnorm(p, lower.tail = FALSE)
    crit <- qnorm(pmin(bounds, 1), lower.tail = FALSE)
    if (length(t) == 1) {
      t <- rep(t, k)
    }
    z2 <- mvtnorm::rmvnorm(n, mean = c(0, 0), sigma = corr)
    p <- 1 - pnorm(sqrt(t) * z1 + sqrt(1 - t) * z2)
    mean(matrixStats::colAnys(t(p) < bounds))
  }

  withr::with_seed(1, {
    correlation <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)
    for (i in 1:10 / 10) {
      bounds <- c(i, 1 - i) * 0.025
      for (p in 1:10 / 50) {
        cer <- .get_cer(
          c(p, p),
          bounds,
          correlation = correlation,
          t = c(0.5, 0.5),
          conn = list(c(1, 2))
        )
        cer_emp <- mc_cer(c(p, p), bounds, correlation, 0.5)
        expect_equal(cer_emp, cer, tolerance = 1e4)
      }
    }
  })
})
