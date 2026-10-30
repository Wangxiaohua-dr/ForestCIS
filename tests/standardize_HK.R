library(ForestCIS)

# Check the agreed scores at the limits, optimal interval, and both ramps.
for (type in c("H", "K")) {
  limits <- switch(type, H = c(0, 1.2, 1.7, 2.7), K = c(0, 0.6, 0.8, 1))
  lower <- limits[1]
  optimum_lower <- limits[2]
  optimum_upper <- limits[3]
  upper <- limits[4]

  observations <- c(
    lower - 1, lower, (lower + optimum_lower) / 2,
    optimum_lower, (optimum_lower + optimum_upper) / 2, optimum_upper,
    optimum_upper + (upper - optimum_upper) / 4,
    optimum_upper + (upper - optimum_upper) / 2,
    optimum_upper + 3 * (upper - optimum_upper) / 4,
    upper, upper + 1
  )
  expected <- c(0.1, 0.1, 0.55, 1, 1, 1, 0.775, 0.55, 0.325, 0.1, 0.1)
  actual <- vapply(observations, standardize_HK, numeric(1), type = type)
  stopifnot(isTRUE(all.equal(actual, expected)))

  # Scores must decrease continuously from the upper optimum to the upper limit.
  descending <- vapply(
    seq(optimum_upper, upper, length.out = 101),
    standardize_HK, numeric(1), type = type
  )
  stopifnot(all(diff(descending) < 0))
  stopifnot(all(descending >= 0.1 & descending <= 1))

  epsilon <- (upper - optimum_upper) * 1e-8
  stopifnot(abs(standardize_HK(optimum_upper + epsilon, type) - 1) < 1e-7)
  stopifnot(abs(standardize_HK(upper - epsilon, type) - 0.1) < 1e-7)
}
