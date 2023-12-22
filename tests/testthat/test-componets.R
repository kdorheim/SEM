# Compare package functions with some of the results returned by the Dietz and Matthes implementation https://github.com/mdietze/PestED
#  The purse of this test it to catch potenital errors that may have occured during the translation process. This test 
# will most likley be deleted after the V1 release.


test_that("arrhenius works", {
  out <- arrhenius(1:10, 1:10)
  expect_true(length(out) == length(1:10))
  # The expected output comes directly from the Dietz & Matthes code base
  expected_out <- c(0.4144242, 0.8624766, 1.3458160, 1.8661554, 2.4252627,
                    3.0249619, 3.6671341,
                    4.3537182, 5.0867121, 5.8681737)
  expect_equal(out, expected_out)
})

test_that("farquhar works", {
  # Randomly select the values to use as inputs, also note that this
  # expected value comes from
  # the Dietz function
  Vcmax <- 18
  Jmax <- Vcmax * 1.67
  Rleaf <- 10
  Gstar <- 10
  alpha <- 0.4
  Ci <- 400
  I <- 10
  Km <- 710.3203
  Fparams <- c(Vcmax, Jmax, Rleaf, Gstar, alpha, Km)
  names(Fparams) <- c("Vcmax", "Jmax", "Rleaf", "Gstar", "alpha", "Km")
  out <- farquhar(Ci, Fparams, I)
  expect_equal(out, -9.085229, tolerance = 1e-5)
})

test_that("ballberry works", {
  g0 <- 0
  m <- 4
  Vcmax <- 18.00
  Jmax <- 30.06
  Rleaf <- 0.72
  Gstar <- 0.50
  alpha <-  0.30
  Ca <- 400
  VPD <- 0.131
  PAR <- 110.900
  Km <- 710.3203
  # This expected value comes from the Dietz implementations of the
  # function and their default values
  expected_out <- 165.0397
  out <- ballberry(input = c(15, 0.1),
                   BBparams = c(g0 = g0, m = m),
                   Fparams = c(Vcmax = Vcmax, 
                               Jmax = Jmax,
                               Rleaf = Rleaf,
                               Gstar = Gstar,
                               alpha = alpha,
                               Km = Km),
                   obs = c(Ca = Ca, 
                           VPD = VPD,
                           PAR = PAR))
  expect_equal(out, expected_out, tolerance = 1e-4)
})

test_that("solve.FVcB works", {
  # This expected value comes from the Dietz implementations of the
  # function and their default values
  expected_output <- c(3.24230114, 0.02875259)
  out <- solve.FVcB(Vcmax = 18, Jmax = 30.06, Rleaf = 1, Gstar = 0.5,
                    alpha = 0.3,
                    m = 4, g0 = 0, VPD = 0.131, PAR = 68.400, Km = 710.3203)
  expect_equal(expected_output, out)
})
