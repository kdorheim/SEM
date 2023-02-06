test_that("SEM works", {
  test_tol <- 1e-7
  ### paramters
  params <- list()
  ## hydrology
  params$gevap <- 0.005  
  params$Wthresh <- 1
  params$Kroot <- 0.2 
  R <- 8.3144621
  Tleaf <- 298
  Kc <- 404.9 * exp(79430 * (Tleaf - 298) / (298 * R * Tleaf))
  Ko <- 278.4 * exp(36380 * (Tleaf - 298) / (298 * R * Tleaf))
  Km <- Kc * (1 + 210 / Ko)
  params$SLA <- 10
  params$alpha <- 0.8
  params$Vcmax <- 18
  params$Jmax  <- params$Vcmax * 1.67
  params$m <- 4
  params$g0 <- 0
  params$allomB0 <- exp(-2.5355) / 0.8  
  params$allomB1 <- 2.4349
  params$allomL0 <- exp(-2.907)
  params$allomL1 <- 1.674
  params$Rleaf <- 0.04 * params$Vcmax
  params$Rroot <- 1.2
  params$Rstem <- 0.05
  params$Rg <- 0.33
  params$Q10 <- 2
  params$Rbasal <- 0.2 / (params$Q10 ^ 2.5) 
  params$leafLitter <- 0.33 / 365 / 86400 * timestep
  params$CWD <- 0.0001 / 365 / 86400 * timestep
  params$rootLitter <- 1.0 / 365 / 86400 * timestep
  params$mort1 <- 1
  params$mort2 <- 5
  params$NSCthreshold <- 0.01
  params$Lmin <- 0.75
  params$q <- 1
  params$StoreMinDay <- 2
  params$Smax <- 1
  params$Rfrac <- 0.2
  params$SeedlingMort <- 0.99
  params$Kleaf <- (1 / 21 / 48) / 2 ^ 2.5  
  params$Km <- Kc * (1 + 210 / Ko)
  DBH <- 10
  X <- rep(1, 7)
  X[1] <- X[3] <- X[4] <- params$allomL0 * DBH ^ params$allomL1
  X[2] <- params$allomB0 * DBH ^ params$allomB1
  X[5] <- 10
  X[7] <- 700
  
  names(X) <- c("leaf", "wood", "root", "storage", "som", "soil_water",
                "stem_density")
  inputs <- data.frame(PAR = 187.2, temp = -6.2, VPD = 0.013, precip = 0)
  
  # Try out the different pest configurations!
  pest0 <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
  out0 <- SEM(X = X, params = params, inputs = inputs, pest = pest0)
  expected_out0 <- c(2.5793242, 26.9563823, 2.5793242, 2.5793242, 10.0000404, 
                     0.9999991, 699.9997472, 35.1959409, 1.0000000, 5.7292199, 
                     1.3706948, 6.9713107)
  expect_equal(out0, expected_out0, tolerance = test_tol)
  pest1 <-c("phloem" = 1, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
  out1 <- SEM(X = X, params = params, inputs = inputs, pest = pest1)
  expected_out1 <- c(2.5792757, 26.9560561, 2.5793242, 2.5792946, 9.9999597,
                     0.9999991, 699.9997308, 35.1959409, 1.0000000, 5.7292199, 
                     1.3706948, 0.0000000)
  expect_equal(out1, expected_out1, tolerance = test_tol)
  
  pest2 <- c("phloem" = 0, "xylem" = 1, "leaf" = 0, "root" = 0, "stem" = 0)
  out2 <- SEM(X = X, params = params, inputs = inputs, pest = pest2)
  expected_out2 <- c(2.5792757, 26.9560561, 2.5793242, 2.5791709, 9.9999597, 
                    0.9999992, 699.9997307, 0.0000000, 0.0000000, 5.7292199, 
                    1.3706948, 0.0000000)
  expect_equal(out2, expected_out2, tolerance = test_tol)
  
  pest3 <- c("phloem" = 0, "xylem" = 0, "leaf" = 1, "root" = 0, "stem" = 0)
  out3 <- SEM(X = X, params = params, inputs = inputs, pest = pest3)
  expected_out3 <- c(2.943282e-04, 2.695606e+01, 2.579324e+00, 2.578903e+00, 
                     9.999959e+00, 9.999992e-01, 6.999997e+02, 0.000000e+00, 
                     1.000000e+00, 0.000000e+00, 1.370695e+00, 4.496680e+00)
  expect_equal(out3, expected_out3, tolerance = test_tol)
  pest4 <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 1, "stem" = 0)
  out4 <- SEM(X = X, params = params, inputs = inputs, pest = pest4)
  expected_out4 <- c(2.5793242, 26.9562645, 2.5793242, 2.5793242, 10.0000112,
                     0.9999991, 699.9997413, 35.1959409, 1.0000000, 5.7292199, 
                     1.3706403, 6.9713243)
  expect_equal(out4, expected_out4, tolerance = test_tol)
  pest5 <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 1)
  out5 <- SEM(X = X, params = params, inputs = inputs, pest = pest5)
  expected_out5 <- c(2.5793242, 26.9563823, 2.5793242, 2.5793242, 
                     10.0014266, 0.9999991, 699.9597929, 35.1959409, 1.0000000,
                     5.7292199, 1.3706948, 6.9713107)
  expect_equal(expected_out5, out5, tolerance = test_tol)
  
})