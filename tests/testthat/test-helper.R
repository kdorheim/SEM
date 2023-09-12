library(assertthat)

test_that("check_contents works", {
  # The check_contents should throw an error when a required elements
  # are missing otherwise the function should pass through an assert
  # that call. It should work for characters/strings or integers.
  expect_error(check_contents(req = letters[1:5], check = letters[6:10]),
                regexp = "following required contents not found: a, b, c, d, e")
  expect_error(assert_that(check_contents(req = letters[1:5],
                                          check = letters[6:10])),
               regexp = "following required contents not found: a, b, c, d, e")
  expect_error(check_contents(req = 1:3, check = letters[1:5]),
               regexp = "following required contents not found: 1, 2, 3")
  expect_true(assert_that(check_contents(req = letters[1:5],
                                         check = letters[1:10])))
})


test_that("check_SEM_run_setup", {

  timestep <- 1800
  
  params <- list()
  params$gevap <- 0.005  
  params$Wthresh <- 1
  params$Kroot <- 0.2 
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
  
  DBH <- 10
  
  PAR <- c(23.5, 51.4, 85.0, 110.8, 148.9, 213.1)
  temp <- c(-5.1, -5.2, -5.1, -5.1, -5.1, -5.0)
  VPD <- c(0.009, 0.010, 0.009, 0.010, 0.010, 0.009)
  precip <- rep(0, 6)
  time <- seq(as.POSIXct("2009-01-01 00:00"), as.POSIXct("2014-12-31 23:30"),by = 1800)[1:6]
  inputs <- data.frame(PAR = PAR, temp = temp, VPD = VPD, precip = precip, time = time)
  
  X <- rep(1, 7)
  X[1] <- X[3] <- X[4] <- params$allomL0 * DBH ^ params$allomL1
  X[2] <- params$allomB0 * DBH ^ params$allomB1
  X[5] <- 10
  X[7] <- 700
  names(X) <- c("leaf", "wood", "root", "storage", "som", "soil_water",
                "stem_density")
  
  pest <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
  
  # Should pass with no problem 
  assert_that(check_SEM_run_setup(pest = pest, pest.time = pest.time, 
                                  inputs = inputs, X = X, param_df = param_df, 
                                  DBH = DBH, quiet = TRUE))
  
  # Make sure that errors are thrown as expected
  expect_error(check_SEM_run_setup(pest = pest, pest.time = 1, inputs = inputs, X = X, param_df = p_df), 
               label = "Error: pest.time must be an element of inputs$time")

  pest[1] <- 1
  pest_time <- inputs$time[2]
  expect_error(run_SEM(pest = pest, pest.time = NULL, inputs = inputs, X = X, param_df = p_df), 
               label  = "Error: if pest.time is NULL all elements of pest must be set to 0")
  
  expect_error(run_SEM(pest = pest0, pest.time = pest_time, inputs = inputs, X = X, param_df = p_df), 
               label  = "Error: if pest.time is NULL all elements of pest must be set to 0 did not throw the expected error.")
 
})
