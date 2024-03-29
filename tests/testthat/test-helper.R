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

  p_df <- params_df
  DBH <- 10
  
  PAR <- c(23.5, 51.4, 85.0, 110.8, 148.9, 213.1)
  temp <- c(-5.1, -5.2, -5.1, -5.1, -5.1, -5.0)
  VPD <- c(0.009, 0.010, 0.009, 0.010, 0.010, 0.009)
  precip <- rep(0, 6)
  time <- seq(as.POSIXct("2009-01-01 00:00"), as.POSIXct("2014-12-31 23:30"),by = 1800)[1:6]
  inputs <- data.frame(PAR = PAR, temp = temp, VPD = VPD, precip = precip, time = time)
  
  X <- pools
  pest <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
  
  # Should pass with no problem 
  assert_that(check_SEM_run_setup(pest = pest, pest.time = NULL, 
                                  inputs = inputs, X = X, param_df = p_df, 
                                  DBH = DBH, quiet = TRUE))

  pest[1] <- 1
  pest_time <- inputs$time[2]
  expect_error(run_SEM(pest = pest, pest.time = NULL, inputs = inputs, X = X, param_df = p_df), 
               label  = "Error: if pest.time is NULL all elements of pest must be set to 0")
  
  expect_error(run_SEM(pest = pest0, pest.time = pest_time, inputs = inputs, X = X, param_df = p_df), 
               label  = "Error: if pest.time is NULL all elements of pest must be set to 0 did not throw the expected error.")
 
})

