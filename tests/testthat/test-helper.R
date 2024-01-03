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

  
  time <- c(200701010000, 200701010030, 200701010100, 200701010130, 200701010200, 200701010230)

  inputs <- data.frame(PAR = PAR, temp = temp, VPD = VPD, precip = precip, time = time)
  
  X <- pools
  pest <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
  
  # Should pass with no problem 
  expect_true(assert_that(check_SEM_run_setup(pest = pest, pest.time = NULL, 
                                              inputs = inputs, X = X, param_df = p_df, 
                                              DBH = DBH, quiet = TRUE)))
  
  # Throw error if pest = 0 but pest.time = time 
  pest_time <- inputs$time[2]
  expect_error(check_SEM_run_setup(pest = pest, pest.time = pest_time, 
                                   inputs = inputs, X = X, param_df = p_df, 
                                   DBH = DBH, quiet = TRUE), "no pest treatment defined")
  
  # Throw error if pest != 0 but pest.time = NULL
  pest[1] <- 1
  expect_error(check_SEM_run_setup(pest = pest, pest.time = NULL, inputs = inputs, 
                                   DBH = DBH, X = X, param_df = p_df), 
               "if pest.time is NULL all elements of pest must be set to 0")
  
  
  
  # If both pest and pest.time are defined there should be no errors thrown 
  expect_true(check_SEM_run_setup(pest = pest, pest.time = pest_time, 
                                  inputs = inputs,  DBH = DBH, X = X, param_df = p_df))
  
  
})


test_that("update_params", {
  
  expect_error(update_params(df = params_df[1:4, ], new = c("Kroot" = 0.5)))
  expect_error(update_params(df = params_df, new = c("fake" = 0.5)))
  
  new_val <- c("Kroot" = 0.5)
  new_df <- update_params(df = params_df, new = new_val)
  expect_true(new_df[new_df$parameter == "Kroot", ]$value == new_val)
  expect_true(unlist(new_df[new_df$parameter == "Kroot", ]$value) != unlist(params_df[params_df$parameter == "Kroot", ]$value))
  
})

# TODO this is to address after all switched over to the YYYYMMDDHHMM format 
test_that("read_SEM_met", {
  expect_error(read_SEM_met("comp_data.csv"))
  x <- read_SEM_met("inputs.csv")

})
