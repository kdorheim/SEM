library(assertthat)

test_that("check_contents works", {
  # The check_contents should throw an error when a required elements 
  # are missing otherwise the function should pass through an assert 
  # that call. It should work for characters/strings or integers.
  
  
  expect_error(check_contents(req = letters[1:5], check = letters[6:10]), 
               regexp = "following required contents not found: a, b, c, d, e")
  expect_error(assert_that(check_contents(req = letters[1:5], check = letters[6:10])), 
              regexp = "following required contents not found: a, b, c, d, e")
  expect_error(check_contents(req = 1:3, check = letters[1:5]), 
               regexp = "following required contents not found: 1, 2, 3")
  expect_true(assert_that(check_contents(req = letters[1:5], 
                                         check = letters[1:10])))

})