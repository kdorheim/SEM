test_that("arrhenius works", {
  
  out <- arrhenius(1:10, 1:10)
  expect_true(length(out) == length(1:10))
  
  # The expected output comes directly from the Dietz & Matthes code base
  expected_out <- c(0.4144242, 0.8624766, 1.3458160, 1.8661554, 2.4252627, 3.0249619,3.6671341,
                    4.3537182, 5.0867121, 5.8681737)
  expect_equal(out, expected_out)
  
})