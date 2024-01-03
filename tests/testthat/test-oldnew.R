test_tol <- 1e-6


test_that("output data has not changed", {
  timestep <- 1800
  p_df <- params_df

  DBH <- 10
  X <- pools

  pest0 <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
  
<<<<<<< HEAD
  xx <- read_SEM_met("inputs.csv")
=======
  xx <- read.csv("inputs.csv")
  xx[["time"]] <- seq(from = as.POSIXct("2005-01-01 00:00"), length.out = 100, by = 1800)
  xx$time <- as.POSIXct(xx$time)
>>>>>>> main
  SEMout <- run_SEM(pest = pest0, pest.time = NULL, inputs = xx, X = X, param_df = p_df)
  
  
  comp_data <- read.csv("comp_data.csv")
  
  expect_equal(comp_data$Bleaf, SEMout$Bleaf, tolerance = test_tol)
  expect_equal(comp_data$BSOM, SEMout$BSOM, tolerance = test_tol)
  
  
})
