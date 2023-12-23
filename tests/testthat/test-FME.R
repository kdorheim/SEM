PAR <- c(23.5, 51.4, 85.0, 110.8, 148.9, 213.1)
temp <- c(-5.1, -5.2, -5.1, -5.1, -5.1, -5.0)
VPD <- c(0.009, 0.010, 0.009, 0.010, 0.010, 0.009)
precip <- rep(0, 6)
time <- c(200701010000, 200701010030, 200701010100, 200701010130, 200701010200, 200701010230)

inputs <- data.frame(time = time, PAR = PAR, temp = temp, VPD = VPD, precip = precip)


test_that("SEM_sensrange & format_sensout", {
  
  pars <-  c("gevap" = 0.005)
  prange <- data.frame(min = pars - pars * 0.75, max = pars + pars * 0.75)
  
  out <- SEM_sensrange(pars = pars, 
                       parRange = prange, 
                       param_df = params_df,
                       inputs = inputs,
                       X = pools,
                       DBH = 10, n = 5)
  expect_true(all(class(out) == list("sensRange", "data.frame")))
  
  outout <- format_sensout(out)
  expect_true(all(class(outout) == list( "summary.sensRange", "data.frame")))
  expect_true(all(dim(outout) == c(84, 11)))
  
})


test_that("SEM_sensfunc", {
  
  pars <-  c("gevap" = 0.005)
  out <- SEM_sensfunc(pars = pars, 
                      param_df = params_df,
                      inputs = inputs,
                      X = pools,
                      DBH = 10)
  
  expect_true(all(class(out) == list("sensFun", "data.frame")))
  outout <- format_sensout(out)
  expect_true(all(class(outout) == list("data.table", "data.frame")))
  
})
