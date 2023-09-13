
test_tol <- 1e-6

### paramters
params <- params_df$value
names(params) <- params_df$parameter
p_df <- params_df

DBH <- 10

PAR <- c(23.5, 51.4, 85.0, 110.8, 148.9, 213.1)
temp <- c(-5.1, -5.2, -5.1, -5.1, -5.1, -5.0)
VPD <- c(0.009, 0.010, 0.009, 0.010, 0.010, 0.009)
precip <- rep(0, 6)
time <- seq(as.POSIXct("2009-01-01 00:00"), as.POSIXct("2014-12-31 23:30"),by = 1800)[1:6]


X <- pools 
test_that("SEM", {

  inputs <- data.frame(PAR = 187.2, temp = -6.2, VPD = 0.013, precip = 0)
  
  # Try out the different pest configurations!
  pest0 <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
  out0 <- SEM(X = X, params = params, inputs = inputs, pest = pest0)
  expected_out0 <- c(2.5793242, 26.9563823, 2.5793242, 2.5793242, 10.0000404, 
                     0.9999991, 699.9997472, 35.1959409, 1.0000000, 5.7292199, 
                     1.3706948, 6.9713107)
  expect_equal(out0, expected_out0, tolerance = test_tol)
  
  pest1 <- c("phloem" = 1, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
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
  expected_out4 <- c(2.579276e+00, 2.695606e+01, 4.718030e-04, 2.579324e+00,
                     9.999959e+00, 9.999991e-01, 6.999997e+02, 3.519594e+01,
                     1.000000e+00, 5.729220e+00, 4.158068e-01, 7.208238e+00)
  expect_equal(out4, expected_out4, tolerance = test_tol)
  
  pest5 <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 1)
  out5 <- SEM(X = X, params = params, inputs = inputs, pest = pest5)
  expected_out5 <- c(2.5793242, 26.9563823, 2.5793242, 2.5793242, 
                     10.0014266, 0.9999991, 699.9597929, 35.1959409, 1.0000000,
                     5.7292199, 1.3706948, 6.9713107)
  expect_equal(expected_out5, out5, tolerance = test_tol)
  
})

test_that("run_SEM control run", {  
  
  inputs <- data.frame("PAR" = PAR, 
                       "temp" = temp, 
                       "VPD" = VPD, 
                       "precip" = precip, 
                       "time" = time)
  
  p_df <- data.frame(parameter = names(params))
  names(params) <- NULL
  p_df$value <- params
  pest0 <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
  SEMout <- run_SEM(pest = pest0, pest.time = NULL, inputs = inputs, X = X, param_df = p_df)
  
  leaf <- c(2.579324, 2.579339, 2.579352, 2.579366, 2.579381, 2.579395)
  wood <- c(26.95628, 26.95647, 26.95669, 26.95692, 26.95714, 26.95733)
  root <- c(2.579324, 2.579339, 2.579352, 2.579366, 2.579381, 2.579395)
  store <- c(2.579324, 2.579339, 2.579352, 2.579366, 2.579381, 2.579395)
  som <- c(10.00001, 10.00001, 10.00003, 10.00004, 10.00005, 10.00005)
  water <- c(0.9999994, 0.9999987, 0.9999981, 0.9999975, 0.9999968, 0.9999962)
  density <- c(699.9997, 699.9995, 699.9992, 699.9990, 699.9987, 699.9984)
  gpp <- c(36.78803, 36.65552, 38.75771, 39.01102, 39.17078, 37.02465)
  Rleaf <- c(5.999626, 5.974619, 5.999660, 5.999690, 5.999724, 6.024851)
  steemroot <- c(1.435331, 1.429352, 1.435344, 1.435352, 1.435362, 1.441374)
  grow <- c(7.283209, 7.088185, 7.627673, 7.666476, 7.707102, 7.165165)

  expect_equal(SEMout$Bleaf, leaf, tolerance = test_tol)
  expect_equal(SEMout$Bwood, wood, tolerance = test_tol)
  expect_equal(SEMout$Broot, root, tolerance = test_tol)
  expect_equal(SEMout$Bstore, store, tolerance = test_tol)
  expect_equal(SEMout$BSOM, som, tolerance = test_tol)
  expect_equal(SEMout$Water, water, tolerance = test_tol)
  expect_equal(SEMout$density, density, tolerance = test_tol)
  expect_equal(SEMout$GPP, gpp, tolerance = test_tol)
  expect_equal(SEMout$Rleaf, Rleaf, tolerance = test_tol)
  expect_equal(SEMout$RstemRroot, steemroot, tolerance = test_tol)
  expect_equal(SEMout$Rgrow, grow, tolerance = test_tol)
  
})


test_that("run_SEM disturbance", {
  
  
  inputs <- data.frame("PAR" = PAR, 
                       "temp" = temp, 
                       "VPD" = VPD, 
                       "precip" = precip, 
                       "time" = time)
  
  p_df <- data.frame(parameter = names(params))
  names(params) <- NULL
  p_df$value <- params
  pest0 <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
  SEMout0 <- run_SEM(pest = pest0, pest.time = NULL, inputs = inputs, X = X, param_df = p_df)
  
  # Make sure that the disturbances affect the expected SEM output 
  pest_time <- inputs$time[2]

  pest_phloem1 <- c("phloem" = 1, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
  SEMout_phloem1 <- run_SEM(pest = pest_phloem1, pest.time = pest_time, inputs = inputs, X = X, param_df = p_df)
  expect_gt(object =  mean(abs(SEMout0$Rgrow - SEMout_phloem1$Rgrow)), expected = 0)
  
  pest_xylem1 <- c("phloem" = 0, "xylem" = 1, "leaf" = 0, "root" = 0, "stem" = 0)
  SEMout_xylem1  <- run_SEM(pest = pest_xylem1, pest.time = pest_time, inputs = inputs, X = X, param_df = p_df)
  expect_gt(object =  mean(abs(SEMout0$GPP - SEMout_xylem1$GPP)), expected = 0)

  pest_leaf1 <- c("phloem" = 0, "xylem" = 0, "leaf" = 1, "root" = 0, "stem" = 0)
  SEMout_leaf1 <- run_SEM(pest = pest_leaf1, pest.time = pest_time, inputs = inputs, X = X, param_df = p_df)
  expect_gt(object =  mean(abs(SEMout0$Bleaf - SEMout_leaf1$Bleaf)), expected = 0)
  
  
  # # TODO Not confident about the implementation of the root and stem pest treatments
  # # Figure out the correct way to do this and then activate these tests.  
  # pest_root1 <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 1, "stem" = 0)
  # SEMout_root1 <- run_SEM(pest = pest_root1, pest.time = pest_time, inputs = inputs, X = X, param_df = p_df)
  # 
  # pest_stem1 <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 1)
  # SEMout_stem1 <- run_SEM(pest = pest_stem1, pest.time = pest_time, inputs = inputs, X = X, param_df = p_df)


  # Test to see if multiple time step disturbance works... 
  pest_time <- inputs$time[2:5]
  pest_xylem_p5 <- c("phloem" = 0, "xylem" = 0.5, "leaf" = 0, "root" = 0, "stem" = 0)
  SEMout_xylem_p5 <- run_SEM(pest = pest_xylem_p5, pest.time = pest_time, inputs = inputs, X = X, param_df = p_df)
  expect_gt(object = mean(abs(SEMout_xylem1$GPP - SEMout_xylem_p5$GPP)), expected = 0)
  
  })


