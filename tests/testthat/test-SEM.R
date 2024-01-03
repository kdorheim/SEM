test_tol <- 1e-6


# Define the paramters and inputs that will be used by all of thests 
params <- params_df$value
names(params) <- params_df$parameter
p_df <- params_df

DBH <- 10

PAR <- c(23.5, 51.4, 85.0, 110.8, 148.9, 213.1)
temp <- c(-5.1, -5.2, -5.1, -5.1, -5.1, -5.0)
VPD <- c(0.009, 0.010, 0.009, 0.010, 0.010, 0.009)
precip <- rep(0, 6)
time <- c(200701010000, 200701010030, 200701010100, 200701010130, 200701010200, 200701010230)

X <- pools 

test_that("SEM", {

  inputs <- data.frame(PAR = 187.2, temp = -6.2, VPD = 0.013, precip = 0)
  
  # Try out the different pest configurations!
  pest0 <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
  out0 <- SEM(X = X, params = params, inputs = inputs, pest = pest0)
  expected_out0 <- c(2.5793242, 26.9562666, 2.5793242, 2.5793242, 10.0000118,
                     0.9999991, 699.9997414, 35.3593822, 1.0000000,
                     5.7292199, 1.3706403, 7.0118774, 1.8054930, 0.2300483)
  expect_equal(out0, expected_out0, tolerance = test_tol)
  
  pest1 <- c("phloem" = 1, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
  out1 <- SEM(X = X, params = params, inputs = inputs, pest = pest1)
  expected_out1 <- c(2.5792757, 26.9560561, 2.5791770, 2.5792946, 9.9999597,  0.9999991, 
                     699.9997308, 35.3593822, 1.0000000, 
                     5.7292199, 1.3706403, 0.0000000, 1.8054930, 0.2300471)
  expect_equal(out1, expected_out1, tolerance = test_tol)
  
  pest2 <- c("phloem" = 0, "xylem" = 1, "leaf" = 0, "root" = 0, "stem" = 0)
  out2 <- SEM(X = X, params = params, inputs = inputs, pest = pest2)
  expected_out2 <- c(2.5792757, 26.9560561, 2.5793242, 2.5791709, 9.9999597, 
                    0.9999992, 699.9997307, 0.0000000, 0.0000000, 5.7292199, 
                    1.3706948, 0.0000000, 1.8054930, 0.2300471)
  expect_equal(out2, expected_out2, tolerance = test_tol)
  
  pest3 <- c("phloem" = 0, "xylem" = 0, "leaf" = 1, "root" = 0, "stem" = 0)
  out3 <- SEM(X = X, params = params, inputs = inputs, pest = pest3)
  expected_out3 <- c(2.943282e-04, 2.695606e+01, 2.579324e+00, 2.578903e+00, 
                     9.999959e+00, 9.999992e-01, 6.999997e+02, 0.000000e+00, 
                     1.000000e+00, 0.000000e+00, 1.370695e+00, 4.496680e+00, 
                     0.000000e+00, 2.300471e-01)
  expect_equal(out3, expected_out3, tolerance = test_tol)
  
  pest4 <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 1, "stem" = 0)
  out4 <- SEM(X = X, params = params, inputs = inputs, pest = pest4)

  expected_out4 <- c(2.579276e+00, 2.695606e+01, 4.744574e-04, 2.579324e+00, 9.999959e+00,
                     9.999991e-01, 6.999997e+02, 3.535938e+01, 1.000000e+00, 5.729220e+00, 
                     4.158068e-01, 7.248791e+00, 1.805493e+00, 2.300471e-01)

  expect_equal(out4, expected_out4, tolerance = test_tol)
  
  pest5 <- c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 1)
  out5 <- SEM(X = X, params = params, inputs = inputs, pest = pest5)
  expected_out5 <- c(2.5793242, 26.9562666, 2.5793242, 2.5793242, 10.0013979, 0.9999991, 
                     699.9597871, 35.3593822, 1.0000000,
                     5.7292199, 1.3706403, 7.0118774, 1.8054930, 0.2300802)
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
  
  leaf <- c(2.579324, 2.579339 ,2.579352, 2.579365, 2.579378, 2.579391)
  wood <- c(26.95628, 26.95648, 26.95668, 26.95688, 26.95707, 26.95727)
  root <- c(2.579324, 2.579339, 2.579352, 2.579365, 2.579378, 2.579391)
  store <- c(2.579324, 2.579339, 2.579352, 2.579365, 2.579378, 2.579391)
  som <- c(10.00001, 10.00002, 10.00002, 10.00003, 10.00003, 10.00004)
  water <- c(0.9999994, 0.9999987, 0.9999981, 0.9999974, 0.9999967, 0.9999961)
  density <- c(699.9997, 699.9995, 699.9992, 699.9990, 699.9987, 699.9984)
  gpp <- c(37.03006, 36.86539, 37.03028, 37.01329, 37.01347, 37.17951)
  Rleaf <- c(5.999626, 5.974619, 5.999661, 5.999690, 5.999721, 6.024845)
  steemroot <- c(1.435331, 1.429352, 1.435344, 1.435352, 1.435360, 1.441372)
  grow <- c(7.343263, 7.137881, 7.197347, 7.188007, 7.188951, 7.222290)
  LAI <- c(1.805493, 1.805492, 1.805502, 1.805510, 1.805519, 1.805527)

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
  expect_equal(SEMout$LAI, LAI, tolerance = test_tol)
  
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


