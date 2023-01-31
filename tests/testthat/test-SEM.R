test_that("SEM works", {
  

  ### paramters
  timestep = 1800 #seconds
  params = list()
  ## hydrology
  params$gevap = 0.005  ## m2/s (Bonan p 201) [was 0.01, tuned]
  params$Wthresh = 1
  params$Kroot = 0.2  # umolH20*ha/(m3*kgRoot*s) -> uptake per kg root per m available water
  ## photosynthesis
  R = 8.3144621 ## ideal gas constant in J/K/mol
  Tleaf = 298
  Kc = 404.9*exp(79430*(Tleaf - 298)/(298*R*Tleaf))
  Ko = 278.4*exp(36380*(Tleaf - 298)/(298*R*Tleaf))
  Km = Kc*(1+210/Ko)
  params$SLA = 10
  params$alpha = 0.8
  params$Vcmax = 18
  params$Jmax  = params$Vcmax*1.67
  params$m     = 4
  params$g0    = 0
  ##allometry
  params$allomB0 = exp(-2.5355)/0.8  ## Jenkins 2004, Pine, adj for BGB
  params$allomB1 = 2.4349
  params$allomL0 = exp(-2.907)  ##Jenkins full database softwoods, Table 3
  params$allomL1 = 1.674
  ## plant respiration
  params$Rleaf = 0.04*params$Vcmax
  params$Rroot = 1.2
  params$Rstem = 0.05
  params$Rg    = 0.33
  ## soil respiration
  params$Q10 = 2
  params$Rbasal = 0.2/(params$Q10^2.5) #umol/m2/sec per Mg/ha of SOM
  ## turnover
  params$leafLitter = 0.33/365/86400*timestep
  params$CWD = 0.0001/365/86400*timestep
  params$rootLitter = 1.0/365/86400*timestep
  ## mortality
  params$mort1 = 1
  params$mort2 = 5
  params$NSCthreshold = 0.01
  ## NSC Allocation
  params$Lmin = 0.75
  params$q    = 1
  params$StoreMinDay = 2
  params$Smax = 1
  params$Rfrac = 0.2
  params$SeedlingMort = 0.99
  params$Kleaf = (1/21/48)/2^2.5  ## assumes it takes 21 days to regrow at 25C
  DBH = 10
  X = rep(1,7)
  X[1] = X[3] = X[4] = params$allomL0*DBH^params$allomL1 
  X[2] = params$allomB0*DBH^params$allomB1 
  X[5] = 10
  X[7] = 700
  inputs <- data.frame(PAR = 200.800, temp = 11.9, VPD = 0.858, precip = 0.508)
  # SEM(X = X, params = params, inputs = inputs, pest =c(0,0,0,1,0), timestep = 1800) 
  # This expected value comes from the Dietz implementations of the function and their default values
  expected_out <- c(2.579324, 26.957172, 2.579324, 2.579324, 10.000111, 1.000452, 699.999787, 
                    112.438358, 1.000000, 11.695095, 2.797897, 24.302350)
  
})
