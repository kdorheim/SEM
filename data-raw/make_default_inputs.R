# Make a copy of the default parameter values and initial values store in one place.
# TODO replace when figure out which parameters can be fixed or are hard coded SEM 
# assumptions/values. 

timestep <- 1800
params <- list()
## hydrology
params$gevap <- 0.005  
params$Wthresh <- 1
params$Kroot <- 0.2 
R <- 8.3144621
Tleaf <- 298
Kc <- 404.9 * exp(79430 * (Tleaf - 298) / (298 * R * Tleaf))
Ko <- 278.4 * exp(36380 * (Tleaf - 298) / (298 * R * Tleaf))
Km <- Kc * (1 + 210 / Ko)
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
params$Km <- Kc * (1 + 210 / Ko)

# Save the initial pools. 
X[1] <- X[3] <- X[4] <- params$allomL0 * DBH ^ params$allomL1
X[2] <- params$allomB0 * DBH ^ params$allomB1
X[5] <- 10
X[7] <- 700
names(X) <- c("leaf", "wood", "root", "storage", "som", "soil_water", "stem_density")
pools <- X
usethis::use_data(pools, overwrite = TRUE)

# Format into a data frame
p_df <- data.frame(parameter = names(params))
names(params) <- NULL
p_df$value <- params
params_df <- p_df
  
usethis::use_data(params_df, overwrite = TRUE)
