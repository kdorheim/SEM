

SEM <- function(X, params, timestep, inputs, pest = c(0, 0, 0, 1, 0), p = 1800) {
  # Hydrology 
  ## evaporation (patch)
  
  rho <- 1.15           # density of air, kg/m3 
  P <- 101.325          # average atm pressure (kPa)
  R <- 8.3144621        # ideal gas constant in J/K/mol
  kH2O <- 1e-6*18*10^-6 #mol/umol*gH2O/mol*m/g
  ## convert umol/m2/sec -> Mg/ha/sec
  k = 1e-6*12*1e-6*10000 #mol/umol*gC/mol*Mg/g*m2/ha
  ktree = 1e-6*12*1e-3   #mol/umol*gC/mol*kg/g -> kg/m2/sec
  kH2O = 1e-6*18*10^-6 #mol/umol*gH2O/mol*m/g
  P = 101.325 ## average atm pressure (kPa)
  
  conversion_factor <- (1/1000) # kg/m2/s to m/s
  EVAP = min(X[["soil_water"]]/timestep, rho * params$gevap * (0.622 * inputs[["VPD"]] / P) * conversion_factor)  ## Bonan Eqn 13.10: rate = m/s
  
  # Not really sure what this is doing... 
  X[["soil_water"]] <- X[["soil_water"]] + inputs[["precip"]]/1000 - EVAP * timestep
  
  # TODO need to figure out more about what the heck is going on here... 
  # Determine the plant available water (trapazoidal response) 
  if(X[["soil_water"]] > params$Wthresh){
    paw <- X[["soil_water"]] - 0.5 * params$Wthresh
  } else {
    paw <- 0.5 * X[["soil_water"]] * X[["soil_water"]] / params$Wthresh
  }
  
  # potential rate of water uptake
  supply <- (1 - pest[["xylem"]]) * params$Kroot * X[["root"]] * paw * X[["stem_density"]] ## potential rate of water uptake, umol/m2Ground/s
  
  # turnover (tree) 
  leafLitter <- X[["leaf"]] * min(1, params$leafLitter + pest[["leaf"]])
  CWD        <- X[["wood"]] * params$CWD
  rootLitter <- X[["root"]] * pest[["root"]] * params$rootLitter
  
  # Update the carbon pools 
  X[["leaf"]] <- X[["leaf"]] - leafLitter
  X[["wood"]] <- X[["wood"]] - CWD
  X[["root"]] <- X[["root"]] - rootLitter
  
  ## Leaf Respiration (m2 leaf)
  Rleaf = arrhenius(params$Rleaf, inputs[["temp"]])   # umol/m2/sec
  
  ## LAI & Canopy Optics (patch) 
  LAI = X[["leaf"]] * params$SLA * (X[["stem_density"]]/10000)  ## m2/m2
  
  # Estimate the mid-canopy PAR (umol/m2/sec)
  PARmid = (1 - 0.5 * exp(-0.5 * LAI)) * inputs[["PAR"]]
  
  # Photosynthesis & Transpiration (plot)
  Ags = c(0, 0)
  if(inputs[["PAR"]] > 1e-20){
    
    Vcmax_adj <- arrhenius(params$Vcmax, inputs[["temp"]])
    Jmax_adj <- arrhenius(params$Jmax, inputs[["temp"]])
    gamma_star <- 42.75 * exp(37830 * (convert_temp_C_K(inputs[["temp"]]) - 298)/(298 * R * convert_temp_C_K(inputs[["temp"]])))
    
    Ags <- solve.FVcB(Vcmax = Vcmax_adj, Jmax = Jmax_adj, Rleaf = Rleaf, Gstar = gamma_star, 
                      alpha = params$alpha, m = params$m, g0 = params$g0, VPD = inputs$VPD,
                      PAR = PARmid, Km = params$Km, Ca = 400, inital_guess = c(15, 0.1))
    
    # transpiration without water limitation, umol/m2/s
    demand = max(Ags[2] * 0.622 * inputs[["VPD"]] / P * LAI * 1e6, 1e-10) 
    fopen = max(0, min(1, supply/demand))
    GPP = (Ags[1] + Rleaf) * fopen * LAI               # umol/m2/sec
    TRANSP = demand * fopen                            # umol/m2/sec
    X[["soil_water"]] = X[["soil_water"]] - TRANSP * timestep * kH2O
    
  } else {
    fopen <- GPP <- TRANSP <- 0
  }
  
  # Respiration & Carbon Allocation 
  
  ## maintainence respiration (priority #2) (umol/s/tree)
  GPP = GPP * 10000 / X[["stem_density"]]
  Rleaf = Rleaf * LAI * 10000 / X[["stem_density"]]
  Rstem = X[["wood"]] * arrhenius(params$Rstem, inputs[["temp"]])
  Rroot = X[["root"]] * arrhenius(params$Rroot, inputs[["temp"]])
  Rg = 0  ## growth respiration: kg per plant per timestep
  
  # Update storage for priorities 1 & 2 (kg/tree) 
  X[["storage"]] = X[["storage"]] + ((1 - pest[["phloem"]]) * (GPP - Rleaf) - Rstem - Rroot) * ktree * timestep
  
  ## calculate allometric potentials
  DBH = (X[["wood"]] / params$allomB0)^(1 / params$allomB1)  ## infer DBH from woody biomas
  Lmax = params$allomL0 * DBH^params$allomL1        ## set maximum leaf biomass from DBH
  Lmin = params$Lmin * Lmax                         ## set minimum leaf and root biomass as a fraction of maximum
  Rmin = Lmin * params$q
  Smin = (Rleaf * LAI * 10000 / X[["stem_density"]] + Rstem + Rroot) * ktree * 86400 * params$StoreMinDay  ## set minimum storage based on the number of days the plant could survive
  Smax = params$Smax * Lmax                         ## Set maximum storage biomas as a multiplier to maximum leaf biomass (similar to Fisher et al 2010)
  
  ## priority 3: only allocate if store above minimum  
  if(X[["storage"]] > Smin){
    
    leafGrowMax = Lmax * params$Kleaf * 2^(inputs[["temp"]]/10)  #thermal limit to growth
    
    ## priority #4 mimimum leaf and root
    if(X[["leaf"]] < Lmin){
      leafAlloc <- max(min(Lmin - X[["leaf"]], (X[["storage"]] - Smin) / (1 + params$Rg), leafGrowMax), 0)  #demand,supply
      X[["leaf"]] <- X[["leaf"]] + leafAlloc
      X[["storage"]] <- X[["storage"]] - leafAlloc * (1 + params$Rg)
      Rg <- Rg + leafAlloc * params$Rg
    }
    if(X[["root"]] < Rmin){
      rootAlloc <- max(min(Lmin - X[["leaf"]], (X[["storage"]] - Smin) / (1 + params$Rg)),0)
      X[["root"]] = X[["root"]] + rootAlloc
      X[["storage"]] = X[["storage"]] - rootAlloc*(1 + params$Rg)
      Rg = Rg + rootAlloc * params$Rg
    }

    ## priority #5, maximum storage
    if(X[["storage"]] > Smax){
      
      ## priority #6: Leaf and stem growth
      if(X[["leaf"]] < Lmax | X[["root"]] < Lmax*params$q){
        leafDemand = Lmax - X[["leaf"]]
        rootDemand = Lmax*params$q - X[["root"]]
        storeSupply = (X[["storage"]]-Smax)/(1+params$Rg)
        falloc = min(1,max(0,storeSupply/(leafDemand+rootDemand)))
        leafAlloc = leafDemand*falloc
        rootAlloc = rootDemand*falloc
        X[["leaf"]] = X[["leaf"]] + leafAlloc
        X[["root"]] = X[["root"]] + rootAlloc
        X[["storage"]] = X[["storage"]] - (leafAlloc+rootAlloc)*(1+params$Rg)
        Rg = Rg + (leafAlloc+rootAlloc)*params$Rg
      }
      
      ## priority #7: Growth & reproduction
      if(X[["storage"]] > Smax){
        growAlloc  = (X[["storage"]]-Smax)/(1+params$Rg)
        reproAlloc = growAlloc*params$Rfrac
        stemAlloc = growAlloc-reproAlloc
        X[["wood"]] = X[["wood"]] + stemAlloc
        X[["storage"]] = X[["storage"]] - growAlloc*(1+params$Rg)
        X[["som"]] = X[["som"]] + reproAlloc*params$SeedlingMort  ## bulk of reproductive allocation dies
        X[["stem_density"]] = X[["stem_density"]] + reproAlloc*(1-params$SeedlingMort)*X[["stem_density"]]/ sum(c(X[["leaf"]], X[["wood"]], X[["root"]], X[["storage"]])) ## naive reproduction (new trees enter as adults)
        Rg = Rg + growAlloc*params$Rg
      }
      
    }  ## end Store > Smax
    
  } ## end Store > Smin
  
  ## mortality
  if (X[["storage"]] <= params$NSCthreshold * Smax) {
    X[["stem_density"]] <- 0
  } else {
    mortRate <- (pest[["stem"]] + params$mort1 * exp(-params$mort2 * X[["storage"]] / Smax)) * timestep / 86400 / 365
    X[["som"]] <- X[["som"]] + X[["stem_density"]] * mortRate * sum(c(X[["leaf"]], X[["wood"]], X[["root"]], X[["storage"]])) / 1000 ## dead trees go to SOM
    X[["stem_density"]] <- X[["stem_density"]] * (1 - mortRate) ## reduce density but not per-tree pools
  }
  
  ## soil respiration
  Rh <- min(params$Rbasal * X[["som"]] * params$Q10^(inputs$temp / 10), X[["som"]] / (k * timestep)) ## min ensures SOM never goes negative
  X[["som"]] <- X[["som"]] - Rh * k * timestep
  
  out <- c(X, GPP, fopen, Rleaf, Rstem + Rroot, Rg / ktree / timestep)
  names(out) <- NULL
  return(out)
  
}






