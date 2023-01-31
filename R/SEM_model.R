

pest <- list() # phloem, xylem, leaf, root, stem
pest[["phloem"]] <- 0 
pest[["xylem"]] <- 0 
pest[["leaf"]] <- 0 
pest[["root"]] <- 0 
pest[["stem"]] <- 0 


X <- list()
X[["soil_water"]] <- 1
X[["root"]] <- 1
X[["stem_density"]] <- 1
X[["storage"]] <- 1
inputs <- list()
inputs[["VPD"]] <-  0.886
inputs[["precip"]] <- 0

# Hydrology 
## evaporation (patch)

rho <- 1.15    # density of air, kg/m3 
P <- 101.325   # average atm pressure (kPa)
R <- 8.3144621 # ideal gas constant in J/K/mol
kH2O <- 1e-6*18*10^-6 #mol/umol*gH2O/mol*m/g



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
                   PAR = PARmid, Km = Km, Ca = 400, inital_guess = c(15, 0.1))
  
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
  
  ## KALYN YOU LEFT OFF HERE
  
  ## priority #5, maximum storage
  if(X[4] > Smax){
    
    ## priority #6: Leaf and stem growth
    if(X[1] < Lmax | X[3] < Lmax*params$q){
      leafDemand = Lmax - X[1]
      rootDemand = Lmax*params$q - X[3]
      storeSupply = (X[4]-Smax)/(1+params$Rg)
      falloc = min(1,max(0,storeSupply/(leafDemand+rootDemand)))
      leafAlloc = leafDemand*falloc
      rootAlloc = rootDemand*falloc
      X[1] = X[1] + leafAlloc
      X[3] = X[3] + rootAlloc
      X[4] = X[4] - (leafAlloc+rootAlloc)*(1+params$Rg)
      Rg = Rg + (leafAlloc+rootAlloc)*params$Rg
    }
    
    ## priority #7: Growth & reproduction
    if(X[4] > Smax){
      growAlloc  = (X[4]-Smax)/(1+params$Rg)
      reproAlloc = growAlloc*params$Rfrac
      stemAlloc = growAlloc-reproAlloc
      X[2] = X[2] + stemAlloc
      X[4] = X[4] - growAlloc*(1+params$Rg)
      X[5] = X[5] + reproAlloc*params$SeedlingMort  ## bulk of reproductive allocation dies
      X[7] = X[7] + reproAlloc*(1-params$SeedlingMort)*X[7]/sum(X[1:4]) ## naive reproduction (new trees enter as adults)
      Rg = Rg + growAlloc*params$Rg
    }
    
  }  ## end Store > Smax
  
} ## end Store > Smin







