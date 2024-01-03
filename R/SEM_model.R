#' Internal implementation of the SEM model
#'
#' @param X named numeric vector containing the following
#' \describe{
#' \item{leaf}{kg/plant}
#' \item{wood}{kg/plant}
#' \item{root}{kg/plant}
#' \item{storage}{kg/plant}
#' \item{som}{soil organic matter, Mg/ha}
#' \item{soil_water}{m}
#' \item{stem_density}{stems/ha}
#' }
#' @param params named vector of the parameter values
#' @param inputs named numeric vector containing the meteorological variables at a single time point
#' \describe{
#' \item{temp}{Air temperature, degrees C}
#' \item{precip}{Precipitation, mm}
#' \item{VPD}{Vapor pressure deficit, kPa}
#' \item{PAR}{Incoming photosynthetically active radiation, umol/m2/s}
#' }
#' @param pest named vector for the pest impacts 
#' \describe{
#' \item{phloem}{phloem feaders TODO figure out the limits}
#' \item{xylem}{xylem disrupters (bark beetle, canker, wilt, girdling) TODO figure out the limits}
#' \item{leaf}{defoliators TODO figure out the limits}
#' \item{root}{root rot TODO figure out the limits}
#' \item{stem}{stem rot TODO figure out the limits}
#' }
#' @return vector of results
#' @noRd
SEM <- function(X, params, inputs, pest, timestep = 1800) {

  # Constants
  rho <- 1.15           # density of air, kg/m3 
  P <- 101.325          # average atm pressure (kPa)
  R <- 8.3144621        # ideal gas constant in J/K/mol
  timestep <- 1800      # number of seconds in 30 min     
  
  # Conversion Factors
  k <- 1e-6*12*1e-6*10000 #(mol/umol)*(gC/mol)*(Mg/g)*(m2/ha) ->  Mg/ha/sec
  ktree <- 1e-6*12*1e-3   #(mol/umol)*(gC/mol)*(kg/g) -> kg/m2/sec
  kH2O <- 1e-6*18*10^-6   #(mol/umol)*(gH2O/mol)*(m/g) -> m h2o
  
  Tleaf <- 298
  Kc <- 404.9 * exp(79430 * (Tleaf - 298) / (298 * R * Tleaf))
  Ko <- 278.4 * exp(36380 * (Tleaf - 298) / (298 * R * Tleaf))
  Km <- Kc * (1 + 210 / Ko)
  
  # Patch Hydrology  -----------
  conversion_factor <- (1/1000) # kg/m2 to m
  EVAP = min(X[["soil_water"]]/timestep, rho * params[["gevap"]] * (0.622 * inputs[["VPD"]] / P) * conversion_factor)  # m/s
  X[["soil_water"]] <- X[["soil_water"]] + (inputs[["precip"]] * conversion_factor) - (EVAP * timestep) # m
  
  # The total amount of plant available water use.
  if(X[["soil_water"]] > params[["Wthresh"]]){
    # Plant available moisture trapezoidal response (m3/m2) 
    paw <- X[["soil_water"]] - 0.5 * params[["Wthresh"]]
  } else {
    # Otherwise plant available moisture is half of the linear relationship. 
    paw <- 0.5 * X[["soil_water"]] * X[["soil_water"]] / params[["Wthresh"]]
  }

  
  # Determine the potential rate of water uptake based on the availability 
  # and also on the amount of tree available, umol/m2Ground/s
  supply <- (1 - pest[["xylem"]]) * X[["root"]] * params[["Kroot"]] * paw * X[["stem_density"]] 
  

  # Update biomass based on turnover  --------- 
  # Use leaf/stem/root turnover rate to calculate the litter pools, then 
  # update the biomass pools appropriately. 
  # Calculate the litter pools the leaf and root litter are adjusted by the "pest" treatment.
  leafLitter  <- X[["leaf"]] * min(1, params[["leafLitter"]] + pest[["leaf"]]) # At most all the leafs can drop
  rootLitter  <- X[["root"]] * min(1, params[["rootLitter"]] + pest[["root"]]) # pest root is hard coded to equal the value of 1
  CWD         <- X[["wood"]] * params[["CWD"]]
  X[["leaf"]] <- X[["leaf"]] - leafLitter
  X[["wood"]] <- X[["wood"]] - CWD
  X[["root"]] <- X[["root"]] - rootLitter
  
  # Respiration & Photosynthesis ------------
  # Adjust the base leaf respiration to the environmental conditions. 
  Rleaf = arrhenius(params[["Rleaf"]], inputs[["temp"]]) 
  
  ## LAI & Canopy Optics (patch) 
  LAI = X[["leaf"]] * params[["SLA"]] * (X[["stem_density"]]/10000) 
  
  # Estimate the mid-canopy PAR (umol/m2/sec)
  PARmid = (1 - 0.5 * exp(-0.5 * LAI)) * inputs[["PAR"]]
  
  # Leaf-level photosynthesis was modeled using a standard enzyme-kinetic 
  # approach (Farquhar et al. 1980) coupled to the Medlyn variant of the 
  # Ball–Berry stomatal conductance model (Medlyn et al. 2011) and scaled to
  # gross primary productivity (GPP) based on leaf area index (LAI).
  Ags = c(0, 0) # Arg[1] = Photosynthesis, Arg[2] = water use
  if(inputs[["PAR"]] > 1e-20){
    
    Vcmax_adj <- arrhenius(params[["Vcmax"]], inputs[["temp"]])
    Jmax_adj <- arrhenius(params[["Jmax"]], inputs[["temp"]])
    
    # CO2 compensation point, temperature dependent assumed to be same for all C3 species see Medlyn et al. 2011
    gamma_star <- 42.75 * exp(37830 * (convert_temp_C_K(inputs[["temp"]]) - 298)/(298 * R * convert_temp_C_K(inputs[["temp"]])))
    
    # Solve for plot level photosynthesis & transpiration. This approach accounts for 
    # temperature, CO2, and light availability. Note that C update to water loss 
    # is optimized. 
    Ags <- solve.FVcB(Vcmax = Vcmax_adj, 
                      Jmax = Jmax_adj, 
                      Rleaf = Rleaf, 
                      Gstar = gamma_star, 
                      alpha = params[["alpha"]],
                      m = params[["m"]], 
                      g0 = params[["g0"]], 
                      VPD = inputs$VPD,
                      PAR = PARmid,
                      Km = Km, 
                      Ca = 400, 
                      inital_guess = c(15, 0.1))
    
    # transpiration without water limitation, umol/m2/s
    demand = max(Ags[2] * 0.622 * inputs[["VPD"]] / P * LAI * 1e6, 1e-10) 
    # fraction of potential water demand that can be supplied under normal conditions
    # If insufficient water is available for transpiration then there will be no 
    # photosynthesis, GPP = 0. 
    fopen = max(0, min(1, supply/demand))
    GPP = (Ags[1] + Rleaf) * fopen * LAI               # umol/m2/sec
    TRANSP = demand * fopen                            # umol/m2/sec
    
    # Update soil water by transpiration 
    X[["soil_water"]] = X[["soil_water"]] - TRANSP * timestep * kH2O
    
  } else {
    # If there is no light then there is no photosynthesis, GPP, or transpiration.
    fopen <- GPP <- TRANSP <- 0
  }
  
  # Allocate Carbon based on the SEM rules -----------------------
  #  1 - carbon is only available once it's in the phloem
  #  2 - maintainence respiration
  #  3 - minimum storage: maintain enough C for n days of Rm
  #  4 - minimum leaf & root
  #      roots:leaves constant (such that for climatic mean Waterbalance, maintain E[supply/demand]~1)
  #      but if stressed, allowed to loose leaves
  #  5 - storage: maintain a proportion of potential Bleaf
  #  6 - allometric maximum leaf & root
  #  7 - stem growth
  # Update biomass/allocation of carbon with respiration numbers --------
  ## maintenance respiration (priority #2) (umol/s/tree)
  # Scale from leaf level GPP and respiration 
  GPP = GPP * 10000 / X[["stem_density"]] # Scale from leaf level GPP to plot 
  Rleaf = Rleaf * LAI * 10000 / X[["stem_density"]]
  
  # Adjust basal stem and root respiration to account for the temperature. 
  Rstem = X[["wood"]] * arrhenius(params[["Rstem"]], inputs[["temp"]])
  Rroot = X[["root"]] * arrhenius(params[["Rroot"]], inputs[["temp"]])
  
  # Growth respiration (kg per plant per timestep) assumed to be 0  
  # until carbon priorities 1-6 are met. 
  Rg = 0 
  
  # Update storage for priorities 1 & 2: --------
  X[["storage"]] = X[["storage"]] + ((1 - pest[["phloem"]]) * (GPP - Rleaf) - Rstem - Rroot) * ktree * timestep
  
  # Calculate the min and max allometric potentials
  DBH = (X[["wood"]] / params[["allomB0"]])^(1 / params[["allomB1"]])  ## infer DBH from woody biomass
  Lmax = params[["allomL0"]] * DBH^params[["allomL1"]]       ## set maximum leaf biomass from DBH
  Lmin = params[["Lmin"]] * Lmax                         ## set minimum leaf and root biomass as a fraction of maximum
  Rmin = Lmin * params[["q"]]                            ## Leaf:root biomass ratio
  Smin = (Rleaf * LAI * 10000 / X[["stem_density"]] + Rstem + Rroot) * ktree * 86400 * params[["StoreMinDay"]]  ## set minimum storage based on the number of days the plant could survive
  Smax = params[["Smax"]] * Lmax                         ## Set maximum storage biomass as a multiplier to maximum leaf biomass (similar to Fisher et al 2010)

  # Priority 3: --------
  # Allocate carbon if and only if there is enough C available to continue maintenance respiration
  if(X[["storage"]] > Smin){
    
    leafGrowMax = Lmax * params[["Kleaf"]] * 2^(inputs[["temp"]]/10)  # thermal limit to growth
    
    # Priority 4: ---------
    # Minimum leaf & root carbon allocation 
    #   roots:leaves constant (such that for climatic mean Waterbalance, maintain E[supply/demand]~1)
    #   but if stressed, allowed to loose leaves
    if(X[["leaf"]] < Lmin){
      leafAlloc <- max(min(Lmin - X[["leaf"]], (X[["storage"]] - Smin) / (1 + params[["Rg"]]), leafGrowMax), 0)  #demand,supply
      X[["leaf"]] <- X[["leaf"]] + leafAlloc
      X[["storage"]] <- X[["storage"]] - leafAlloc * (1 + params[["Rg"]])
      Rg <- Rg + leafAlloc * params[["Rg"]]
    }
    if(X[["root"]] < Rmin){
      rootAlloc <- max(min(Lmin - X[["leaf"]], (X[["storage"]] - Smin) / (1 + params[["Rg"]])),0)
      X[["root"]] = X[["root"]] + rootAlloc
      X[["storage"]] = X[["storage"]] - rootAlloc*(1 + params[["Rg"]])
      Rg = Rg + rootAlloc * params[["Rg"]]
    }
    
    # Priority 5: ----------
    ## maintain a proportion of potential Bleaf
    if(X[["storage"]] > Smax){
      
      ## Priority 6 ----------
      ## Leaf and stem growth
      if(X[["leaf"]] < Lmax | X[["root"]] < Lmax*params[["q"]]){
        leafDemand = Lmax - X[["leaf"]]
        rootDemand = Lmax*params[["q"]] - X[["root"]]
        storeSupply = (X[["storage"]]-Smax)/(1+params[["Rg"]])
        falloc = min(1,max(0, storeSupply/(leafDemand+rootDemand)))
        leafAlloc = leafDemand * falloc
        rootAlloc = rootDemand * falloc
        X[["leaf"]] = X[["leaf"]] + leafAlloc
        X[["root"]] = X[["root"]] + rootAlloc
        X[["storage"]] = X[["storage"]] - (leafAlloc+rootAlloc)*(1+params[["Rg"]])
        Rg = Rg + (leafAlloc + rootAlloc) * params[["Rg"]]
      }
      
      ## Priority 7  ----------
      ## Growth & reproduction
      if(X[["storage"]] > Smax){
        growAlloc  = (X[["storage"]]-Smax)/(1+params[["Rg"]])
        reproAlloc = growAlloc*params[["Rfrac"]]
        stemAlloc = growAlloc-reproAlloc
        X[["wood"]] = X[["wood"]] + stemAlloc
        X[["storage"]] = X[["storage"]] - growAlloc*(1+params[["Rg"]])
        X[["som"]] = X[["som"]] + reproAlloc*params[["SeedlingMort"]]  ## bulk of reproductive allocation dies
        X[["stem_density"]] = X[["stem_density"]] + reproAlloc*(1-params[["SeedlingMort"]])*X[["stem_density"]]/ sum(c(X[["leaf"]], X[["wood"]], X[["root"]], X[["storage"]])) ## naive reproduction (new trees enter as adults)
        Rg = Rg + growAlloc*params[["Rg"]]
      }
      
    }  ## end Store > Smax
    
  } ## end Store > Smin
  
  # Mortality  ----------
  if (X[["storage"]] <= params[["NSCthreshold"]] * Smax) {
    X[["stem_density"]] <- 0
  } else {
    mortRate <- (pest[["stem"]] + params[["mort1"]] * exp(-params[["mort2"]] * X[["storage"]] / Smax)) * timestep / 86400 / 365
    X[["som"]] <- X[["som"]] + X[["stem_density"]] * mortRate * sum(c(X[["leaf"]], X[["wood"]], X[["root"]], X[["storage"]])) / 1000 ## dead trees go to SOM
    X[["stem_density"]] <- X[["stem_density"]] * (1 - mortRate) ## reduce density but not per-tree pools
  }
  
  # Soil respiration  ----------
  Rh <- min(params[["Rbasal"]] * X[["som"]] * params[["Q10"]]^(inputs[["temp"]] / 10), X[["som"]] / (k * timestep)) ## min ensures SOM never goes negative
  X[["som"]] <- X[["som"]] - Rh * k * timestep
  
  # Format Output  ----------
  out <- c(X, GPP, fopen, Rleaf, Rstem + Rroot, Rg / ktree / timestep, LAI, Rh)
  names(out) <- NULL
  
  return(out)
  
}


#' Run the SEM 
#'
#' @param pest named vector for the pest impacts 
#' \describe{
#' \item{phloem}{phloem feaders}
#' \item{xylem}{xylem disrupters (bark beetle, canker, wilt, girdling)}
#' \item{leaf}{defoliators}
#' \item{root}{root rot}
#' \item{stem}{stem rot}
#' }
#' @param pest.time NULL or vector of the times to apply the pest disturbance to 
#' @param inputs named numeric vector containing the meteorological variables at a single time point
#' \describe{
#' \item{temp}{Air temperature, degrees C}
#' \item{precip}{Precipitation, mm}
#' \item{VPD}{Vapor pressure deficit, kPa}
#' \item{PAR}{Incoming photosynthetically active radiation, umol/m2/s}
#' \item{time}{as.POSIXct date fromat, must be every 30 min, TODO there needs to be some better way to handle the dates}
#' }
#' @param X named numeric vector containing the following
#' \describe{
#' \item{leaf}{kg/plant}
#' \item{wood}{kg/plant}
#' \item{root}{kg/plant}
#' \item{storage}{kg/plant}
#' \item{som}{soil organic matter, Mg/ha}
#' \item{soil_water}{m}
#' \item{stem_density}{stems/ha}
#' }
#' @param param_df dataframe containing the following
#' \describe{
#' \item{parameter}{character of the parameter name}
#' \item{value}{double parameter values}
#' }
#' @param DBH diameter at breast height default value set to 10
#' @param quiet boolean default set to TRUE if set to FALSE will print date
#' @return vector of results
#' @export
run_SEM <- function(pest, pest.time, inputs, X, param_df, DBH = 10, quiet = TRUE){
  
  # TODO 
  # Implement some sort of handle time colum of the inputs df.
  
  # Check all of inputs to the run SEM function.
  assert_that(check_SEM_run_setup(pest = pest,
                                  pest.time = pest.time,
                                  inputs = inputs,
                                  X = X,
                                  param_df = param_df, 
                                  DBH = DBH, 
                                  quiet = quiet))

  
  # Extract the parameter values into a vector 
  params <- list()
  params <- param_df$value
  names(params) <- param_df$parameter
  
  # Add to certain values to the parameters list, these are values that are based on SEM assumptions. 
  params[["Rleaf"]] <- 0.04 * params[["Vcmax"]] #Basal leaf respiration (umol/m2/s) is a fraction of the maximum carboxylation rate
  # TODO add other params such as Jmax? 
  
  # Extract the parameter values into a vector 
  params <- list()
  params <- param_df$value
  names(params) <- param_df$parameter
  
  # Add to certain values to the parameters list, these are values that are based on SEM assumptions. 
  params[["Rleaf"]] <- 0.04 * params[["Vcmax"]] #Basal leaf respiration (umol/m2/s) is a fraction of the maximum carboxylation rate
  # TODO add other params such as Jmax? 

  # Save a copy of the pest vector
  pest.orig <- pest
  pest <- c("phloem" = 0,  "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
  
  # Set up the data frame to store the output results in. 
  output <- array(NA, c(length(inputs$time), 14))
  
  
  for(t in inputs$time){
    ## turn pests on/off
    if(!is.null(pest.time)){
      if(t %in% pest.time){
        pest <- pest.orig
      } else {
        pest <- c("phloem" = 0,  "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0)
      }
    }
    
    # Index over the different meteorology
    index <- which(inputs$time == t)
    output[index, ] <- SEM(X = X, params = params, inputs = inputs[index, ], pest = pest)
    X <- output[index, 1:7]
    names(X) <-  c("leaf", "wood", "root", "storage", "som", "soil_water", "stem_density")
    
    # Check time
    date <- format(inputs$time[index], format = "%d")
    hms <- format(inputs$time[index], format = "%H:%M:%S")
    if (!quiet) if(date == "01" & hms == "00:00:00"){print(inputs$time[index])}
    
  } 
  
  # Format output
  # TODO decide between returning a long or wide data frame and adding a units column? 
  colnames(output) <- c("Bleaf", "Bwood", "Broot", "Bstore", "BSOM", "Water", 
                        "density", "GPP", "fopen", "Rleaf", "RstemRroot", 
                        "Rgrow", "LAI", "Rh")

  output <- cbind(time = inputs$time, data.frame(output))
  return(output)
  
  
}
