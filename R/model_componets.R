#' Arrhenius formula for temperature dependence of reaction rates
#'
#' @param observed.value value in deg C
#' @param new.temp value in deg C
#' @param old.temp default value of 25
#' @return numeric value
#' @noRd
arrhenius <- function(observed.value, new.temp, old.temp = 25) {
  
  new.temp <- convert_temp_C_K(new.temp)
  old.temp <- convert_temp_C_K(old.temp)
  
  out <- observed.value / exp(3000 * (1 / new.temp - 1 / old.temp))
  
  return(out)
}


#' Ballberry function
#'
#' @details This actually the Medlyn et al 2011 model as used in Dietze,
#' Michael C., and Jaclyn Hatala Matthes. 2014. “A General
#' Ecophysiological Framework for Modelling the Impact of Pests and
#' Pathogens on Forest Ecosystems.” Ecology Letters 17 (11): 1418–26.
#'
#' @param input vector length 2 unkown what these things are supposed to be gah
#' @param BBparams vector of two parameters
#' \describe{
#'  \item{g0}{Cuticular conductance}
#'  \item{m}{Stomatal slope}
#'  }
#' @param Fparams vector of the \link{farquhar} parameters
#' \describe{
#'  \item{alpha}{Quantum yield}
#'  \item{Jmax}{TBD}
#'  \item{Gstar}{TBD}
#'  \item{Vcmax}{Maximum carboxylation rate (umol/m2/s)}
#'  \item{Rleaf}{Basal leaf respiration (umol/m2/s)}
#'  }
#' @param obs vector of observations at one time step
#' \describe{
#'  \item{Ca}{Atmospheric CO2 concentrations}
#'  \item{VPD}{Vapor pressure deficit, (kPa)}
#'  \item{PAR}{Incoming photosynthetically active radiation, (umol/m2/s)}
#'  }
#' @return numeric value  TBD
#' @noRd
ballberry <- function(input, BBparams, Fparams, obs) {
  
  assert_that(check_contents(req = c("Ca", "VPD"), check = names(obs)),
              msg = "obs")
  assert_that(check_contents(req = c("g0", "m"), check = names(BBparams)),
              msg = "BBparams")
  
  ## is actually the Medlyn et al 2011 model
  Ci <- obs[["Ca"]] - 1.6 * input[1] / input[2]
  e1 <- (farquhar(Ci = Ci, Fparams = Fparams, I = obs[["PAR"]]) - input[1])
  e2 <- (BBparams[["g0"]] + BBparams[["m"]] * input[1] / ( (obs[["Ca"]] - Fparams[["Gstar"]]) *
                                                          (1 + obs[["VPD"]])) - input[2]) * 100  
  out <- e1^2 + e2^2
  names(out) <- NULL
  
  return(out)
}


#' Farquhar-Ball Berry Optimization Functions 
#'
#' @details
#' Leaf-level photosynthesis was modelled using a standard enzyme-kinetic approach as used in 
#' Dietze, Michael C., and Jaclyn Hatala Matthes. 2014. “A General Ecophysiological Framework 
#' for Modelling the Impact of Pests and Pathogens on Forest Ecosystems.” Ecology Letters 17 (11): 1418–26.
#' TODO what is up with the km should is be read in as a parameter?
#'
#' @param Ci TBD
#' @param Fparams vector of the five farquhar parameters 
#' \describe{
#'  \item{alpha}{Quantum yield}
#'  \item{Jmax}{TBD}
#'  \item{Gstar}{TBD}
#'  \item{Vcmax}{Maximum carboxylation rate (umol/m2/s)}
#'  \item{Rleaf}{Basal leaf respiration (umol/m2/s)}
#'  \item{Km}{TBD}
#'  }
#' @param I TBD
#' @return numeric value TBD what this value is!
#' @noRd
farquhar <- function(Ci, Fparams, I){
  
  req <- c("alpha", "Jmax", "Gstar", "Vcmax", "Rleaf", "Km")
  assert_that(check_contents(req, check = names(Fparams)))
  
  a <- 0.9 ## curvature parameter
  b <- -(Fparams[["alpha"]] * I + Fparams[["Jmax"]])
  c <- Fparams[["alpha"]] * I * Fparams[["Jmax"]]
  J <- (-b-sqrt(b^2-4*a*c))/(2*a)
  
  aJ <- J*(Ci-Fparams[["Gstar"]])/(4*Ci+8*Fparams[["Gstar"]])    ## electron transport limited without covariates
  aC <- Fparams[["Vcmax"]]*(Ci-Fparams[["Gstar"]])/(Ci + Fparams[["Km"]])  
  
  out <- min(aJ, aC) - Fparams[["Rleaf"]]
  return(out)
}


#' TBD I have no idea what this function is actually doing! hopefully it will come to light 
#'
#' @details
#' Dietze, Michael C., and Jaclyn Hatala Matthes. 2014. “A General Ecophysiological Framework 
#' for Modelling the Impact of Pests and Pathogens on Forest Ecosystems.” Ecology Letters 17 (11): 1418–26.
#'
#' @param Vcmax numeric, Maximum carboxylation rate (umol/m2/s)
#' @param Jmax numeric, TBD
#' @param Rleaf numeric, Basal leaf respiration (umol/m2/s)
#' @param Gstar numeric, TBD
#' @param alpha numeric, Quantum yield
#' @param m numeric, Stomatal slope
#' @param g0 numeric, Cuticular conductance
#' @param VPD numeric, Vapor pressure deficit, (kPa)
#' @param PAR numeric, Incoming photosynthetically active radiation, (umol/m2/s)
#' @param Km numeric, TBD
#' @param Ca atmospheric CO2 concentrations with a default value of 400 TBD if should vary 
#' @param inital_guess numeric vector length 2 TBD what this info is but default values of c(15, 0.1)
#' @return numeric vector length 2
#' @noRd
#' @importFrom stats optim
solve.FVcB <- function(Vcmax, Jmax, Rleaf, Gstar, alpha, m, g0, VPD, PAR, Km, Ca = 400, inital_guess = c(15, 0.1)){
  
  # Format the parameters into the input vectors for the farquhar and ballberry functions. 
  # Note that an error will be thrown if km is not globally defined. 
  BBparams <- c(g0=g0, m=m)
  Fparams <- c(Vcmax=Vcmax, Jmax=Jmax, Rleaf=Rleaf, Gstar=Gstar, alpha=alpha, Km=Km)
  obs <- c(Ca=Ca, VPD=VPD, PAR=PAR)
  
  fit <- optim(inital_guess,			  # solve simultaneously for An.pred and gs.pred
               ballberry,
               BBparams = BBparams,	# Ballberry params                                                                                                                                                                                                                                                                                                            
               Fparams = Fparams,	  # Farquhar params
               obs = obs)  		
  
  if(fit$par[2] >= 0){
    out <- fit$par
  } else { 
    out <- c(0, 0)
  }
  return(out)
}
