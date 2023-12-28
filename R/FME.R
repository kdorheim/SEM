
#' Global sensitivity ranges for SEM
#' 
#' Given a SEM configuration estimate the global effect of parameter sensitivity. 
#' Based on the sensRange function from the FME package. TODO figure out how to link this
#'
#' @param pars named numeric vector of the parameter values to be tested as part of the sensitivity analysis  
#' @param parRange data frame of the min/max parameter values
#' @param param_df data frame of the parameters that are being held constant
#' \describe{
#' \item{parameter}{character of the parameter name}
#' \item{value}{double parameter values}
#' }
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
#' @param DBH diameter at breast height default value set to 10 
#' @param pest default set to no disturbance otherwise is a named vector for the pest impacts 
#' @param pest.time NULL or vector of the times to apply the pest disturbance to 
#' @param ... additional arguments passed to FME::sensRange such as num or dist
#' @return results of the FME::sensRange
#' @export
#' @family sensitivity
SEM_sensrange <- function(pars, 
                          parRange, 
                          param_df,
                          inputs, 
                          X,
                          DBH,
                          pest = c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0), 
                          pest.time = NULL, ...){
  
  func <- function(pars){
    
    new_params_df <- internal_update_params(df = param_df, new = pars)
    return(run_SEM_internal(pest = pest, pest.time = pest.time, inputs = inputs,
                   X = pools, param_df = new_params_df, DBH = DBH))
  }
  
  return(FME::sensRange(func, parms = pars, parRange = parRange, ...))
}


#' Local sensitivity for SEM
#'
#' Estimate the local effect of a parameter on a SEM run model output
#' 
#' @param pars named numeric vector of the parameter values to be tested as part of the sensitivity analysis  
#' @param param_df data frame of the parameters that are being held constant
#' \describe{
#' \item{parameter}{character of the parameter name}
#' \item{value}{double parameter values}
#' }
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
#' @param DBH diameter at breast height default value set to 10 
#' @param pest default set to no disturbance otherwise is a named vector for the pest impacts 
#' @param pest.time NULL or vector of the times to apply the pest disturbance to 
#' @param ... additional arguments passed to FME::sensRange such as num or dist
#' @return results of the FME::sensRange
#' @export
#' @family sensitivity
SEM_sensfunc <- function(pars, 
                         param_df,
                         inputs, 
                         X,
                         DBH,
                         pest = c("phloem" = 0, "xylem" = 0, "leaf" = 0, "root" = 0, "stem" = 0), 
                         pest.time = NULL, ...){ 
  func <- function(pars){
    
    new_params_df <- internal_update_params(df = param_df, new = pars)
    return(run_SEM_internal(pest = pest, pest.time = pest.time, inputs = inputs,
                   X = pools, param_df = new_params_df, DBH = DBH))
  }
  
  return(FME::sensFun(func, parms = pars, ...))
}




#' Format the output returned by memc_sensrange or memc_sensfunc into long data format for easy plotting
#'
#' @param obj object returned by memc_sensrange
#' @return the a long dataframe of the summary memc_sensrange
#' @export
#' @family sensitivity
format_sensout <- function(obj){
  
  cond <- any(class(obj)[[1]] %in% c("sensRange", "sensFun"))
  assert_that(cond)
  
  if(class(obj)[[1]] == "sensRange"){
    out <- summary(obj)
    names(out)[1] <- "time"
    vars <- gsub(pattern = "\\.|[[:digit:]]+", replacement = "", x = row.names(out))
    out$variable <- vars
    row.names(out) <- NULL
  }
  
  if(class(obj)[[1]] == "sensFun"){
    
    out <- data.table::as.data.table(obj)
    names(out)[1] <- "time"
    names(out)[2] <- "variable"
    params <- names(out)[3:ncol(out)]
    
    out <- data.table::melt(out, id.vars = c("time", "variable"),
                            value.name = "value", variable.name = "parameter")
    
  }
  
  return(out)
}

















