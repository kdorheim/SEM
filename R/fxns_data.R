# Some helper functions that work to prep and check data for SEM format. 


#' Convert a netcdf file into a data frame that can be used by SEM
#'
#'
#' @param nc_path character path to the .nc file of met data, this file must contain PAR, temp, VPD, and precip dagta
#' @return a data frame consisting of columns of the following:
#' \describe{
#' \item{time}{the day of the year}
#' \item{PAR}{incoming photosynthetically active radiation, umol/m2/s}
#' \item{temp}{air temperature, degrees C}
#' \item{VPD}{vapor pressure deficit, kPa}
#' \item{precip}{precipitation, mm}
#' }
#' @importFrom assertthat assert_that
#' @import ncdf4
#' @export
convert_nc_SEM <- function(nc_path){
  # Quality check the nc before extracting the data required by SEM. 
  assert_that(file.exists(nc_path))
  nc <- nc_open(nc_path)
  assert_that(class(nc) == "ncdf4")
  
  req_vars <- c("PAR", "TA", "VPD", "PREC", "DOY")
  assert_that(check_contents(req = req_vars, check = names(nc$var)))
  
  # Extract the required data, replacing NA data when appropriate. 
  PAR <- ncvar_get(nc, "PAR")
  for (i in which(PAR < -10)) {
    PAR[i] <- PAR[i - 1]
  } ## uber-naive gapfilling
  temp <- ncvar_get(nc, "TA")
  VPD <- ncvar_get(nc, "VPD")
  precip <- ncvar_get(nc, "PREC")
  time <- ncvar_get(nc, "DOY")
  nc_close(nc)
  
  # Format into a single data frame
  out <- data.frame(time = time, PAR = PAR, temp = temp, 
                    VPD = VPD, precip = precip)
  return(out)
}


#' Convert a netcdf file into a data frame that can be used by SEM
#'
#'
#' @param temp numeric in degrees C to be converted into degrees K
#' @return numeric vector of the temperature in K 
#' @noRd
convert_temp_C_K <- function(temp){
  out <- temp + 273.15
  return(out)
}



