# Define a handful of functions used internally in the SEM package.

#' Check a vector for required contents
#'
#' This is an internal package function that is helpful to make sure
#' the conditions are met.
#'
#' @param req vector of the required contents
#' @param check vector that must contain the required contents
#' @return TRUE if the all required elements are within the check vector otherwise an error is thrown.
#' @noRd
check_contents <- function(req, check){
  
  assert_that(is.vector(req))
  assert_that(is.vector(check))
  missing <- setdiff(req, check)
  
  if (length(missing) == 0) {
    return(TRUE)
  } else {
    stop("following required contents not found: ",
         paste0(missing, collapse = ", "))
  }
}


#' Check all the inputs for the \code{run_SEM} function.  
#'
#' This is an internal package function that is helpful to make sure
#' the conditions are met.
#'
#' @param pest named vector for the pest impacts 
#' @param pest.time NULL or vector of the times to apply the pest disturbance to 
#' @param inputs named numeric vector containing the meteorological variables at a single time point
#' @param X named numeric vector of the carbon pools
#' @param param_df data frame containing the model parameters
#' @param DBH diameter at breast height default
#' @param quiet boolean 
#' @return Boolean to indicate if all the tests pass, otherwise an error will be thrown.
#' @noRd
check_SEM_run_setup <- function(pest, pest.time, inputs, X, param_df, DBH, quiet){
  
  # Check the function arguments
  assert_that(check_contents(req = c("phloem", "xylem", "leaf", "root", "stem"), check = names(pest)))
  assert_that(check_contents(req = c("PAR", "temp", "VPD", "precip", "time"), check = names(inputs)))
  # Check to make sure that the time column is in YYYYMMDDHHMM format with 30 min resolution, 
  # this is not the most robust way of handeling dates or fool proof for checking for 30 mins. 
  assert_that(all(nchar(inputs$time) == 12), msg = "time column requires yyyymmddhhmm format")
  assert_that(min(diff(inputs$time)) == 30, msg = "data should be half hourly")
    assert_that(is.data.frame(param_df))
  assert_that(check_contents(req = c("value", "parameter"), check = names(param_df)))
  assert_that(is.numeric(DBH), msg = "DBH must be numeric")
  
  # Check the disturbance set up 
  if(!is.null(pest.time)){
    assert_that(all(pest.time %in% inputs$time), msg = "pest.time must NULL or be an element of inputs$time")
    assert_that(sum(pest) > 0, msg = "no pest treatment defined")
  }
  if(is.null(pest.time)){
    assert_that(sum(pest) == 0, msg = "if pest.time is NULL all elements of pest must be set to 0")
  }

  # TODO not all of the parameters might have to be read in each time, some might be fixed, talk to LH.
  req_params <- c("gevap", "Wthresh", "Kroot", "SLA", "alpha", "Vcmax", "m", "g0", "allomB0",
                  "allomB1", "allomL0", "allomL1", "Rroot", "Rstem", "Rg", "Q10", "Rbasal", "leafLitter",
                  "CWD", "rootLitter", "mort1", "mort2", "NSCthreshold", "Lmin", "q", "StoreMinDay", "Smax",
                  "Rfrac","SeedlingMort", "Kleaf")  
  assert_that(check_contents(req = req_params, check = param_df$parameter))
  
  return(TRUE)
}


#' Update parameter values defined in the \code{params_df} 
#'
#'
#' @param df data frame of the SEM parameter values 
#' @param new named numeric vector of the parameter values to be updated
#' @return SEM paramter data frame with the new parameter values
#' @export
update_params <- function(df, new){
  
  # Note this check might change if the number of tune able parameters being 
  # read into the SEM change for whatever reason. 
  assert_that(all(dim(df) == list(33, 2)))
  assert_that(check_contents(req = c("parameter", "value"), check = names(df)))
  assert_that(is.vector(new) & is.numeric(new) & !is.null(names(new)))
  assert_that(all(names(new) %in% df$parameter))
  
  to_keep <- df[! df$parameter %in% names(new), ]
  out <- rbind(to_keep, 
               data.frame(parameter = names(new), value = new, row.names = NULL))
  return(out)
}


#' Parse out the year information from yyyymmddhhmm
#'
#' @param x numeric vector of time yyyymmddhhmm
#' @return year value
#' @noRd
get_year <- function(x){
  assert_that( all(nchar(x) == 12), msg = "x must be a character with lenght 12 aka YYYYMMDDHHMM")
  out <- substr(x = x, start = 1, stop = 4)
  return(out)
  
}


#' Parse out the month information from yyyymmddhhmm
#'
#' @param x numeric vector of time yyyymmddhhmm
#' @return month value
#' @noRd
get_month <- function(x){
  assert_that( all(nchar(x) == 12), msg = "x must be a character with lenght 12 aka YYYYMMDDHHMM")
  out <- substr(x = x, start = 5, stop = 6)
  return(out)
}


#' Parse out the day information from yyyymmddhhmm
#'
#' @param x numeric vector of time yyyymmddhhmm
#' @return day value
#' @noRd
get_day <- function(x){
  assert_that( all(nchar(x) == 12), msg = "x must be a character with lenght 12 aka YYYYMMDDHHMM")
  out <- substr(x = x, start = 7, stop = 8)
  return(out)
}


#' Parse out the hour information from yyyymmddhhmm
#'
#' @param x numeric vector of time yyyymmddhhmm
#' @return hour value
#' @noRd
get_hr <- function(x){
  assert_that( all(nchar(x) == 12), msg = "x must be a character with lenght 12 aka YYYYMMDDHHMM")
  out <- substr(x = x, start = 9, stop = 10)
  return(out)
}


#' Parse out the minute information from yyyymmddhhmm
#'
#' @param x numeric vector of time yyyymmddhhmm
#' @return minute value
#' @noRd
get_min <- function(x){
  assert_that( all(nchar(x) == 12), msg = "x must be a character with lenght 12 aka YYYYMMDDHHMM")
  out <- substr(x = x, start = 11, stop = 12)
  return(out)
}


#' Load and format the SEM meteorological input file 
#'
#' This function helps handle the time column which can be affected by being 
#' saved as a csv file. 
#'
#' @param f character pathway of the meteorological file to read into R file should have the following contents
#' \describe{
#' \item{time}{date & time column YYYYMMDDHHMM}
#' \item{temp}{Air temperature, degrees C}
#' \item{precip}{Precipitation, mm}
#' \item{VPD}{Vapor pressure deficit, kPa}
#' \item{PAR}{Incoming photosynthetically active radiation, umol/m2/s}
#' }
#' @importFrom utils read.csv
#' @return SEM paramter data frame with the new parameter values
#' @export
#' @examples
#' \donttest{
#' # Using the regular read.csv function can cause the problems with the time column. 
#' metdata <- read.csv(system.file("metdata/example_inputs.csv", package = "SEM"))
#' head(metdata$time)
#' class(metdata$time)
#' 
#' # Where as using the read_SEM_met function returns a the time column with the 
#' # correct date and time information. 
#' metdata <- read_SEM_met(system.file("metdata/example_inputs.csv", package = "SEM"))
#' head(metdata$time)
#' class(metdata$time)
#' }
read_SEM_met <- function(f){
  assert_that(file.exists(f))
  d <- read.csv(f)
  check_contents(req = c("time", "PAR", "temp", "VPD",  "precip"), 
                 check = names(d))
  
  # Check to make sure that the time column is in YYYYMMDDHHMM format with 30 min resolution, 
  # this is not the most robust way of handeling dates or fool proof for checking for 30 mins. 
  assert_that(all(nchar(d$time) == 12), msg = "time column requires YYYYMMDDHHMM format")

  return(d)
}

