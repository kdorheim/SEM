


#' Arrhenius formula for temperature dependence of reaction rates 
#'
#' @param observed.value value in deg C
#' @param new.temp value in deg C
#' @param old.temp default value of 25
#' @return numeric value 
arrhenius <- function(observed.value, new.temp, old.temp = 25) {
  
  new.temp <- convert_temp_C_K(new.temp)
  old.temp <- convert_temp_C_K(old.temp)

  out <- observed.value / exp(3000 * (1 / new.temp - 1 / old.temp))
  
  return(out)
}
