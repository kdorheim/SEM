# Generate/format the example input data, this only needs to be run intermittently. 
# TODO decide of this ends up in the final release. 
devtools::load_all()

# The ncdf is from https://github.com/mdietze/PestED/blob/master/AMF_USMe2_2005_L2_GF_V006.nc 
example_inputs <- convert_nc_SEM("./data-raw/AMF_USMe2_2005_L2_GF_V006.nc")
time <- seq(as.POSIXct("2005-01-01 00:00"), as.POSIXct("2005-12-31 23:30"), by = 1800)
time_time <- gsub(pattern = "-| |:|EST", replacement = "", x = time)
index <- which(nchar(time_time) == 8)
time_time[index] <- paste0(time_time[index], "000000")
time <- substr(x = time_time, start = 1, stop = 12)
example_inputs$time <- time

write.csv(example_inputs, "./inst/metdata/example_inputs.csv", row.names = FALSE)

