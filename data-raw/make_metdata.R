# Generate/format the example input data, this only needs to be run intermittently. 
# TODO decide of this ends up in the final release. 
devtools::load_all()

# The ncdf is from https://github.com/mdietze/PestED/blob/master/AMF_USMe2_2005_L2_GF_V006.nc 
example_inputs <- convert_nc_SEM("./data-raw/AMF_USMe2_2005_L2_GF_V006.nc")
example_inputs[["time"]] <- seq(as.POSIXct("2005-01-01 00:00"), as.POSIXct("2005-12-31 23:30"), by = 1800)

as.Date(example_inputs$time, format = "%d/%m/%Y %h%m")




as.character(example_inputs$time)

write.csv(example_inputs, "./inst/metdata/example_inputs.csv", row.names = FALSE)

thetimes = chron(dates=dtparts[,1],times=dtparts[,2],
                 +                  format=c('y-m-d','h:m:s'))


chron()


as.character(as.POSIXlt(x = example_inputs$time))
