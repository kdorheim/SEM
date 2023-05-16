# Generate/format the input data, this only needs to be run intermittently to generate 
# example data. 
devtools::load_all()

example_inputs <- convert_nc_SEM("./data-raw/met_data.nc")
example_inputs[["time"]] <- seq(as.POSIXct("2009-01-01 00:00"), as.POSIXct("2009-12-31 23:30"), by = 1800)

# TBD if want to ship with example data
write.csv(example_inputs, "inst/metdata/example_inputs.csv", row.names = FALSE)
row <- floor(nrow(example_inputs) * 0.25)
write.csv(example_inputs[1:row, ], "tests/testthat/inputs.csv", row.names = FALSE)
