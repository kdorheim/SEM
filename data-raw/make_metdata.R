# Generate/format the input data, this only needs to be run intermittently to generate 
# example data. 
devtools::load_all()

example_inputs <- convert_nc_SEM("./data-raw/met_data.nc")
write.csv(example_inputs, "inst/metdata/example_inputs.csv", row.names = FALSE)