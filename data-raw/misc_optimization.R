library(profvis)
devtools::load_all("/Users/dorh012/Documents/2023/FoRTEII/SEM")


BASE_DIR <- 
MET_DIR <- here::here("Ameriflux_WD", "ameriflux_data", "SEM-inputs")
WRITE_TO <- here::here("Ameriflux_WD", "ameriflux_data", "rslts", "dev")
dir.create(WRITE_TO, showWarnings = FALSE, recursive = TRUE)
f <- list.files(MET_DIR, full.names = TRUE)[1]


profvis({SEMout <- run_SEM(pest = pest,
                           pest.time = pest.time,
                           inputs = metdata,
                           X = pools,              # SEM::pools
                           param_df = params_df)})


# Profvis time before doing minimal optimization 89760 , it brought it down to 3070