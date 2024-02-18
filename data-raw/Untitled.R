parameter     value
allomL0     0.0546394

exp(-2.907)



Lmax = params$allomL0*DBH^params$allomL1        ## set maximum leaf biomass from DBH
Lmin = params$Lmin*Lmax                         ## set minimum leaf and root biomass as a fraction of maximum

DBH <- 10
allomL0_MD <-  params_df[params_df$parameter == "allomL0", ][["value"]][[1]]
allomL1_MD <- params_df[params_df$parameter == "allomL1", ][["value"]][[1]]
Lmax_MD <- allomL0_MD * DBH ^ allomL1_MD; Lmax_MD
Lmin_MD <- Lmax_MD *  params_df[params_df$parameter == "Lmin", ][["value"]][[1]]; Lmin_MD

params_df[params_df$parameter == "allomB1", ][["value"]][[1]]

allomL0_UMBS <- exp(-3.862)
allomL1_UMBS <- 1.74
Lmax_UMBS <- allomL0_UMBS * DBH ^ allomL1_UMBS; Lmax_UMBS
Lmin_UMBS <- Lmax_UMBS *  params_df[params_df$parameter == "Lmin", ][["value"]][[1]]; Lmin_UMBS

# i think... that is analogus to where get from in the Jenkins.. but I am not sure how using these will affect things... 
B0 exp(-2.4800)
B1 2.4835