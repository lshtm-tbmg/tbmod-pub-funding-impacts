#-----------------------------------------------
# Generate Epi Output for Funding Shocks scenarios
# Author: Rebecca Clark
# Last updated: 14 April 2025
#-----------------------------------------------

# 1. Set-up: 

# Load in the required packages
suppressPackageStartupMessages({
  rm(list=ls())
  model = new.env()
  require(tbmoddev)
  library(here)
  library(data.table)
  library(arrow)
  library(getopt)
  source(here("R", "run_param_set_FundingShocks.R"))
})


# Set the country code, parameters, scenario characteristics 

if (T){ # testing on local machine
  cc <- "AFG"
  grid_task_int <- 1
  
} else{ # on HPC
  opts = getopt(matrix(c('cc','c', 1, "character"),
                       byrow=TRUE, ncol=4))
  cc <- opts$cc
  grid_task_int <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
}

countries <- fread("./processing_files/countries.csv")
HIV_status <- countries[CountryCode == cc]$HIV_status

parameters    <- fread(paste0("./processing_files/param_sets/", cc, "_params.csv"))
print(paste0("number of parameter sets to run = ", nrow(parameters)))

vx_scenarios <- fread("./processing_files/scenarios_FundingShocks.csv")

if (grid_task_int == 1){
  dir.create("./epi_output/")
  dir.create("./epi_output/n_epi/")
  dir.create(paste0("./epi_output/n_epi/", cc, "/"))
}

# 2. Generate the output
for (j in 1:nrow(parameters)) {
  
  print(paste0("parameter set = ", j))
  
  params     <- parameters[j, ]
  params_uid <- params[, uid]
  params     <- params[, !c("uid", "nhits")]
  params     <- unlist(params)
  
  print(vx_scenarios$runtype)

  cc_n_epi_param <- list()

  #for (i in 1:nrow(vx_scenarios)){
   for (i in c(1, 2, 23, 24)){ 
    vx_chars <- vx_scenarios[i,]
    
    print(paste0("Running scenario number ", i, ": ", vx_chars$runtype))
    
    # run the model with the row of parameters
    vx_scen_output <- run_param_set(cc, params, params_uid, vx_chars, HIV_status)
    
    cc_n_epi_param[[i]] <- vx_scen_output[["n_epi"]]
    
  }
  
  write_parquet(rbindlist(cc_n_epi_param), paste0("./epi_output/n_epi/", cc, "/", cc, "_", params_uid, ".parquet"))
  rm(cc_n_epi_param)
  
  print(paste0("End time for parameter set ", j, " = ", Sys.time()))
  
}


# ----end

