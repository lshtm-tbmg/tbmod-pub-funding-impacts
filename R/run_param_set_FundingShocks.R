run_param_set <- function(cc, params, params_uid, vx_chars, HIV_status) {
  
  combined_ipj <- list()
  
  # set.paths = initialize the params
  model_paths <- set.paths(countrycode  = cc,
                           xml          = vx_chars$xml,
                           parameters   = vx_chars$input)
  
  if (grepl("baseline", vx_chars$runtype)){
    scen_baseline = NULL
  } else {
    scen_baseline = model$baseline_output 
  } 
  
  # Run the model with the parameter set
  output = run(model_paths, new.parameter.values = params,
               baseline = scen_baseline, output.flows = F)
  
  #### n_epi
  cc_counts <- output$stocks
  cc_counts <- cc_counts[age_from == 0 & age_thru == 99, ][, AgeGrp := "[0,99]"]
  cc_counts <- cc_counts[, !c("age_from", "age_thru")]
  cc_counts <- cc_counts[!(year %% 0.5 == 0),]
  
  #### Incidence
  inc <- cc_counts[TB == "sTBcount",]
  inc <- inc[, .(N_inc = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  
  #### New infections
  newinf <- cc_counts[TB == "UnIfcount",]
  newinf <- newinf[, .(N_newinf = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  
  #### Mortality
  mort <- cc_counts[TB == "TBdead",]
  mort <- mort[, .(N_mort = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  
  #### Treatment
  treat <- cc_counts[TB == "sTBTcount"]
  treat <- treat[, .(N_tx = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  
  treat_succ <- cc_counts[TB == "OTRcount"]
  treat_succ <- treat_succ[, .(N_tx_succ = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  
  treat_fail <- cc_counts[TB == "OTsTBcount"]
  treat_fail <- treat_fail[, .(N_tx_fail = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  
  treat_mort <- cc_counts[TB == "OTdcount"]
  treat_mort <- treat_mort[, .(N_tx_mort = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  
  #### Infectious disease prevalence
  prev <- cc_counts[TB == "aTB" | TB == "sTB"]
  prev <- prev[, .(N_prev = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  
  #### TB infection prevalence
  inf_prev <- cc_counts[TB == "Is" | TB == "If"]
  inf_prev <- inf_prev[, .(N_infprev = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  
  #### Symptomatic TB prevalence
  sTB_prev <- cc_counts[TB == "sTB"]
  sTB_prev <- sTB_prev[, .(N_sTBprev = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  
  #### Asymptomatic TB prevalence
  aTB_prev <- cc_counts[TB == "aTB"]
  aTB_prev <- aTB_prev[, .(N_aTBprev = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  
  #### Population size
  pop <- cc_counts[!(grepl("count", TB))]
  pop <- pop[!(grepl("dead", TB))]
  pop <- pop[!(grepl("dead", HIV))]
  pop <- pop[, .(N_pop = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
  
  
  
  if (HIV_status == "HIV"){
    hiv_prev <- cc_counts[HIV == "HIV1" | HIV == "ART1"]
    hiv_prev <- hiv_prev[!(grepl("count", TB))]
    hiv_prev <- hiv_prev[, .(N_HIVprev = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    
    art_prev <- cc_counts[HIV == "ART1"]
    art_prev <- art_prev[!(grepl("count", TB))]
    art_prev <- art_prev[, .(N_ARTprev = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    
    tbhiv_inc <- cc_counts[TB == "sTBcount" & (HIV == "HIV1" | HIV == "ART1")]
    tbhiv_inc <- tbhiv_inc[, .(N_tbhiv_inc = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    
    tbhiv_mort <- cc_counts[TB == "TBdead" & (HIV == "HIV1" | HIV == "ART1")]
    tbhiv_mort <- tbhiv_mort[, .(N_tbhiv_mort = sum(value)), by = .(Country = country, Year = year, AgeGrp)]
    
  }
  
  n_epi <- inc[mort, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  n_epi <- n_epi[newinf, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  n_epi <- n_epi[treat, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  n_epi <- n_epi[treat_succ, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  n_epi <- n_epi[treat_fail, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  n_epi <- n_epi[treat_mort, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  n_epi <- n_epi[prev, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  n_epi <- n_epi[inf_prev, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  n_epi <- n_epi[sTB_prev, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  n_epi <- n_epi[aTB_prev, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  n_epi <- n_epi[pop, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  
  if(HIV_status == "HIV"){
    n_epi <- n_epi[hiv_prev, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
    n_epi <- n_epi[art_prev, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
    n_epi <- n_epi[tbhiv_inc, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
    n_epi <- n_epi[tbhiv_mort, on = .(Country = Country, Year = Year, AgeGrp = AgeGrp)]
  }
  
  n_epi <- n_epi[, Year := floor(Year)]
  
  if (grepl("baseline", vx_chars$runtype)){
    model$baseline_output <- output
  }
  
  # Add the scenario characteristics
  combined_ipj[["n_epi"]] <- n_epi[, `:=`(uid     = params_uid,
                                          runtype = vx_chars$runtype)]
  
  rm(output)
  combined_ipj
}

