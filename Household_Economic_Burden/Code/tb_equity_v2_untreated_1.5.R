index<-as.integer(commandArgs(trailingOnly=TRUE)[1])
tbequityv2 <- function(index){

## Load packages
library(dplyr)
library(tidyr)
library(data.table)
library(arrow)
library(magrittr)
library(stringr)
library(reshape2)

## Create lists of countries
countries <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/countries_included.csv",header=TRUE,check.names=FALSE)[,2]
base <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/countries_included.csv",header=TRUE,check.names=FALSE)
  
## Specify discount rate & analytic years
r <- 0.03 #discount rate
disc_year <- 2020 #first year of vaccine introduction
start_year <- 2025 #first year of analytic results
end_year <- 2050
N <- 200 #number of parameter sets
untreated_mult <- 1.5

## Load equity inputs
ses <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/ses_targets.csv",header=TRUE,check.names=FALSE)
care <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/careseeking_pred.csv",header=TRUE,check.names=FALSE)
prop <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/ses_proportions.csv",header=TRUE,check.names=FALSE)

## Convert equity inputs to long format
ses_long <- pivot_longer(data=ses,cols=3:7,names_to="quintile",values_to="ses")
care_long <- pivot_longer(data=care,cols=5:9,names_to="quintile",values_to="careseeking")
prop_long <- pivot_longer(data=prop,cols=3:7,names_to="quintile",values_to="prop")

## Load cost uncertainty sets

poorest_pred_dm <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/cost_dm_poorest_pred_sets.csv",header=TRUE,check.names=FALSE)
poorest_pred_nm <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/cost_nm_poorest_pred_sets.csv",header=TRUE,check.names=FALSE)
poorest_pred_ind <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/cost_ind_poorest_pred_sets.csv",header=TRUE,check.names=FALSE)

poorer_pred_dm <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/cost_dm_poorer_pred_sets.csv",header=TRUE,check.names=FALSE)
poorer_pred_nm <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/cost_nm_poorer_pred_sets.csv",header=TRUE,check.names=FALSE)
poorer_pred_ind <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/cost_ind_poorer_pred_sets.csv",header=TRUE,check.names=FALSE)

middle_pred_dm <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/cost_dm_middle_pred_sets.csv",header=TRUE,check.names=FALSE)
middle_pred_nm <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/cost_nm_middle_pred_sets.csv",header=TRUE,check.names=FALSE)
middle_pred_ind <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/cost_ind_middle_pred_sets.csv",header=TRUE,check.names=FALSE)

richer_pred_dm <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/cost_dm_richer_pred_sets.csv",header=TRUE,check.names=FALSE)
richer_pred_nm <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/cost_nm_richer_pred_sets.csv",header=TRUE,check.names=FALSE)
richer_pred_ind <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/cost_ind_richer_pred_sets.csv",header=TRUE,check.names=FALSE)

richest_pred_dm <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/cost_dm_richest_pred_sets.csv",header=TRUE,check.names=FALSE)
richest_pred_nm <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/cost_nm_richest_pred_sets.csv",header=TRUE,check.names=FALSE)
richest_pred_ind <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/cost_ind_richest_pred_sets.csv",header=TRUE,check.names=FALSE)

## Load catastrophic cost proportion uncertainty sets
cc_all <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/cc_all_1.5.csv",header=TRUE,check.names=FALSE)
oop_all <- read.csv("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/data/oop_all_1.5.csv",header=TRUE,check.names=FALSE)

# outermost loop = country

i <- countries[index]
  
  # Initialize dataframe
  output_year <- as.data.frame(seq(start_year,end_year,1))
  colnames(output_year) <- "year"
  output_all <- NULL
  
  ## Grab list of UIDs
  directory <- paste0("/n/netscratch/menzies_lab/Lab/VPM/tbmod/econ_output/")
  
  UID_list <- list.files(path = paste0(directory,"cc_TB/",i,"_TB/"))
  UID_list <- tools::file_path_sans_ext(UID_list)
  
  # Set econ parameter set counter to zero
  
  counter <- 0
  
# second loop: uid

  for (j0 in 1:length(UID_list)) {
    
    j <- UID_list[j0]
    
    ## Load data
    cty <- read_parquet(paste0(directory,"cc_TB/",i,"_TB/",j,".parquet")) %>% collect()
    base_cty <- base[which(base$code==i),]
    
    ## Subset equity inputs to country
    ses_long_cty <- ses_long[which(ses_long$code==i),]
    care_long_cty <- care_long[which(care_long$code==i),]
    prop_long_cty <- prop_long[which(prop_long$code==i),]
    
    ## Subset costs/income to specified country
    poorest_dm_cty <- poorest_pred_dm[which(poorest_pred_dm$iso3==i),]
    poorest_nm_cty <- poorest_pred_nm[which(poorest_pred_nm$iso3==i),]
    poorest_ind_cty <- poorest_pred_ind[which(poorest_pred_ind$iso3==i),]
 
    poorer_dm_cty <- poorer_pred_dm[which(poorer_pred_dm$iso3==i),]
    poorer_nm_cty <- poorer_pred_nm[which(poorer_pred_nm$iso3==i),]
    poorer_ind_cty <- poorer_pred_ind[which(poorer_pred_ind$iso3==i),]

    middle_dm_cty <- middle_pred_dm[which(middle_pred_dm$iso3==i),]
    middle_nm_cty <- middle_pred_nm[which(middle_pred_nm$iso3==i),]
    middle_ind_cty <- middle_pred_ind[which(middle_pred_ind$iso3==i),]
   
    richer_dm_cty <- richer_pred_dm[which(richer_pred_dm$iso3==i),]
    richer_nm_cty <- richer_pred_nm[which(richer_pred_nm$iso3==i),]
    richer_ind_cty <- richer_pred_ind[which(richer_pred_ind$iso3==i),]
   
    richest_dm_cty <- richest_pred_dm[which(richest_pred_dm$iso3==i),]
    richest_nm_cty <- richest_pred_nm[which(richest_pred_nm$iso3==i),]
    richest_ind_cty <- richest_pred_ind[which(richest_pred_ind$iso3==i),]
    
    ## Subset cc/oop to specified country
    cc_cty <- cc_all[which(cc_all$code==i),]
    oop_cty <- oop_all[which(oop_all$code==i),]

    ## Reformat variables
    # The following backs out numeric values for the first age shown (Age) in AgeGrp, 
    # regexpr() gives the ctyex of given characters, 
    # and substr() extracts a piece of the character vector. 
    
    cty$Age <- as.numeric(substr(cty$AgeGrp,2,regexpr(",",cty$AgeGrp)-1))
    
    # Grab list of delivery scenarios
    
    scenarios <- unique(cty$Runtype)
    
    # Increase econ parameter set counter
    
    counter <- counter + 1
    
# third loop: delivery scenario
# calc all results then separate step for summary statistics
    
    for (k0 in 1:length(scenarios)) {
      
      k <- scenarios[k0]
      
      inc_scen <- cty[which(cty$Runtype==k&cty$Year>=start_year&cty$Year<=end_year),]
      
      ## Aggregate population by scenario
      output_year$pop <- aggregate(inc_scen$Population~inc_scen$Year,FUN=sum)[,2]
      
      ## Aggregate cases by scenario
      cases <- aggregate(inc_scen$Total_Inc~inc_scen$Year,FUN=sum)[,2]

      ## Aggregate treated cases by scenario
      notif <- aggregate(inc_scen$Total_Notif~inc_scen$Year,FUN=sum)[,2]

      ## Calculate untreated cases by scenario
      untreated <- cases - notif

      ## Extrapolate cases by two SES strata to five income quintiles by SES prevalence rate ratio
      output_year$cases_richest <- cases*(ses_long_cty$ses[5]/(ses_long_cty$ses[5]+ses_long_cty$ses[4]+ses_long_cty$ses[3]+ses_long_cty$ses[2]+ses_long_cty$ses[1]))*1000
      output_year$cases_richer <- cases*(ses_long_cty$ses[4]/(ses_long_cty$ses[5]+ses_long_cty$ses[4]+ses_long_cty$ses[3]+ses_long_cty$ses[2]+ses_long_cty$ses[1]))*1000
      output_year$cases_middle <- cases*(ses_long_cty$ses[3]/(ses_long_cty$ses[5]+ses_long_cty$ses[4]+ses_long_cty$ses[3]+ses_long_cty$ses[2]+ses_long_cty$ses[1]))*1000
      output_year$cases_poorer <- cases*(ses_long_cty$ses[2]/(ses_long_cty$ses[5]+ses_long_cty$ses[4]+ses_long_cty$ses[3]+ses_long_cty$ses[2]+ses_long_cty$ses[1]))*1000
      output_year$cases_poorest <- cases*(ses_long_cty$ses[1]/(ses_long_cty$ses[5]+ses_long_cty$ses[4]+ses_long_cty$ses[3]+ses_long_cty$ses[2]+ses_long_cty$ses[1]))*1000
      
      ## Estimate p(treatment)
      p_tx <- notif / cases

      ## Extrapolate treated by two SES strata to five income quintiles by SES prevalence rate ratio and DHS care-seeking
      output_year$notif_richest <- cases*p_tx*((care_long_cty$careseeking[5]*output_year$cases_richest)/(care_long_cty$careseeking[5]*output_year$cases_richest+care_long_cty$careseeking[4]*output_year$cases_richer+care_long_cty$careseeking[3]*output_year$cases_middle+care_long_cty$careseeking[2]*output_year$cases_poorer+care_long_cty$careseeking[1]*output_year$cases_poorest))*1000
      output_year$notif_richer <- cases*p_tx*((care_long_cty$careseeking[4]*output_year$cases_richer)/(care_long_cty$careseeking[5]*output_year$cases_richest+care_long_cty$careseeking[4]*output_year$cases_richer+care_long_cty$careseeking[3]*output_year$cases_middle+care_long_cty$careseeking[2]*output_year$cases_poorer+care_long_cty$careseeking[1]*output_year$cases_poorest))*1000
      output_year$notif_middle <- cases*p_tx*((care_long_cty$careseeking[3]*output_year$cases_middle)/(care_long_cty$careseeking[5]*output_year$cases_richest+care_long_cty$careseeking[4]*output_year$cases_richer+care_long_cty$careseeking[3]*output_year$cases_middle+care_long_cty$careseeking[2]*output_year$cases_poorer+care_long_cty$careseeking[1]*output_year$cases_poorest))*1000
      output_year$notif_poorer <- cases*p_tx*((care_long_cty$careseeking[2]*output_year$cases_poorer)/(care_long_cty$careseeking[5]*output_year$cases_richest+care_long_cty$careseeking[4]*output_year$cases_richer+care_long_cty$careseeking[3]*output_year$cases_middle+care_long_cty$careseeking[2]*output_year$cases_poorer+care_long_cty$careseeking[1]*output_year$cases_poorest))*1000
      output_year$notif_poorest <- cases*p_tx*((care_long_cty$careseeking[1]*output_year$cases_poorest)/(care_long_cty$careseeking[5]*output_year$cases_richest+care_long_cty$careseeking[4]*output_year$cases_richer+care_long_cty$careseeking[3]*output_year$cases_middle+care_long_cty$careseeking[2]*output_year$cases_poorer+care_long_cty$careseeking[1]*output_year$cases_poorest))*1000
      
      ## Constrain so treated/notif is always less than or equal to total TB cases 
      output_year$notif_richest <- pmin(output_year$cases_richest,output_year$notif_richest)
      output_year$notif_richer <- pmin(output_year$cases_richer,output_year$notif_richer)
      output_year$notif_middle <- pmin(output_year$cases_middle,output_year$notif_middle)
      output_year$notif_poorer <- pmin(output_year$cases_poorer,output_year$notif_poorer)
      output_year$notif_poorest <- pmin(output_year$cases_poorest,output_year$notif_poorest)
      
      ## Calculate untreated by quintile
      output_year$untreated_richest <- output_year$cases_richest - output_year$notif_richest
      output_year$untreated_richer <- output_year$cases_richer - output_year$notif_richer
      output_year$untreated_middle <- output_year$cases_middle - output_year$notif_middle
      output_year$untreated_poorer <- output_year$cases_poorer - output_year$notif_poorer
      output_year$untreated_poorest <- output_year$cases_poorest - output_year$notif_poorest
      
      ## Adjust out-of-pocket costs from TB quintiles in PCS to population quintiles
      v1 <- c(prop_long_cty$prop[1],prop_long_cty$prop[2],prop_long_cty$prop[3],prop_long_cty$prop[4],prop_long_cty$prop[5])
      id <- round(quantile(0:1e5,c(0,cumsum(v1))))
      
      # direct medical costs
      dm1 <- c(poorest_dm_cty[,counter+4],poorer_dm_cty[,counter+4],middle_dm_cty[,counter+4],richer_dm_cty[,counter+4],richest_dm_cty[,counter+4])
      dm2 <- rep(NA,5)
      v2 <- rep(dm1,each=1e5/5)
      for(l in 1:5) dm2[l] <- mean(v2[id[l]:id[l+1]])
      
      # direct non-medical costs
      nm1 <- c(poorest_nm_cty[,counter+4],poorer_nm_cty[,counter+4],middle_nm_cty[,counter+4],richer_nm_cty[,counter+4],richest_nm_cty[,counter+4])
      nm2 <- rep(NA,5)
      v3 <- rep(nm1,each=1e5/5)
      for(l in 1:5) nm2[l] <- mean(v3[id[l]:id[l+1]])
      
      # indirect costs
      ind1 <- c(poorest_ind_cty[,counter+4],poorer_ind_cty[,counter+4],middle_ind_cty[,counter+4],richer_ind_cty[,counter+4],richest_ind_cty[,counter+4])
      ind2 <- rep(NA,5)
      v4 <- rep(ind1,each=1e5/5)
      for(l in 1:5) ind2[l] <- mean(v4[id[l]:id[l+1]])
      
      ## Calculate out-of-pocket costs (direct medical only) by scenario and income quintile among treated cases
      output_year$oop_treated_richest_und <- dm2[5] * output_year$notif_richest
      output_year$oop_treated_richer_und <- dm2[4] * output_year$notif_richer
      output_year$oop_treated_middle_und <- dm2[3] * output_year$notif_middle
      output_year$oop_treated_poorer_und <- dm2[2] * output_year$notif_poorer
      output_year$oop_treated_poorest_und <- dm2[1] * output_year$notif_poorest
      
      ## Calculate direct non-medical costs by scenario and income quintile among treated cases
      output_year$nm_treated_richest_und <- nm2[5] * output_year$notif_richest
      output_year$nm_treated_richer_und <- nm2[4] * output_year$notif_richer
      output_year$nm_treated_middle_und <- nm2[3] * output_year$notif_middle
      output_year$nm_treated_poorer_und <- nm2[2] * output_year$notif_poorer
      output_year$nm_treated_poorest_und <- nm2[1] * output_year$notif_poorest
      
      ## Calculate indirect costs by scenario and income quintile among treated cases
      output_year$ind_treated_richest_und <- ind2[5] * output_year$notif_richest
      output_year$ind_treated_richer_und <- ind2[4] * output_year$notif_richer
      output_year$ind_treated_middle_und <- ind2[3] * output_year$notif_middle
      output_year$ind_treated_poorer_und <- ind2[2] * output_year$notif_poorer
      output_year$ind_treated_poorest_und <- ind2[1] * output_year$notif_poorest
      
      ## Calculate total costs (direct medical, direct non-medical, indirect) by scenario and income quintile among treated cases
      output_year$total_treated_richest_und <- (dm2[5]+nm2[5]+ind2[5]) * output_year$notif_richest
      output_year$total_treated_richer_und <- (dm2[4]+nm2[4]+ind2[4]) * output_year$notif_richer
      output_year$total_treated_middle_und <- (dm2[3]+nm2[3]+ind2[3]) * output_year$notif_middle
      output_year$total_treated_poorer_und <- (dm2[2]+nm2[2]+ind2[2]) * output_year$notif_poorer
      output_year$total_treated_poorest_und <- (dm2[1]+nm2[1]+ind2[1]) * output_year$notif_poorest
      
      ## Calculate out-of-pocket costs (direct medical only) by scenario and income quintile among untreated cases
      output_year$oop_untreated_richest_und <- dm2[5] * untreated_mult * output_year$untreated_richest
      output_year$oop_untreated_richer_und <- dm2[4] * untreated_mult * output_year$untreated_richer
      output_year$oop_untreated_middle_und <- dm2[3] * untreated_mult * output_year$untreated_middle
      output_year$oop_untreated_poorer_und <- dm2[2] * untreated_mult * output_year$untreated_poorer
      output_year$oop_untreated_poorest_und <- dm2[1] * untreated_mult * output_year$untreated_poorest
      
      ## Calculate direct non-medical costs by scenario and income quintile among untreated cases
      output_year$nm_untreated_richest_und <- nm2[5] * untreated_mult * output_year$untreated_richest
      output_year$nm_untreated_richer_und <- nm2[4] * untreated_mult * output_year$untreated_richer
      output_year$nm_untreated_middle_und <- nm2[3] * untreated_mult * output_year$untreated_middle
      output_year$nm_untreated_poorer_und <- nm2[2] * untreated_mult * output_year$untreated_poorer
      output_year$nm_untreated_poorest_und <- nm2[1] * untreated_mult * output_year$untreated_poorest
      
      ## Calculate indirect costs by scenario and income quintile among untreated cases
      output_year$ind_untreated_richest_und <- ind2[5] * untreated_mult * output_year$untreated_richest
      output_year$ind_untreated_richer_und <- ind2[4] * untreated_mult * output_year$untreated_richer
      output_year$ind_untreated_middle_und <- ind2[3] * untreated_mult * output_year$untreated_middle
      output_year$ind_untreated_poorer_und <- ind2[2] * untreated_mult * output_year$untreated_poorer
      output_year$ind_untreated_poorest_und <- ind2[1] * untreated_mult * output_year$untreated_poorest
      
      ## Calculate total costs (direct medical, direct non-medical, indirect) by scenario and income quintile among untreated cases
      output_year$total_untreated_richest_und <- ((dm2[5]+nm2[5]+ind2[5])* untreated_mult) * output_year$untreated_richest
      output_year$total_untreated_richer_und <- ((dm2[4]+nm2[4]+ind2[4])* untreated_mult) * output_year$untreated_richer
      output_year$total_untreated_middle_und <- ((dm2[3]+nm2[3]+ind2[3])* untreated_mult) * output_year$untreated_middle
      output_year$total_untreated_poorer_und <- ((dm2[2]+nm2[2]+ind2[2])* untreated_mult) * output_year$untreated_poorer
      output_year$total_untreated_poorest_und <- ((dm2[1]+nm2[1]+ind2[1])* untreated_mult) * output_year$untreated_poorest
      
      ## Calculate out-of-pocket costs (direct medical only) by scenario and income quintile
      output_year$oop_richest_und <- output_year$oop_treated_richest_und + output_year$oop_untreated_richest_und
      output_year$oop_richer_und <- output_year$oop_treated_richer_und + output_year$oop_untreated_richer_und
      output_year$oop_middle_und <- output_year$oop_treated_middle_und + output_year$oop_untreated_middle_und
      output_year$oop_poorer_und <- output_year$oop_treated_poorer_und + output_year$oop_untreated_poorer_und
      output_year$oop_poorest_und <- output_year$oop_treated_poorest_und + output_year$oop_untreated_poorest_und
      
      ## Calculate direct non-medical costs by scenario and income quintile
      output_year$nm_richest_und <- output_year$nm_treated_richest_und + output_year$nm_untreated_richest_und
      output_year$nm_richer_und <- output_year$nm_treated_richer_und + output_year$nm_untreated_richer_und
      output_year$nm_middle_und <- output_year$nm_treated_middle_und + output_year$nm_untreated_middle_und
      output_year$nm_poorer_und <- output_year$nm_treated_poorer_und + output_year$nm_untreated_poorer_und
      output_year$nm_poorest_und <- output_year$nm_treated_poorest_und + output_year$nm_untreated_poorest_und
      
      ## Calculate indirect costs by scenario and income quintile
      output_year$ind_richest_und <- output_year$ind_treated_richest_und + output_year$ind_untreated_richest_und
      output_year$ind_richer_und <- output_year$ind_treated_richer_und + output_year$ind_untreated_richer_und
      output_year$ind_middle_und <- output_year$ind_treated_middle_und + output_year$ind_untreated_middle_und
      output_year$ind_poorer_und <- output_year$ind_treated_poorer_und + output_year$ind_untreated_poorer_und
      output_year$ind_poorest_und <- output_year$ind_treated_poorest_und + output_year$ind_untreated_poorest_und
      
      ## Calculate total costs (direct medical, direct non-medical, indirect) by scenario and income quintile
      output_year$total_richest_und <- output_year$total_treated_richest_und + output_year$total_untreated_richest_und
      output_year$total_richer_und <- output_year$total_treated_richer_und + output_year$total_untreated_richer_und
      output_year$total_middle_und <- output_year$total_treated_middle_und + output_year$total_untreated_middle_und
      output_year$total_poorer_und <- output_year$total_treated_poorer_und + output_year$total_untreated_poorer_und
      output_year$total_poorest_und <- output_year$total_treated_poorest_und + output_year$total_untreated_poorest_und
      
      ## Estimate discounted and undiscounted OOP costs
      for (m in 1:nrow(output_year)) {
        
        ## Treated cases
        
        if (output_year$year[m]<disc_year) {
          output_year[m,"oop_treated_richest_d"] <- output_year[m,"oop_treated_richest_und"]
        } else {
          output_year[m,"oop_treated_richest_d"] <- output_year[m,"oop_treated_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"oop_treated_richer_d"] <- output_year[m,"oop_treated_richer_und"]
        } else {
          output_year[m,"oop_treated_richer_d"] <- output_year[m,"oop_treated_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"oop_treated_middle_d"] <- output_year[m,"oop_treated_middle_und"]
        } else {
          output_year[m,"oop_treated_middle_d"] <- output_year[m,"oop_treated_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"oop_treated_poorer_d"] <- output_year[m,"oop_treated_poorer_und"]
        } else {
          output_year[m,"oop_treated_poorer_d"] <- output_year[m,"oop_treated_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"oop_treated_poorest_d"] <- output_year[m,"oop_treated_poorest_und"]
        } else {
          output_year[m,"oop_treated_poorest_d"] <- output_year[m,"oop_treated_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"nm_treated_richest_d"] <- output_year[m,"nm_treated_richest_und"]
        } else {
          output_year[m,"nm_treated_richest_d"] <- output_year[m,"nm_treated_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"nm_treated_richer_d"] <- output_year[m,"nm_treated_richer_und"]
        } else {
          output_year[m,"nm_treated_richer_d"] <- output_year[m,"nm_treated_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"nm_treated_middle_d"] <- output_year[m,"nm_treated_middle_und"]
        } else {
          output_year[m,"nm_treated_middle_d"] <- output_year[m,"nm_treated_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"nm_treated_poorer_d"] <- output_year[m,"nm_treated_poorer_und"]
        } else {
          output_year[m,"nm_treated_poorer_d"] <- output_year[m,"nm_treated_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"nm_treated_poorest_d"] <- output_year[m,"nm_treated_poorest_und"]
        } else {
          output_year[m,"nm_treated_poorest_d"] <- output_year[m,"nm_treated_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"ind_treated_richest_d"] <- output_year[m,"ind_treated_richest_und"]
        } else {
          output_year[m,"ind_treated_richest_d"] <- output_year[m,"ind_treated_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"ind_treated_richer_d"] <- output_year[m,"ind_treated_richer_und"]
        } else {
          output_year[m,"ind_treated_richer_d"] <- output_year[m,"ind_treated_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"ind_treated_middle_d"] <- output_year[m,"ind_treated_middle_und"]
        } else {
          output_year[m,"ind_treated_middle_d"] <- output_year[m,"ind_treated_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"ind_treated_poorer_d"] <- output_year[m,"ind_treated_poorer_und"]
        } else {
          output_year[m,"ind_treated_poorer_d"] <- output_year[m,"ind_treated_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"ind_treated_poorest_d"] <- output_year[m,"ind_treated_poorest_und"]
        } else {
          output_year[m,"ind_treated_poorest_d"] <- output_year[m,"ind_treated_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"total_treated_richest_d"] <- output_year[m,"total_treated_richest_und"]
        } else {
          output_year[m,"total_treated_richest_d"] <- output_year[m,"total_treated_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"total_treated_richer_d"] <- output_year[m,"total_treated_richer_und"]
        } else {
          output_year[m,"total_treated_richer_d"] <- output_year[m,"total_treated_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"total_treated_middle_d"] <- output_year[m,"total_treated_middle_und"]
        } else {
          output_year[m,"total_treated_middle_d"] <- output_year[m,"total_treated_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"total_treated_poorer_d"] <- output_year[m,"total_treated_poorer_und"]
        } else {
          output_year[m,"total_treated_poorer_d"] <- output_year[m,"total_treated_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"total_treated_poorest_d"] <- output_year[m,"total_treated_poorest_und"]
        } else {
          output_year[m,"total_treated_poorest_d"] <- output_year[m,"total_treated_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        
        ## Untreated cases
        
        if (output_year$year[m]<disc_year) {
          output_year[m,"oop_untreated_richest_d"] <- output_year[m,"oop_untreated_richest_und"]
        } else {
          output_year[m,"oop_untreated_richest_d"] <- output_year[m,"oop_untreated_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"oop_untreated_richer_d"] <- output_year[m,"oop_untreated_richer_und"]
        } else {
          output_year[m,"oop_untreated_richer_d"] <- output_year[m,"oop_untreated_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"oop_untreated_middle_d"] <- output_year[m,"oop_untreated_middle_und"]
        } else {
          output_year[m,"oop_untreated_middle_d"] <- output_year[m,"oop_untreated_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"oop_untreated_poorer_d"] <- output_year[m,"oop_untreated_poorer_und"]
        } else {
          output_year[m,"oop_untreated_poorer_d"] <- output_year[m,"oop_untreated_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"oop_untreated_poorest_d"] <- output_year[m,"oop_untreated_poorest_und"]
        } else {
          output_year[m,"oop_untreated_poorest_d"] <- output_year[m,"oop_untreated_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"nm_untreated_richest_d"] <- output_year[m,"nm_untreated_richest_und"]
        } else {
          output_year[m,"nm_untreated_richest_d"] <- output_year[m,"nm_untreated_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"nm_untreated_richer_d"] <- output_year[m,"nm_untreated_richer_und"]
        } else {
          output_year[m,"nm_untreated_richer_d"] <- output_year[m,"nm_untreated_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"nm_untreated_middle_d"] <- output_year[m,"nm_untreated_middle_und"]
        } else {
          output_year[m,"nm_untreated_middle_d"] <- output_year[m,"nm_untreated_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"nm_untreated_poorer_d"] <- output_year[m,"nm_untreated_poorer_und"]
        } else {
          output_year[m,"nm_untreated_poorer_d"] <- output_year[m,"nm_untreated_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"nm_untreated_poorest_d"] <- output_year[m,"nm_untreated_poorest_und"]
        } else {
          output_year[m,"nm_untreated_poorest_d"] <- output_year[m,"nm_untreated_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"ind_untreated_richest_d"] <- output_year[m,"ind_untreated_richest_und"]
        } else {
          output_year[m,"ind_untreated_richest_d"] <- output_year[m,"ind_untreated_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"ind_untreated_richer_d"] <- output_year[m,"ind_untreated_richer_und"]
        } else {
          output_year[m,"ind_untreated_richer_d"] <- output_year[m,"ind_untreated_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"ind_untreated_middle_d"] <- output_year[m,"ind_untreated_middle_und"]
        } else {
          output_year[m,"ind_untreated_middle_d"] <- output_year[m,"ind_untreated_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"ind_untreated_poorer_d"] <- output_year[m,"ind_untreated_poorer_und"]
        } else {
          output_year[m,"ind_untreated_poorer_d"] <- output_year[m,"ind_untreated_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"ind_untreated_poorest_d"] <- output_year[m,"ind_untreated_poorest_und"]
        } else {
          output_year[m,"ind_untreated_poorest_d"] <- output_year[m,"ind_untreated_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"total_untreated_richest_d"] <- output_year[m,"total_untreated_richest_und"]
        } else {
          output_year[m,"total_untreated_richest_d"] <- output_year[m,"total_untreated_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"total_untreated_richer_d"] <- output_year[m,"total_untreated_richer_und"]
        } else {
          output_year[m,"total_untreated_richer_d"] <- output_year[m,"total_untreated_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"total_untreated_middle_d"] <- output_year[m,"total_untreated_middle_und"]
        } else {
          output_year[m,"total_untreated_middle_d"] <- output_year[m,"total_untreated_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"total_untreated_poorer_d"] <- output_year[m,"total_untreated_poorer_und"]
        } else {
          output_year[m,"total_untreated_poorer_d"] <- output_year[m,"total_untreated_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"total_untreated_poorest_d"] <- output_year[m,"total_untreated_poorest_und"]
        } else {
          output_year[m,"total_untreated_poorest_d"] <- output_year[m,"total_untreated_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        
        ## Overall cases
        
        if (output_year$year[m]<disc_year) {
          output_year[m,"oop_richest_d"] <- output_year[m,"oop_richest_und"]
        } else {
          output_year[m,"oop_richest_d"] <- output_year[m,"oop_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"oop_richer_d"] <- output_year[m,"oop_richer_und"]
        } else {
          output_year[m,"oop_richer_d"] <- output_year[m,"oop_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"oop_middle_d"] <- output_year[m,"oop_middle_und"]
        } else {
          output_year[m,"oop_middle_d"] <- output_year[m,"oop_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"oop_poorer_d"] <- output_year[m,"oop_poorer_und"]
        } else {
          output_year[m,"oop_poorer_d"] <- output_year[m,"oop_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"oop_poorest_d"] <- output_year[m,"oop_poorest_und"]
        } else {
          output_year[m,"oop_poorest_d"] <- output_year[m,"oop_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"nm_richest_d"] <- output_year[m,"nm_richest_und"]
        } else {
          output_year[m,"nm_richest_d"] <- output_year[m,"nm_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"nm_richer_d"] <- output_year[m,"nm_richer_und"]
        } else {
          output_year[m,"nm_richer_d"] <- output_year[m,"nm_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"nm_middle_d"] <- output_year[m,"nm_middle_und"]
        } else {
          output_year[m,"nm_middle_d"] <- output_year[m,"nm_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"nm_poorer_d"] <- output_year[m,"nm_poorer_und"]
        } else {
          output_year[m,"nm_poorer_d"] <- output_year[m,"nm_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"nm_poorest_d"] <- output_year[m,"nm_poorest_und"]
        } else {
          output_year[m,"nm_poorest_d"] <- output_year[m,"nm_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"ind_richest_d"] <- output_year[m,"ind_richest_und"]
        } else {
          output_year[m,"ind_richest_d"] <- output_year[m,"ind_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"ind_richer_d"] <- output_year[m,"ind_richer_und"]
        } else {
          output_year[m,"ind_richer_d"] <- output_year[m,"ind_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"ind_middle_d"] <- output_year[m,"ind_middle_und"]
        } else {
          output_year[m,"ind_middle_d"] <- output_year[m,"ind_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"ind_poorer_d"] <- output_year[m,"ind_poorer_und"]
        } else {
          output_year[m,"ind_poorer_d"] <- output_year[m,"ind_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"ind_poorest_d"] <- output_year[m,"ind_poorest_und"]
        } else {
          output_year[m,"ind_poorest_d"] <- output_year[m,"ind_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"total_richest_d"] <- output_year[m,"total_richest_und"]
        } else {
          output_year[m,"total_richest_d"] <- output_year[m,"total_richest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"total_richer_d"] <- output_year[m,"total_richer_und"]
        } else {
          output_year[m,"total_richer_d"] <- output_year[m,"total_richer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"total_middle_d"] <- output_year[m,"total_middle_und"]
        } else {
          output_year[m,"total_middle_d"] <- output_year[m,"total_middle_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"total_poorer_d"] <- output_year[m,"total_poorer_und"]
        } else {
          output_year[m,"total_poorer_d"] <- output_year[m,"total_poorer_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
        if (output_year$year[m]<disc_year) {
          output_year[m,"total_poorest_d"] <- output_year[m,"total_poorest_und"]
        } else {
          output_year[m,"total_poorest_d"] <- output_year[m,"total_poorest_und"] / (1+r)^(output_year$year[m]-disc_year)
        }
      }
      
      ################################################################################
      ### PERCENTAGE OF HOUSEHOLDS EXPERIENCING CATASTROPHIC COSTS
      ################################################################################
      
      ## Adjust CC/OOP proportions from TB quintiles in PCS to population quintiles
      # % experiencing catastrophic costs
      cc1 <- c(cc_cty[counter,"cc_poorest"],cc_cty[counter,"cc_poorer"],cc_cty[counter,"cc_middle"],cc_cty[counter,"cc_richer"],cc_cty[counter,"cc_richest"])
      cc2 <- rep(NA,5)
      v5 <- rep(cc1,each=1e5/5)
      for(l in 1:5) cc2[l] <- mean(v5[id[l]:id[l+1]])
      
      # % experiencing catastrophic direct medical costs
      oop1 <- c(oop_cty[counter,"oop_poorest"],oop_cty[counter,"oop_poorer"],oop_cty[counter,"oop_middle"],oop_cty[counter,"oop_richer"],oop_cty[counter,"oop_richest"])
      oop2 <- rep(NA,5)
      v6 <- rep(oop1,each=1e5/5)
      for(l in 1:5) oop2[l] <- mean(v6[id[l]:id[l+1]])
      
      ## Assess catastrophic costs among treated cases
      output_year$cc_treated_richest <- cc2[5]*output_year$notif_richest
      output_year$cc_treated_richer <- cc2[4]*output_year$notif_richer
      output_year$cc_treated_middle <- cc2[3]*output_year$notif_middle
      output_year$cc_treated_poorer <- cc2[2]*output_year$notif_poorer
      output_year$cc_treated_poorest <- cc2[1]*output_year$notif_poorest
      
      output_year$oop_treated_richest <- oop2[5]*output_year$notif_richest
      output_year$oop_treated_richer <- oop2[4]*output_year$notif_richer
      output_year$oop_treated_middle <- oop2[3]*output_year$notif_middle
      output_year$oop_treated_poorer <- oop2[2]*output_year$notif_poorer
      output_year$oop_treated_poorest <- oop2[1]*output_year$notif_poorest
      
      ## Assess catastrophic costs among untreated cases
      output_year$cc_untreated_richest <- cc2[5]*output_year$untreated_richest
      output_year$cc_untreated_richer <- cc2[4]*output_year$untreated_richer
      output_year$cc_untreated_middle <- cc2[3]*output_year$untreated_middle
      output_year$cc_untreated_poorer <- cc2[2]*output_year$untreated_poorer
      output_year$cc_untreated_poorest <- cc2[1]*output_year$untreated_poorest
      
      output_year$oop_untreated_richest <- oop2[5]*output_year$untreated_richest
      output_year$oop_untreated_richer <- oop2[4]*output_year$untreated_richer
      output_year$oop_untreated_middle <- oop2[3]*output_year$untreated_middle
      output_year$oop_untreated_poorer <- oop2[2]*output_year$untreated_poorer
      output_year$oop_untreated_poorest <- oop2[1]*output_year$untreated_poorest
      
      ## Assess catastrophic costs among all cases
      output_year$cc_richest <- output_year$cc_treated_richest + output_year$cc_untreated_richest
      output_year$cc_richer <- output_year$cc_treated_richer + output_year$cc_untreated_richer
      output_year$cc_middle <- output_year$cc_treated_middle + output_year$cc_untreated_middle
      output_year$cc_poorer <- output_year$cc_treated_poorer + output_year$cc_untreated_poorer
      output_year$cc_poorest <- output_year$cc_treated_poorest + output_year$cc_untreated_poorest
      
      output_year$oop_richest <- output_year$oop_treated_richest + output_year$oop_untreated_richest
      output_year$oop_richer <- output_year$oop_treated_richer + output_year$oop_untreated_richer
      output_year$oop_middle <- output_year$oop_treated_middle + output_year$oop_untreated_middle
      output_year$oop_poorer <- output_year$oop_treated_poorer + output_year$oop_untreated_poorer
      output_year$oop_poorest <- output_year$oop_treated_poorest + output_year$oop_untreated_poorest
      
      ## Collate by country, UID, delivery scenario, coverage scenario
      output_names <- colnames(output_year)
      output <- as.data.frame(cbind(unique(base_cty$country),i,unique(base_cty$region),unique(base_cty$income),j,k,output_year))      
      colnames(output) <- c("country","code","region","income","UID","scenario",output_names)
   
      ## Append to overall output dataframe
      output_all <- rbind(output_all,output)
      
    }

  }

  ## Save output for each country
  write.csv(output_all,paste0("/n/netscratch/menzies_lab/Lab/Allison/EquityV2/output/",i,"_equity_untreated_1.5.csv"),row.names=FALSE)
  rm(output)
  
}
tbequityv2(index)
