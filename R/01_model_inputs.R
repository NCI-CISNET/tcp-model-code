### CISNET Population Model
### Model of T21 policy for all US states
### Code by Rafael Meza modified by Lyss Crippen, Jamie Tam
### Use state_process_inputs.R to complete data processing and generate inputs for each state 

rm(list = ls()) 
setwd("/Users/ac3456/Dropbox/state_tcp_tool/LC_code/Darth_TCP_model/R/")

# Package names
packages <- c("ggplot2", "readxl", "readr", "dplyr", "tidyr", "reshape2","grid", 
              "ggpubr", "gridBase", "gridExtra", "cdlTools", "stringr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
lapply(packages, library, character.only = TRUE)

#---------------------- Load data sets --------------------------------------------------------
load("../data/T21policycoverage2003.2024.Rda") # T21 policy coverage data

load('../data/harmonized_tuscps_prevs.Rda') # TUSCPS survey data for model verification

#-------------------- Set model inputs -----------------------------------------------------------
date_variable <- format(Sys.Date(), "%m.%d.%y")
startbc <- 1908   # starting birth cohort 
endbc <- 2100     # ending birth cohort
endyear <- 2200   # final calendar year, extra 100 years needed for AC-AP conversion
policyyear=2005   # policy year, NA for T21 analysis
cohyears <- endbc-startbc+1     # number of cohort years

v_calyears <- (startbc-startbc+1):(endbc-startbc+1)  # index of calendar years 1908-2100
v_stdbirths<- rep(1000000, times = 193)    #fixed birthrate/population size for prevalence calculations
v_mla.effects= c(main=0.3391,upper=0.5266,lower=0.1517,baseline=0)  # main T21 policy effect estimate with upper, lower bounds

#tiers of policy coverage scenarios: local only, state&local, combined federal&state&local, and baseline (no coverage)
v_policy.scen= c("local","statelocal","fedstatelocal","baseline")

#-- run the model looping through all states -------------------------------------
source('02_main_analysis.R')

