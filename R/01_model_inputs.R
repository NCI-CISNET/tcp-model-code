### CISNET Population Model
### Model of T21 policy for all US states
### Code by Rafael Meza modified by Lyss Crippen, Jamie Tam
### Use state_process_inputs.R to complete data processing and generate inputs for each state 

rm(list = ls()) 
setwd("/Users/ac3456/Dropbox/state_tcp_tool/LC_code/TCP_model/")

# Package names
packages <- c("ggplot2", "readxl", "readr", "dplyr", "tidyr", "reshape2","grid", 
              "ggpubr", "gridBase", "gridExtra", "cdlTools", "stringr", "ggrepel")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
lapply(packages, library, character.only = TRUE)

#---------------------- Load data sets --------------------------------------------------------
load("data/T21policycoverage2005.2025.Rda") # T21 policy coverage data

load('data/harmonized_tuscps_prevs.Rda') # TUSCPS survey data for model verification

#-------------------- Set model inputs -----------------------------------------------------------
##### Scenario indicators 
##### policy_decay -> 0/1 default is standard policy effects (no exponential decay) =0 
##### make_tcp_out -> 0/1 default is not to make TCP tool files =0
policy_decay <- 0
make_tcp_out <- 0


#other model inputs

v_policy.ages <- c(18:20) # ages affected by the policy
date_variable <- format(Sys.Date(), "%m.%d.%y")
startbc <- 1908   # starting birth cohort 
endbc <- 2100     # ending birth cohort
endyear <- 2200   # final calendar year, extra 100 years needed for AC-AP conversion
policycohort <- 1985 # first birth cohort affected by T21 in the year 2005 is 1985
policyyear=2005   # policy year, NA for T21 analysis
cohyears <- endbc-startbc+1     # number of cohort years
v_calyears <- (startbc-startbc+1):(endbc-startbc+1)  # index of calendar years 1908-2100
v_stdbirths<- rep(1000000, times = 193)    #fixed birthrate/population size for prevalence calculations
v_mla.effects= c(main=0.3391,upper=0.5266,lower=0.1517,baseline=0)  # main T21 policy effect estimate with upper, lower bounds

#tiers of policy coverage scenarios: local only, state&local, combined federal&state&local, and baseline (no coverage)
v_policy.scen= c("local","statelocal","fedstatelocal","baseline")

# vector of states to run simulations for (50 states + DC)
v_statefips <- c('01','02','04','05','06','08','09','10','11','12','13','15','16',
                 '17','18','19','20','21','22','23','24','25','26','27','28','29',
                 '30','31','32','33','34','35','36','37','38','39','40','41','42',
                 '44','45','46','47','48','49','50','51','53','54','55','56')

# vector of states to generate main manuscript figures for
#paperfips=c('06','21','25','55') # CA, KY, MA, WI 

#-- run the model looping through all states -------------------------------------
source('R/02_model_functions.R')
source('R/03_main_analysis.R')
source('R/04_visualization.R')
