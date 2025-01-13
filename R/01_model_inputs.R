# Package names
packages <- c("dplyr","ggplot2","ggplot2","ggpubr")# "readxl", "dplyr", "tidyr", "reshape2", "grid", "gridBase", "gridExtra", "stringr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
lapply(packages, library, character.only = TRUE)

# Load country-specific data ----------------------------------------------

# census populations, smoking parameters, mortality, life expectancy by smoking status, birth cohort, calendar year
load(paste0("data/p.mort_01.RData")) # mortality
load(paste0("data/smk_01.RData")) # smoking init/cess cast as AC = age-cohort
load(paste0("data/pop_01.RData")) # census pop
load(paste0("data/le_01.RData")) # life expectacies

# smoking survey data for model verification
load('data/harmonized_tuscps_prevs.Rda') 
df_survey_data <- subset(tuscps_prevs, fips==1)

# Set model inputs --------------------------------------------------------

v_policy.ages <- c(18:20) # ages affected by the policy
date_variable <- format(Sys.Date(), "%m.%d.%y")
startbc <- 1908   # starting birth cohort 
endbc <- 2100     # ending birth cohort
endyear <- 2200   # final calendar year, extra 100 years needed for AC-AP conversion
policyyear <- 2025   # policy year
policycohort <- 2005 # first birth cohort affected by T21 in the year 2025 is 2005
cohyears <- endbc-startbc+1     # number of cohort years
v_calyears <- (startbc-startbc+1):(endbc-startbc+1)  # index of calendar years 1908-2100
v_stdbirths<- rep(1000000, times = 193)    #fixed birthrate/population size for prevalence calculations
v_mla.effects <- c(main=0.3391,upper=0.5266,lower=0.1517,baseline=0)  # main T21 policy effect estimate with upper, lower bounds
