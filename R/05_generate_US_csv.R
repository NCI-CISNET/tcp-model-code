# Load data, change data and paths as needed 


dir <- "/Users/wangmengyao/Documents/GitHub/tcp-model-code/source_data" 
load('output/model_output_03.20.25.RData') #### whatever dataset you want to load



v_statefips <- c('01','02','04','05','06','08','09','10','11','12','13','15','16',
                 '17','18','19','20','21','22','23','24','25','26','27','28','29',
                 '30','31','32','33','34','35','36','37','38','39','40','41','42',
                 '44','45','46','47','48','49','50','51','53','54','55','56')



#tiers of policy coverage scenarios: local only, state&local, combined federal&state&local, and baseline (no coverage)

date_variable <- format(Sys.Date(), "%m.%d.%y")
startbc <- 1908   # starting birth cohort 
endbc <- 2100     # ending birth cohort
endyear <- 2200   # final calendar year, extra 100 years needed for AC-AP conversion
policyyear=2005   # policy year, NA for T21 analysis
cohyears <- endbc-startbc+1     # number of cohort years



#---------- mortality outputs ----------------------------------------------------

# Set output directories for deaths and LYG
us_deaths_dir <- file.path(dir, "US", "t21", "deaths")
us_lyg_dir <- file.path(dir, "US", "t21", "lyg")
us_res_dir <- file.path(dir, "US", "t21", "results")

# Ensure the output directories exist
if (!dir.exists(us_deaths_dir)) {
  dir.create(us_deaths_dir, recursive = TRUE)
}

if (!dir.exists(us_lyg_dir)) {
  dir.create(us_lyg_dir, recursive = TRUE)
}

if (!dir.exists(us_res_dir)) {
  dir.create(us_res_dir, recursive = TRUE)
}  

# List of all state directories (excluding the 'US' folder)
state_dirs <- list.dirs(dir, full.names = TRUE, recursive = FALSE)
state_dirs <- state_dirs[basename(state_dirs) != "US"]

# Initialize empty lists to store summed data for deaths and cLYG
combined_deaths <- list()
combined_cLYG <- list()

# Loop through each state directory
for (state_dir in state_dirs) {
  
  # Process deaths data
  deaths_dir <- file.path(state_dir, "t21", "deaths")
  
  if (dir.exists(deaths_dir)) {
    # Get the list of death CSV files
    death_files <- list.files(deaths_dir, pattern = "\\.csv$", full.names = TRUE)
    
    for (file in death_files) {
      state_data <- read.csv(file)
      file_name <- basename(file)
      
      
      
      # Initialize or sum deaths data
      if (!file_name %in% names(combined_deaths)) {
        combined_deaths[[file_name]] <- state_data
      } else {
        combined_deaths[[file_name]]$deaths_avoided_males <- combined_deaths[[file_name]]$deaths_avoided_males + state_data$deaths_avoided_males
        combined_deaths[[file_name]]$deaths_avoided_females <- combined_deaths[[file_name]]$deaths_avoided_females + state_data$deaths_avoided_females
      }
    }
  }
  
  # Process cLYG data
  lyg_dir <- file.path(state_dir, "t21", "lyg")
  
  if (dir.exists(lyg_dir)) {
    # Get the list of cLYG CSV files
    clyg_files <- list.files(lyg_dir, pattern = "\\.csv$", full.names = TRUE)
    
    for (file in clyg_files) {
      state_data <- read.csv(file)
      file_name <- basename(file)
      
      # Initialize or sum cLYG data
      if (!file_name %in% names(combined_cLYG)) {
        combined_cLYG[[file_name]] <- state_data
      } else {
        combined_cLYG[[file_name]]$cLYG_males <- combined_cLYG[[file_name]]$cLYG_males + state_data$cLYG_males
        combined_cLYG[[file_name]]$cLYG_females <- combined_cLYG[[file_name]]$cLYG_females + state_data$cLYG_females
      }
    }
  }
}

# Write the combined deaths data to the US/deaths directory
for (file_name in names(combined_deaths)) {
  write.csv(combined_deaths[[file_name]], file = file.path(us_deaths_dir, file_name), row.names = FALSE)
}

# Write the combined cLYG data to the US/t21/lyg directory
for (file_name in names(combined_cLYG)) {
  write.csv(combined_cLYG[[file_name]], file = file.path(us_lyg_dir, file_name), row.names = FALSE)
}



#--------- prevalence outputs --------------------------------------------------

v_cohorts <- c(1990,2000,2010,2020,2030)
v_agecohorts <- pmin(99,2100-v_cohorts)

agegroups <- c('12-17', '18-24', '25-44', '45-64', '65p', '18-99')
agel <- c(12, 18, 25, 45, 65, 18)
ageu <- c(17, 24, 44, 64, 99, 99)
# 
df_allstates_res <- NULL
# function to generate TCP files
calculate_allstate_prev <- function(l_mla, l_baseline, fipscode, policy.scen, mla.effect) {
  # Extract policy values
  m_M_popAP <- l_mla[['m_M_popAP']]
  m_F_popAP <- l_mla[['m_F_popAP']]
  m_M_smokersAP <- l_mla[['m_M_smokers']]
  m_F_smokersAP <- l_mla[['m_F_smokers']]
  
  m_M_basepopAP <- l_baseline[['m_M_popAP']]
  m_F_basepopAP <- l_baseline[['m_F_popAP']]
  m_M_basesmokeAP <- l_baseline[['m_M_smokers']]
  m_F_basesmokeAP <- l_baseline[['m_F_smokers']]
  #v_cohorts <- 2005
  
  # Initialize df_allstates_res as an empty data frame
  df_allstates_res <- data.frame(
    year = integer(),
    age = integer(),
    coh = integer(),
    malesmoke = numeric(),
    femsmoke = numeric(),
    malesmokebase = numeric(),
    femsmokebase = numeric(),
    malepop = numeric(),
    fempop = numeric(),
    malepopbase = numeric(),
    fempopbase = numeric(),
    fipscode = integer(),
    policy.scen = character(),
    mla.effect = numeric(),
    agegroup = character(),
    stringsAsFactors = FALSE
  )
  
  for (year in 2005:2100) {
    for (coh in v_cohorts) {
      age <- year - coh
      
      # Skip iterations where age is outside the valid range
      if (age < 0 | age >= 100) next
      
      if (age < 100) {
        for (kag in 1:6) {
        # Calculate cohort smokers and pop
        malesmokebase <- sum(m_M_basesmokeAP[(agel[kag]:ageu[kag]) + 1, year - startbc + 1])
        femsmokebase <- sum(m_F_basesmokeAP[(agel[kag]:ageu[kag]) + 1, year - startbc + 1])
        malepopbase <- sum(m_M_basepopAP[(agel[kag]:ageu[kag]) + 1, year - startbc + 1])
        fempopbase <- sum(m_F_basepopAP[(agel[kag]:ageu[kag]) + 1, year - startbc + 1])
        
        malesmoke <- sum(m_M_smokersAP[(agel[kag]:ageu[kag]) + 1, year - startbc + 1])
        femsmoke <- sum(m_F_smokersAP[(agel[kag]:ageu[kag]) + 1, year - startbc + 1])
        malepop <- sum(m_M_popAP[(agel[kag]:ageu[kag]) + 1, year - startbc + 1])
        fempop <- sum(m_F_popAP[(agel[kag]:ageu[kag]) + 1, year - startbc + 1])
        
        # Create row of results
        df_allstates_temp <- data.frame(
          year, age, coh, malesmoke, femsmoke, malesmokebase,
          femsmokebase, malepop, fempop, malepopbase, fempopbase, fipscode,
          policy.scen, mla.effect, agegroups[kag])
        
        names(df_allstates_temp) <- c('year', 'age', 'cohort', 'malesmoke', 'femsmoke', 'malesmokebase',
                                      'femsmokebase', 'malepop', 'fempop', 'malepopbase', 'fempopbase',
                                      'state', 'policy.scen', 'mla.effect', 'agegroup')
        
        # Append results
        df_allstates_res <- rbind(df_allstates_res, df_allstates_temp)
        }
      }
    }
  }
  return(df_allstates_res)
}

for (s in v_statefips){
  l_thissttate <- l_combined_state_prev[[s]]
  l_base <- l_thissttate[['baseline']]
  l_baseline <- l_base[[names(v_mla.effects[4])]]
  for (p in 1:3){
      l_policy <- l_thissttate[[v_policy.scen[p]]]
      for (e in 1:3){
        l_mla <- l_policy[[names(v_mla.effects[e])]]
        df_temp <- calculate_allstate_prev(l_mla, l_baseline, s, v_policy.scen[p], names(v_mla.effects[e]))
        df_allstates_res <-rbind(df_allstates_res, df_temp)
      } 
    }
}



# Isolate 18-99 age group for cohort results
df_cohorts_allages <- subset(df_allstates_res, agegroup == '18-99')

policy_scenarios <- unique(df_allstates_res$policy.scen)
mla_effects <- unique(df_allstates_res$mla.effect)

# Loop through each combination of policy.scen and mla.effect
for (policy in policy_scenarios) {
  for (mla in mla_effects) {
    
    # Filter the data for each combination of policy.scen and mla.effect
    df_filtered <- df_allstates_res %>%
      filter(policy.scen == policy, mla.effect == mla)
    
    # Cohort rows
    national_prevalence_coh <- df_filtered %>%
      group_by(year, age, cohort) %>%
      summarize(
        national_male_prevalence_baseline = sum(malesmokebase) / sum(malepopbase),
        national_female_prevalence_baseline = sum(femsmokebase) / sum(fempopbase),
        national_male_prevalence_policy = sum(malesmoke) / sum(malepop),
        national_female_prevalence_policy = sum(femsmoke) / sum(fempop),
        combined_prevalence_policy = (sum(malesmoke) + sum(femsmoke)) / (sum(malepop) + sum(fempop)),
        combined_prevalence_baseline = (sum(malesmokebase) + sum(femsmokebase)) / (sum(malepopbase) + sum(fempopbase))
      )
    
    # Group by year and agegroup, summarizing prevalence data
    national_prevalence_combined <- df_filtered %>%
      group_by(year, agegroup) %>%
      summarize(
        national_male_prevalence_baseline = sum(malesmokebase) / sum(malepopbase),
        national_female_prevalence_baseline = sum(femsmokebase) / sum(fempopbase),
        national_male_prevalence_policy = sum(malesmoke) / sum(malepop),
        national_female_prevalence_policy = sum(femsmoke) / sum(fempop),
        combined_prevalence_policy = (sum(malesmoke) + sum(femsmoke)) / (sum(malepop) + sum(fempop)),
        combined_prevalence_baseline = (sum(malesmokebase) + sum(femsmokebase)) / (sum(malepopbase) + sum(fempopbase))
      ) %>%
      mutate(cohort = 'ALL') %>%          # Set cohort to 'ALL'
      rename(age = agegroup) %>%          # Rename 'agegroup' to 'age'
      relocate(year, age, cohort)         # Ensure the first columns are 'year', 'age', and 'cohort'
    
    # Make some formatting changes
    national_prevalence_coh$cohort <- as.character(national_prevalence_coh$cohort)
    national_prevalence_coh$age <- as.character(national_prevalence_coh$age)
    resultsfile_temp <- rbind(national_prevalence_coh, national_prevalence_combined)
    
    resultsfile <- resultsfile_temp %>%
      select('year', 'age', 'cohort', 'national_male_prevalence_baseline',
            'national_female_prevalence_baseline','national_male_prevalence_policy',
             'national_female_prevalence_policy', 'combined_prevalence_baseline', 
             'combined_prevalence_policy')
    
    resultsfile$policy_year <- 'NA'
    colnames(resultsfile) <- c('year', 'age', 'cohort', 'males_baseline',
                               'females_baseline', 'males_policy', 'females_policy',
                               'both_baseline', 'both_policy', 'policy_year')
    
    # Set initperc based on mla values
    if (mla == "main") {
      initperc <- 34
    } else if (mla == "upper") {
      initperc <- 53
    } else if (mla == "lower") {
      initperc <- 15
    } else {
      initperc <- 0  # Default value if none match
    }
    
    # Abbreviations for policy.scen
    if (policy == "local") {
      policy.abbr <- "L"
    } else if (policy == "statelocal") {
      policy.abbr <- "SL"
    } else if (policy == "fedstatelocal") {
      policy.abbr <- "FSL"
    } else {
      policy.abbr <- 'BL'  # Baseline case if policy.scen does not match any condition
    }
    
    # Write to CSV
    write_csv(resultsfile, paste0('source_data/US/t21/results/results_', initperc, '_', policy.abbr, '.csv'))
  }
}

   
