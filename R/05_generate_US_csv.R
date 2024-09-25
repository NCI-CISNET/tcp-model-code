## generates TCP tool csv files for the entire US
# load data 

load('../output/model_output_07.19.24.RData')
#--------- mortality outputs --------------------------------------------------

# Aggregate across all states to get US totals






#--------- prevalence outputs --------------------------------------------------

v_cohorts <- c(1990,2000,2010,2020,2030)
v_agecohorts <- pmin(99,2100-v_cohorts)
resultsfile <- data.frame(year=2005,age=0,cohort='NA',males_baseline=0,
                       females_baseline=0,males_policy=0,females_policy=0,
                       both_baseline=0,both_policy=0,policy_year=0)

agegroups <- c('12-17', '18-24', '25-44', '45-64', '65+', '18-99')
agel <- c(12, 18, 25, 45, 65, 18)
ageu <- c(17, 24, 44, 64, 99, 99)
# 
df_allstates_res <- NULL
# function to generate TCP files
calculate_USprev <- function(l_mla, l_baseline, fipscode, policy.scen, mla.effect) {
  # Extract policy values
  m_M_popAP <- l_mla[['m_M_popAP']]
  m_F_popAP <- l_mla[['m_F_popAP']]
  m_M_smokersAP <- l_mla[['m_M_smokers']]
  m_F_smokersAP <- l_mla[['m_F_smokers']]
  
  m_M_basepopAP <- l_baseline[['m_M_popAP']]
  m_F_basepopAP <- l_baseline[['m_F_popAP']]
  m_M_basesmokeAP <- l_baseline[['m_M_smokers']]
  m_F_basesmokeAP <- l_baseline[['m_F_smokers']]
  v_cohorts <- 2005
  
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
        df_temp <- calculate_USprev(l_mla, l_baseline, s, v_policy.scen[p], names(v_mla.effects[e]))
        df_allstates_res <-rbind(df_allstates_res, df_temp)
      } 
    }
}




# for combined US 
# sift out for each age group, each polic and mla, sum across all states, then perform analysis the same as eac

#write_csv(resultsfile,paste0('../source_data/',fips(fipscode,to="Abbreviation"), '/mla/results/results_',initperc,'_',policy.abbr,'.csv'))


