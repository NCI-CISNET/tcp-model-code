#----- contains generate_prevs(), calculate_mort(), run_model()functions 

generate_prevs <- function(startbc, gender, m_init.policy_AC, m_cess.policy_AC, m_initAC, 
                          m_cessAC, p_mortNS_AC, p_mortCS_AC, p_mortYSQ_AC, v_stdbirths){
  
  # file to store output with multi former smoker compartments
  v_namesoutput <- c("gender","cohort","age","year","baseline_initiation_rate",
                     "baseline_cessation_rate", "initiation_rate","cessation_rate",
                     "survivors","alive_smokers","smoking_prevalence", "former_prevalence", 
                     paste0("former_smokers_YSQ",1:40))
  m_output <- matrix(0,100*(endbc-startbc+1),length(v_namesoutput))
  colnames(m_output) <- v_namesoutput
  
  # Initialize matrices to store population by smoking status for all cohorts
  m_NSprevAC <- m_CSprevAC <- m_popAC <- matrix(0,100,endbc-startbc+1)
  colnames(m_NSprevAC) <- colnames(m_CSprevAC) <- colnames(m_popAC) <- startbc:endbc
  a_FSprevAC <- array(0,dim=c(100,endbc-startbc+1,40))
  
  COUNT=1  # counter for output file
  for (cohort in startbc:endbc){  # loop over cohorts
    numcoh <- as.numeric(cohort)
    charcoh <- as.character(cohort)
    
    v_p_smkinitpol <- m_init.policy_AC[,charcoh]   # initiation probabilities for current cohort (policy)
    v_p_smkcesspol <- m_cess.policy_AC[,charcoh]   # cessation probabilities for current cohort (policy)
    v_p_smkinitbase <- m_initAC[,charcoh]    # initiation probabilities for current cohort (baseline)
    v_p_smkcessbase <- m_cessAC[,charcoh]    # cessation probabilities for current cohort (baseline)
    
    ### death probabilities for current cohort
    v_p_mort.NS <- p_mortNS_AC[p_mortNS_AC[,1]==numcoh,5]       # death probabilities for Never Smokers
    v_p_mort.CS <- p_mortCS_AC[p_mortCS_AC[,1]==numcoh,5]        # death probabilities for Current Smokers
    v_p_mort.YSQ <- p_mortYSQ_AC[p_mortYSQ_AC[,1,1]==numcoh,5,]       # death probabilities for Former Smokers by YSQ
    
    # Matrices to store population by age for current cohort
    m_NS <- matrix(0,100,1)                    # matrix for Never Smokers
    m_CS <- matrix(0,100,1)                    # matrix for Current Smokers
    m_FS_YSQ <- matrix(0,100,40)               # matrix for Former Smokers by YSQ
    
    ## Fill output for age 0
    m_NS[1] <- v_stdbirths[cohort-startbc+1]
    m_output[COUNT,] <- c(gender,cohort,0,cohort,0,0,0,0,m_NS[1],0,0,0,0*(1:40))
    COUNT <- COUNT+1
    
    for (i in 2:100) {  # loop over age
      m_NS[i] <- m_NS[i-1]*(1-v_p_smkinitpol[i-1])*(1-v_p_mort.NS[i-1])
      
      m_CS[i] <- m_NS[i-1]*v_p_smkinitpol[i-1]*(1-v_p_mort.NS[i-1])+m_CS[i-1]*(1-v_p_smkcesspol[i-1])*(1-v_p_mort.CS[i-1]) 
      
      m_FS_YSQ[i,1] <- m_CS[i-1]*(v_p_smkcesspol[i-1])*(1-v_p_mort.CS[i-1])
      
      for (j in 2:39){
        m_FS_YSQ[i,j] <- m_FS_YSQ[i-1,j-1]*(1-v_p_mort.YSQ[i-1,j-1]) 
      }
      m_FS_YSQ[i,40] <- m_FS_YSQ[i-1,39]*(1-v_p_mort.YSQ[i-1,39]) + m_FS_YSQ[i-1,40]*(1-v_p_mort.YSQ[i-1,40]) 
      
      # store populations by smoking status and prevalence and initiation/cessation rates in output file
      survivors <- m_NS[i]+m_CS[i]+sum(m_FS_YSQ[i,])
      m.FS_YSQsum <- sum(m_FS_YSQ[i,]) # using multiple former compartments
      
      m_output[COUNT,] <- c(gender,cohort,i-1,cohort+i-1,v_p_smkinitbase[i],v_p_smkcessbase[i],v_p_smkinitpol[i],v_p_smkcesspol[i],survivors,m_CS[i],m_CS[i]/survivors,m.FS_YSQsum/survivors,m_FS_YSQ[i,])
      COUNT <- COUNT+1
    }
    
    # total population
    m_totalpopbc <- m_NS+m_CS+rowSums(m_FS_YSQ)
    
    # store prevalence for current cohort
    m_NSprevAC[,charcoh] <- m_NS/m_totalpopbc
    m_CSprevAC[,charcoh] <- m_CS/m_totalpopbc
    m_popAC[,charcoh] <- m_totalpopbc
    
    for (j in 1:40){
      a_FSprevAC[,cohort-startbc+1,j] <- m_FS_YSQ[,j]/m_totalpopbc
    }
  }
  
  m_popAP <- matrix(0,nrow=100,ncol=cohyears+100)
  m_CSprevAP <-  m_NSprevAP <-  matrix(0, nrow=100, ncol= (cohyears+100))
  a_FSprevAP <- array(0,dim=c(100,(cohyears+100),40))
  
  # format by calendar year (AP) instead of by cohort (AC)
  for (byr in 1:cohyears){
    for (age in 0:99){
      m_popAP[age+1,byr+age] <- m_popAC[age+1,byr]
      m_CSprevAP[age+1,byr+age] <- m_CSprevAC[age+1,byr]
      m_NSprevAP[age+1,byr+age] <- m_NSprevAC[age+1,byr]
      
      for (j in 1:40){
        a_FSprevAP[age+1,byr+age,j] <- a_FSprevAC[age+1,byr,j]
      }
    }
  }
  
  m_smokersAP <- m_CSprevAP*m_popAP
  colnames(m_popAP) <- colnames(m_CSprevAP) <- colnames(m_NSprevAP) <- colnames(a_FSprevAP) <- startbc:endyear
  
  return(list(m_output= m_output, m_NSprevAC= m_NSprevAC, m_CSprevAC= m_CSprevAC,
              a_FSprevAC= a_FSprevAC, m_popAC= m_popAC, m_NSprevAP= m_NSprevAP,
              m_CSprevAP= m_CSprevAP, a_FSprevAP= a_FSprevAP, m_popAP= m_popAP, m_smokersAP= m_smokersAP)) 
}

#------------------------------------------------------------------------------
# Calculate number of Smoking-Attributable Deaths (SADs), and
# Years of Life Lost (YLL) using output from generate_prev function
#-------------------------------------------------------------------------------

calculate_mort<-function(l_pop_outputs, m_p_mortNS_AP, m_p_mortCS_AP, 
                        a_p_mortYSQ_AP, m_NS.LE, df_census_data){
  
  m_popdist<- as.matrix(cbind(df_census_data,rep(df_census_data[193],100))) #reformatted census data
  
  ## Calculate SADs using former smoker mortality probability by years 
  ## since quitting (YSQ) with multiple former smoker compartments
  
  m_SAD_AP <- m_popdist[,v_calyears]*(l_pop_outputs$m_CSprevAP[,v_calyears]*(m_p_mortCS_AP-m_p_mortNS_AP))
  
  for (j in 1:40){
    m_SAD_AP <- m_SAD_AP+m_popdist[,v_calyears]*l_pop_outputs$a_FSprevAP[,v_calyears,j]*(a_p_mortYSQ_AP[,,j]-m_p_mortNS_AP)
  }
  df_SAD_AP <- as.data.frame(m_SAD_AP)
  
  # Set all SADs before 2025 birth cohort (column 78) to NA
  policycohort <- 2005 # first birth cohort affected by T21 in the year 2025 is 2005
  df_SAD_AP[,1:(policycohort-startbc)]<-0
  for (c in (policycohort-startbc+1):ncol(df_SAD_AP)){
    for (r in 1:nrow(df_SAD_AP)){
      if ((c-r)<(policycohort-startbc)){
        df_SAD_AP[r,c]<- 0 # all cells below the diagonal for the 1985 birth cohort
      }
    }
  }
  v_SADyear=colSums(df_SAD_AP)
  
  ###------------- LIFE YEARS LOST--------------------------
  df_YLL_AP <- df_SAD_AP*m_NS.LE #life expectancy of never smokers 
  v_YLLyear <- colSums(df_YLL_AP)
  
  ##---------- output as AC----------------------------------
  m_SAD_AP <- as.matrix(df_SAD_AP)
  m_YLL_AP <- as.matrix(df_YLL_AP)
  
  n <- nrow(m_SAD_AP)
  m <- ncol(m_SAD_AP)
  
  m_SAD_AC <- matrix(NA, nrow = n, ncol = m)
  m_YLL_AC <- matrix(NA, nrow = n, ncol = m)
  
  for (i in 1:n) {
    for (j in 1:m) {
      m_SAD_AC[i, j - i + 1] <- m_SAD_AP[i, j]
      m_YLL_AC[i, j - i + 1] <- m_YLL_AP[i, j]
    }
  }
  return(list(df_SAD_AP=df_SAD_AP, v_SADyear= v_SADyear, df_YLL_AP= df_YLL_AP,
              v_YLLyear=v_YLLyear, m_SAD_AC=m_SAD_AC, m_YLL_AC=m_YLL_AC))
}

#-------------------------------------------------------------------------------

run_model <- function(mla.effect, name){

  # RUN STATUS QUO MODEL
  # set initiation and cessation for policy to be the same as the baseline 
  # generate_prevs() determines prevalence and calculate_mort() determines mortality outcomes
  # generate_prevs inputs: (starting cohort, gender, initiation, cessation , policy_initiation, policy_cessation,
  # neversmoker_mortality, currentsmoker_mortality, formersmoker_mortality, mortality_by_YSQ, state_number_of_births)
  
  gender <- 'Men'
  l_M.base.prev <- generate_prevs(startbc, gender, m_M.initAC, m_M.cessAC, 
                          m_M.initAC, m_M.cessAC,m_p_M.mortNS_AC,
                          m_p_M.mortCS_AC, a_p_M.mortYSQ_AC, v_stdbirths)
  
  l_M.base.mort <- calculate_mort(l_M.base.prev, m_p_M.mortNS_AP, m_p_M.mortCS_AP, 
                               a_p_M.mortYSQ_AP, m_M.NS.LE, df_M.census_data)
  
  gender <- 'Women'
  l_F.base.prev <- generate_prevs(startbc, gender, m_F.initAC, m_F.cessAC,
                               m_F.initAC, m_F.cessAC, m_p_F.mortNS_AC,
                               m_p_F.mortCS_AC, a_p_F.mortYSQ_AC, v_stdbirths)
  
  l_F.base.mort <- calculate_mort(l_F.base.prev, m_p_F.mortNS_AP, m_p_F.mortCS_AP,
                               a_p_F.mortYSQ_AP, m_F.NS.LE, df_F.census_data)
  
  ### TO APPLY SCENARIO EFFECTS, DATA NEEDS TO BE REFORMATTED TO AGE-PERIOD (AP)
  ### REFORMATTING ALONG DIAGONAL FROM AC TO AP REQUIRES EXTENDING TO 2200 (CALYEARS) 
  
  m_F.init.base_AP <- matrix(NA,100,(cohyears+100)) # initiation rates for women, age-period for baseline (status-quo)
  m_M.init.base_AP <- matrix(NA,100,(cohyears+100))
  colnames(m_F.init.base_AP) <- colnames(m_M.init.base_AP) <- startbc:endyear
  m_MLAeffectsAP <- matrix(1,100,(cohyears+100))
  colnames(m_MLAeffectsAP) <- startbc:endyear

  ## REFORMAT
  for (byr in startbc:(endbc)){
    for (age in 0:99){
      calyr <- byr+age
      m_F.init.base_AP[age+1,calyr-startbc+1] <- m_F.initAC[age+1,byr-startbc+1]
      m_M.init.base_AP[age+1,calyr-startbc+1] <- m_M.initAC[age+1,byr-startbc+1]
    }
  }
  
  ## APPLY POLICY EFFECTS TO BASELINE INITIATION Starting in Policy Year 2025-2200
  for (y in v_policy.ages) {
    m_MLAeffectsAP[y, (policyyear-startbc+1):(endyear-startbc+1)] <- 1 - mla.effect
  }

  m_F.init.policy_AP <- m_MLAeffectsAP*m_F.init.base_AP
  m_M.init.policy_AP <- m_MLAeffectsAP*m_M.init.base_AP
  
  ## REFORMAT BACK TO AGE COHORT
  m_F.init.policy_AC <- NULL
  m_M.init.policy_AC <- NULL 
  
  for (bc in c(startbc:endbc)){
    m_F.init.policy_AC <- cbind(m_F.init.policy_AC, diag(m_F.init.policy_AP[,(bc-(startbc-1)):293]))   
    m_M.init.policy_AC <- cbind(m_M.init.policy_AC, diag(m_M.init.policy_AP[,(bc-(startbc-1)):293]))
  }
  colnames(m_F.init.policy_AC) <- colnames(m_M.init.policy_AC) <- startbc:endbc
  
  
  ### RUN POLICY SCENARIOS   
  gender <- 'Men'
  l_M.policy.prev <- generate_prevs(startbc, gender, m_M.init.policy_AC, m_M.cessAC,
                                  m_M.initAC, m_M.cessAC, m_p_M.mortNS_AC,
                                  m_p_M.mortCS_AC, a_p_M.mortYSQ_AC, v_stdbirths)
  
  l_M.policy.mort <- calculate_mort(l_M.policy.prev, m_p_M.mortNS_AP, m_p_M.mortCS_AP, 
                                    a_p_M.mortYSQ_AP, m_M.NS.LE, df_M.census_data)
  
  gender <- 'Women'
  l_F.policy.prev <- generate_prevs(startbc, gender, m_F.init.policy_AC, m_F.cessAC,
                                    m_F.initAC, m_F.cessAC, m_p_F.mortNS_AC,
                                    m_p_F.mortCS_AC, a_p_F.mortYSQ_AC, v_stdbirths)
  
  l_F.policy.mort <- calculate_mort(l_F.policy.prev, m_p_F.mortNS_AP, m_p_F.mortCS_AP, 
                                    a_p_F.mortYSQ_AP, m_F.NS.LE, df_F.census_data)
  
  #------------------- format prev for outputting -----------------------------------
  df_CSprevs <- NULL
  
  # age groups to loop through
  v_minage <- c(18, 18, 25, 45, 65)
  v_maxage <- c(99, 24, 44, 64, 99)
  
  for (i in c(1:5)){
    minage <- v_minage[i]
    maxage <- v_maxage[i]
    
    m_M.CSprev <- l_M.policy.prev$m_CSprevAP
    m_F.CSprev <- l_F.policy.prev$m_CSprevAP
    
    # Create population matrices
    m.M.pop_AP <- as.matrix(cbind(df_M.census_data, rep(df_M.census_data[193], 100))) # Assume constant population sizes in future
    m.F.pop_AP <- as.matrix(cbind(df_F.census_data, rep(df_F.census_data[193], 100)))
    
    # Calculate prevalence for men
    v_M.prev.minmax <- colSums(m.M.pop_AP[(minage+1):(maxage+1), ] * m_M.CSprev[(minage+1):(maxage+1), ]) / colSums(m.M.pop_AP[(minage+1):(maxage+1), ])
    
    # Calculate prevalence for women
    v_F.prev.minmax <- colSums(m.F.pop_AP[(minage+1):(maxage+1), ] * m_F.CSprev[(minage+1):(maxage+1), ]) / colSums(m.F.pop_AP[(minage+1):(maxage+1), ])
    
    # Calculate combined prevalence for both men and women
    v_numerator <- colSums(m.M.pop_AP[(minage+1):(maxage+1), ] * m_M.CSprev[(minage+1):(maxage+1), ]) + colSums(m.F.pop_AP[(minage+1):(maxage+1), ] * m_F.CSprev[(minage+1):(maxage+1), ])
    v_denominator <- colSums(m.M.pop_AP[(minage+1):(maxage+1), ]) + colSums(m.F.pop_AP[(minage+1):(maxage+1), ])
    v_B.prev.minmax <- v_numerator / v_denominator
    
    # Combine data into a single data frame for the current age group
    df_CSprev_temp <- as.data.frame(rbind(
      cbind(v_M.prev.minmax[1:cohyears], "Men"),
      cbind(v_F.prev.minmax[1:cohyears], "Women"),
      cbind(v_B.prev.minmax[1:cohyears], "Both")
    ))
    
    # Set column names
    colnames(df_CSprev_temp) <- c("prev", "gender")
    
    # Add additional columns
    df_CSprev_temp$age <- paste0(minage, ".", maxage)
    df_CSprev_temp$year <- rep(names(v_M.prev.minmax[1:cohyears]), 3)
    df_CSprev_temp$mla.effect <- name
    
    # Combine with the final data frame
    df_CSprevs <- rbind(df_CSprevs, df_CSprev_temp)
  }
  
  df_CSprevs$prev<- as.numeric(df_CSprevs$prev)
  df_CSprevs$year<- as.numeric(df_CSprevs$year)
  
  #--------------format prev for output ----------------------------------------
  #-----------------------------------------------------------------------------
  
  m_M.smokers <- l_M.policy.prev$m_smokersAP
  m_F.smokers <- l_F.policy.prev$m_smokersAP
  m_M.popAP <- l_M.policy.prev$m_popAP
  m_F.popAP <- l_F.policy.prev$m_popAP
  m_M.CSprevAC <- cbind(l_M.policy.prev$m_CSprevAC, 'Men')
  m_F.CSprevAC <- cbind(l_F.policy.prev$m_CSprevAC, 'Women')
  
  l_pop_out <- list(
    mla.effect=mla.effect, 
    m_M_smokers = l_M.policy.prev$m_smokersAP,
    m_F_smokers = l_F.policy.prev$m_smokersAP,
    m_M_popAP = l_M.policy.prev$m_popAP,
    m_F_popAP = l_F.policy.prev$m_popAP
  )
  
  #---------------format policy YLL and SADs for outputting --------------------
  #annual sum YLL
  v_M.YLLyear <- l_M.policy.mort$v_YLLyear
  v_F.YLLyear <- l_F.policy.mort$v_YLLyear
  v_B.YLLyear <- v_M.YLLyear + v_F.YLLyear
  #cumulative sum YLL
  v_M.cYLL <- cumsum(v_M.YLLyear)
  v_F.cYLL <- cumsum(v_F.YLLyear)
  v_B.cYLL <- cumsum(v_B.YLLyear)
  
  #annual sum SAD
  v_M.SADyear <- l_M.policy.mort$v_SADyear
  v_F.SADyear <- l_F.policy.mort$v_SADyear
  v_B.SADyear <- v_M.SADyear+ v_F.SADyear
  #cumulative sum SAD
  v_M.cSAD <- cumsum(v_M.SADyear)
  v_F.cSAD <- cumsum(v_F.SADyear)
  v_B.cSAD <- cumsum(v_B.SADyear)
  
  #---------------- LYG using YSQ ----------------------------------------------------------------------------
  
  df_M.LYG_AP <- l_M.base.mort$df_YLL_AP - l_M.policy.mort$df_YLL_AP
  df_F.LYG_AP <- l_F.base.mort$df_YLL_AP - l_F.policy.mort$df_YLL_AP
  #annual sum LYG
  v_M.LYGyear <- colSums(df_M.LYG_AP)
  v_F.LYGyear <- colSums(df_F.LYG_AP)
  v_B.LYGyear <- v_M.LYGyear + v_F.LYGyear
  #cumulative sum LYG
  v_M.cLYG <- cumsum(v_M.LYGyear)
  v_F.cLYG <- cumsum(v_F.LYGyear)
  v_B.cLYG <- cumsum(v_B.LYGyear)
  
  #annual sum SADs averted 
  v_M.SADs_averted <- l_M.base.mort$v_SADyear - l_M.policy.mort$v_SADyear
  v_F.SADs_averted <- l_F.base.mort$v_SADyear - l_F.policy.mort$v_SADyear
  v_B.SADs_averted <- v_M.SADs_averted + v_F.SADs_averted
  #cumulative sum SADs averted
  v_M.cSADs_averted <- cumsum(v_M.SADs_averted)
  v_F.cSADs_averted <- cumsum(v_F.SADs_averted)  
  v_B.cSADs_averted <- cumsum(v_B.SADs_averted)
 
  ##---combine mortality outputs------------------------------------------------------
  m_M.mortout <- cbind(v_M.YLLyear, v_M.cYLL, v_M.SADyear, v_M.cSAD, 
                    v_M.LYGyear, v_M.cLYG, v_M.SADs_averted,
                    v_M.cSADs_averted,1908:2100, 'Men')
  
  m_F.mortout <- cbind(v_F.YLLyear, v_F.cYLL, v_F.SADyear, v_F.cSAD, 
                      v_F.LYGyear, v_F.cLYG, v_F.SADs_averted,
                      v_F.cSADs_averted, 1908:2100, 'Women')
  
  m_B.mortout <- cbind(v_B.YLLyear, v_B.cYLL, v_B.SADyear, v_B.cSAD, 
                      v_B.LYGyear, v_B.cLYG, v_B.SADs_averted,
                      v_B.cSADs_averted, 1908:2100, 'Both')
  
  df_mort.outputs <- as.data.frame(rbind(m_M.mortout,m_F.mortout,m_B.mortout))
  colnames(df_mort.outputs) <- c('YLL','cYLL','SADs', 'cSAD', 'LYG', 'cLYG',
                              'SADsAverted','cSADsAverted','year', 'gender' )
  df_mort.outputs$mla.effect <- name
  df_mort.outputs <- df_mort.outputs %>% mutate(across(c('YLL','cYLL','SADs',
                                                         'cSAD', 'LYG', 'cLYG', 
                                                         'SADsAverted','cSADsAverted',
                                                         'year'), as.numeric))


  return(list(df_mort.outputs= df_mort.outputs, l_pop_out=l_pop_out, df_CSprevs=df_CSprevs ))

}