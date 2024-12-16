### SCRIPT TO LOOP THROUGH ALL US STATES AND RUN MODEL FUNCTIONS
### REFORMATS OUTPUTS AND SAVES DATA 

# Run model and get prevalence for each state -----------------------------------------------------

df_mortality.out <- NULL
df_prev.by.state <- NULL
l_combined_state_prev <- list()
t_init=Sys.time()

for (s in v_statefips){
  l_state_scenarios <- list()  # Initialize list for state's scenarios
  print(paste0(fips(s,to="Abbreviation")," (",s,")"))
  #loop through policy scenarios
  for (p in seq_along(v_policy.scen)){
    l_state_policy_effects <- list()
    if (p==length(v_policy.scen)){      #baseline scenario
      print(v_mla.effects[length(v_mla.effects)])
      df_out=runstates(s, v_mla.effects[length(v_mla.effects)], names(v_mla.effects)[length(v_mla.effects)],v_policy.scen[p])
      df_mortality.out=rbind(df_mortality.out, df_out$df_mort.outputs)
      df_prev.by.state=rbind(df_prev.by.state, df_out$df_CSprevs.by.state)
      l_state_policy_effects[[names(v_mla.effects)[length(v_mla.effects)]]] <- df_out$l_prev_out
    }else{
      # loop through mla effects 
      for (e in seq_along(v_mla.effects)[-length(v_mla.effects)]) {
        print(v_mla.effects[e])
        df_out=runstates(s, v_mla.effects[e], names(v_mla.effects)[e],v_policy.scen[p])
        df_mortality.out=rbind(df_mortality.out, df_out$df_mort.outputs)
        df_prev.by.state=rbind(df_prev.by.state, df_out$df_CSprevs.by.state)
        l_state_policy_effects[[names(v_mla.effects)[e]]] <- df_out$l_prev_out 
      }
    }
    l_state_scenarios[[v_policy.scen[p]]] <- l_state_policy_effects
  }
  l_combined_state_prev[[s]]<-l_state_scenarios
}

#-----------------reformat all states by policy effort--------
#determine % reduction in cumulative SADs
df_mort.policy <- subset(df_mortality.out, policy.scenario!='baseline')
df_mort.base <- subset(df_mortality.out, policy.scenario=='baseline')

# split results by policy tier
policytierSADs<-function(fipscode, sex){
  df_fed.mort <- subset(df_mort.policy,state==fipscode & mla.effects=='main' & policy.scenario=='fedstatelocal' & gender==sex)
  df_state.mort<-subset(df_mort.policy,state==fipscode & mla.effects=='main' & policy.scenario=='statelocal' & gender==sex)
  df_local.mort<-subset(df_mort.policy,state==fipscode & mla.effects=='main' & policy.scenario=='local' & gender==sex)
  df_baseSADs<-subset(df_mort.base, state==fipscode & gender==sex)
  #determine differences in results by tier
  df_fed.mort$policyeffort <- df_fed.mort$SADsAvertedcum-df_state.mort$SADsAvertedcum
  df_fed.mort$policy_tier <- 'federal'
  df_state.mort$policyeffort <- df_state.mort$SADsAvertedcum-df_local.mort$SADsAvertedcum
  df_state.mort$policy_tier <- 'state'
  df_local.mort$policyeffort <- df_local.mort$SADsAvertedcum
  df_local.mort$policy_tier <- 'local'
  df_fed.mort$SADreduc <- (df_fed.mort$SADsAvertedcum/df_baseSADs$SADcum)*100
  df_state.mort$SADreduc <- (df_state.mort$SADsAvertedcum/df_baseSADs$SADcum)*100
  df_local.mort$SADreduc <- (df_local.mort$SADsAvertedcum/df_baseSADs$SADcum)*100
  #reduction in SADs based on each policy level tier
  df_fed.mort$SADreduc_tier <- (df_fed.mort$policyeffort/df_baseSADs$SADcum)*100
  df_state.mort$SADreduc_tier <- (df_state.mort$policyeffort/df_baseSADs$SADcum)*100
  df_local.mort$SADreduc_tier <- (df_local.mort$policyeffort/df_baseSADs$SADcum)*100
  
  return(list(df_fed.mort=df_fed.mort, df_local.mort=df_local.mort, df_state.mort=df_state.mort))
}

df_tiered_mort=NULL
for (s in v_statefips){
  for (sex in c('Female', 'Male', 'Both')){
    cohout <- policytierSADs(s,sex)
    df_tiered_mort <- rbind(df_tiered_mort, cohout$df_fed.mort, cohout$df_local.mort, cohout$df_state.mort)
  }
}

# # Save outputs
save(df_mortality.out, df_tiered_mort, df_mort.policy, df_mort.base,
     df_prev.by.state, l_combined_state_prev, file=paste0('output/model_output_', date_variable, '.RData'))

Sys.time()-t_init
#source('04_visualization.R')
