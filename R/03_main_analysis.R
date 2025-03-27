### SCRIPT TO LOOP THROUGH ALL US STATES AND RUN MODEL FUNCTIONS
### REFORMATS OUTPUTS AND SAVES DATA 

# Run model and get prevalence for each state -----------------------------------------------------

df_mortality.out <- NULL
df_prev.by.state <- NULL
l_combined_state_prev <- list()
t_init <- Sys.time()





































































#--------- tax effect analysis (incomplete) ------------------------------------

args <- commandArgs(trailingOnly = TRUE)

# Process each state using apply function (no loops)
state_results <- lapply(v_statefips, function(s) {
  l_state_scenarios <- list()  # Initialize list for state's scenarios
  print(paste0(fips(s,to="Abbreviation")," (",s,")"))
  
  # Load state-specific data
  load(paste0("data/state_inputs/p.mort_", s, ".RData"))  # load mortality probabilities
  load(paste0("data/state_inputs/smk_", s, ".RData"))     # load smoking parameters
  load(paste0("data/state_inputs/pop_", s, ".RData"))     # load census population data
  load(paste0("data/state_inputs/le_", s, ".RData"))      # load life expectancy data
  
  # Run baseline scenario
  baseline_prev <- runstates(s, tax.effect = 0, effect.CI = 0, policy.scen = "baseline")
  
  # Run tax policy scenario
  adjusted_prev <- runstates(s, 
                           tax.effect = pricechange, 
                           effect.CI = initeff, 
                           policy.scen = "tax")
  
  # Store results
  l_state_scenarios[["baseline"]] <- baseline_prev
  l_state_scenarios[["tax"]] <- adjusted_prev
  
  return(l_state_scenarios)
})

l_combined_state_prev <- state_results

# Combine mortality outputs from all states and both policy scenarios (baseline and tax)
df_mortality.out <- do.call(rbind, lapply(l_combined_state_prev, function(x) {
  rbind(
    x$baseline$df_mort.outputs, 
    x$tax$df_mort.outputs)
}))

# Combine smoking prevalence outputs from all states and both policy scenarios (baseline and tax)
df_prev.by.state <- do.call(rbind, lapply(l_combined_state_prev, function(x) {
  rbind(x$baseline$df_CSprevs.by.state, 
        x$tax$df_CSprevs.by.state)
}))

# Save all outputs to a single file
save(df_mortality.out, df_prev.by.state, l_combined_state_prev, 
     initprice, tax, tax_year, newprice, pricechange,
     file=paste0('output/model_output_', date_variable, '.RData'))

# Run visualization
#source('R/04_visualization.R')
