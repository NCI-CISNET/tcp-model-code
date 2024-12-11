rm(list = ls()) 
setwd("/Users/jt936/Dropbox/GitHub/tcp-model-code/")

source('R/01_model_inputs.R')
source('R/02_model_functions.R')

# Run model and get prevalence, mortality -----------------------------------------------------

df_mortality.out <- NULL
df_prev.out <- NULL
l_pop.out <- list()

for (e in seq_along(v_mla.effects)) {
      print(v_mla.effects[e])
      df_out=run_model(v_mla.effects[e], names(v_mla.effects)[e])
      df_mortality.out=rbind(df_mortality.out, df_out$df_mort.outputs)
      df_prev.out=rbind(df_prev.out, df_out$df_CSprevs)
      l_pop.out[[names(v_mla.effects)[e]]] <- df_out$l_pop_out 
}

# # Save outputs
save(df_mortality.out, df_prev.out, l_pop.out, file=paste0('output/model_run_', date_variable, '.RData'))

source('R/04_visualization.R')