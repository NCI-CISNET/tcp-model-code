# Main Analysis
# Initialize data containers before the loop
# Output list: baseline_results, tax_out_list, tcexp_out_list, airlaws_out_list, warnings_out_list, warnings_out_list_2025

# Set policy flags (0 for baseline, 1 for policy analysis)
tax_policy <- 1
tcexp_policy <- 1
airlaws_policy <- 1 
warnings_policy <- 1

# ------------------------ Baseline Analysis -----------------------------------

df_mortality.out    <- data.frame()
df_prev.by.state    <- data.frame()
l_combined_state_prev <- list()
t_init <- Sys.time()

baseline_initiation_effect <- matrix(1, nrow = 100, ncol = cohyears+100)
baseline_cessation_effect  <- matrix(1, nrow = 100, ncol = cohyears+100)

colnames(baseline_initiation_effect) <- colnames(baseline_cessation_effect) <- as.character(startbc:endyear)

baseline_results <- list()  # to store baseline outputs for each state

for (s in v_statefips) {
  
  print(paste0(fips(s, to="Abbreviation"), " (", s, ") - Baseline"))
  
  # Run the baseline scenario using runstates() (which uses baseline AP->AC conversion)
  baseline_out <- runstates(s, baseline_initiation_effect, baseline_cessation_effect) 
  
  # Store outputs (you may also store the entire baseline object for later comparison)
  baseline_results[[s]] <- baseline_out
  df_mortality.out <- rbind(df_mortality.out, baseline_out$df_mort.outputs)
  df_prev.by.state <- rbind(df_prev.by.state, baseline_out$df_CSprevs.by.state)
}

# SAVE
out_dir <- "analysis_output"          
file_rds <- file.path(out_dir, "baseline_results.rds")
saveRDS(baseline_results, file = file_rds, compress = "xz")  

# ------------------------ Tobacco Tax Analysis --------------------------------

tax_out_list <- list()

df_prices <- read.csv("state_tax/cigarette_prices_by_state_2025.csv", stringsAsFactors = FALSE) %>%
  mutate(State = toupper(trimws(State))) %>%
  mutate(fips_code = sprintf("%02d", sapply(State, function(x) fips(x, to = "FIPS"))))

if (tax_policy == 1) {
  print("Running tax policy analysis")
  
  tax_grid <- 1:5
  policyYear <- 2025
  
  for (s in v_statefips) {
    
    initprice_s <- df_prices[df_prices$fips_code == s, "default_init_price"]
    
    for (tax in tax_grid) {
      
      tax_effects <- tax_effectCalculation(initprice=initprice_s, tax, startbc, endyear, policyYear,
                                           inidecay = 0.0, cesdecay = 0.2, iniagemod = 1, cesagemod = 1)
      
      print(paste0(fips(s, to = "Abbreviation"), " (", s,") – Tax +$", tax, " policy"))
      
      tax_out <- runstates(s, tax_effects$m.initiation.effect, tax_effects$m.cessation.effect)
      
      tax_out$initprice <- initprice_s
      tax_out$tax_raise <- tax
      
      key <- sprintf("%s_%ddollar", s, tax)       
      tax_out_list[[key]] <- tax_out
      
      df_mortality.out <- rbind(df_mortality.out, tax_out$df_mort.outputs)
      df_prev.by.state <- rbind(df_prev.by.state, tax_out$df_CSprevs.by.state)
      
      if (is.null(l_combined_state_prev[[s]])) {
        l_combined_state_prev[[s]] <- list()}
      l_combined_state_prev[[s]][["tax_policy"]] <- tax_out$l_prev_out}}}

# SAVE
out_dir <- "analysis_output"
file_rds <- file.path(out_dir, "tax_out_list.rds")
saveRDS(tax_out_list, file = file_rds, compress = "xz")  

# ------------------------ Tobacco Control Expenditures Analysis -------------------------

tcexp_policy <- 1
tcexp_out_list <- list()
df_mortality.out <- data.frame()
df_prev.by.state <- data.frame()
l_combined_state_prev <- list()

df_exp <- read.csv("state_tcexp/tcexp_by_state_2025.csv",
                   stringsAsFactors = FALSE) %>%
  mutate(State = toupper(trimws(State)), fips_code = fips(State, to = "FIPS"),
         default_level = parse_number(default_level) / 100)

if (tcexp_policy == 1) {
  message("Running tobacco control expenditures analysis")
  policyYear <- 2025
  
  for (s in v_statefips) {
    row_s <- df_exp[as.integer(df_exp$fips_code) == as.integer(s), ]
    base_lvl <- row_s$default_level    
    
    if (base_lvl >= 1.0) {
      exp_lvls <- 1.0                   
    } else {
      exp_lvls <- seq(base_lvl + 0.10, 1.0, by = 0.10)
    }
    
    for (lvl in exp_lvls) {
      if (lvl == 1.0 && base_lvl >= 1.0) {
        out_s <- baseline_results[[s]]    
      } else {
        eff  <- tcexp_effectCalculation(
          exp_level  = lvl,
          base_level = base_lvl,
          startbc    = 1908,
          endyear    = 2200,
          policyYear = policyYear,
          decay      = 0.2,
          iniagemod  = 1,
          cesagemod  = 1)
        
        print(paste0(fips(s, to = "Abbreviation"), " (", s, ") - Tobacco Control Expenditures Policy"))
        out_s <- runstates(s,eff$m.initiation.effect,eff$m.cessation.effect)}
      
      out_s$exp_level   <- lvl
      out_s$base_level  <- base_lvl
      out_s$already_100 <- base_lvl >= 1.0
      
      scenario_name <- sprintf("%s_exp%02d", s, round(lvl * 100))
      tcexp_out_list[[scenario_name]] <- out_s
      
      df_mortality.out <- rbind(df_mortality.out, out_s$df_mort.outputs)
      df_prev.by.state <- rbind(df_prev.by.state, out_s$df_CSprevs.by.state)
      l_combined_state_prev <- modifyList(l_combined_state_prev,
                                          setNames(list(setNames(list(out_s$l_prev_out), scenario_name)), s))}}}


# SAVE
out_dir <- "analysis_output"
file_rds <- file.path(out_dir, "tcexp_out_list.rds")
saveRDS(tcexp_out_list, file = file_rds, compress = "xz") 

## ----------------------- Smoke-free Air Laws Analysis -------------------------

airlaws_out_list <- list()
df_mortality.out <- data.frame()
df_prev.by.state <- data.frame()
l_combined_state_prev <- list()

##    pacwp = workplace, pacr = restaurant, pacb = bar
df_airlaws <- read.csv("state_airlaws/airlaws_by_state_2025.csv",stringsAsFactors = FALSE) %>%  # State, pacwp, pacr, pacb (0–100)
  mutate(State = toupper(trimws(State)),
         fips_code = fips(State, to = "FIPS"),
         default_pacwp = default_pacwp / 100,
         default_pacr = default_pacr / 100,
         default_pacb = default_pacb / 100)

names(df_airlaws)

if (airlaws_policy == 1) {
  message("Running smoke‑free air laws analysis")
  policyYear <- 2025                                    
  
  for (s in v_statefips) {
    row_s <- df_airlaws[df_airlaws$fips_code == as.integer(s), ]
    
    ## baseline coverage
    pacwp <- row_s$default_pacwp
    pacr <- row_s$default_pacr
    pacb <- row_s$default_pacb
    
    ## eight possible combinations of workplace / restaurant / bar (0 = no change, 1 = raise to 100%)
    scen_tbl <- expand.grid(Iwp = c(0, 1), Ir = c(0, 1), Ib = c(0, 1), KEEP.OUT.ATTRS = FALSE)
    
    for (ix in seq_len(nrow(scen_tbl))) {
      Iwp <- scen_tbl$Iwp[ix]; Ir <- scen_tbl$Ir[ix]; Ib <- scen_tbl$Ib[ix]
      ## initiation / cessation effect matrices ----------------
      eff <- airlaw_effectCalculation(initeff = 0.10,     # base initiation effect
                                      ceseff = 0.50,     # base cessation effect
                                      Iwp = Iwp, Ir = Ir, Ib = Ib,
                                      pacwp = pacwp, pacr = pacr, pacb = pacb,
                                      startbc = 1908, endyear = 2200,
                                      policyYear = policyYear,
                                      inidecay = 0.0, cesdecay = 0.2,
                                      iniagemod = 1, cesagemod = 1)  
      
      print(paste0(fips(s, to = "Abbreviation"), " (", s, ") - smoke-free air laws Policy"))
      
      out_s <- runstates(s, eff$m.initiation.effect, eff$m.cessation.effect)
      
      scenario_name <- sprintf("%s_air_wp%d_r%d_b%d", s, Iwp, Ir, Ib)
      
      out_s$Iwp <- Iwp; 
      out_s$Ir <- Ir; 
      out_s$Ib <- Ib    
      
      airlaws_out_list[[scenario_name]] <- out_s
      df_mortality.out  <- rbind(df_mortality.out,  out_s$df_mort.outputs)
      df_prev.by.state  <- rbind(df_prev.by.state,  out_s$df_CSprevs.by.state)
      
      if (is.null(l_combined_state_prev[[s]]))
        l_combined_state_prev[[s]] <- list()
      l_combined_state_prev[[s]][[scenario_name]] <- out_s$l_prev_out
    }
  }
}

# SAVE
out_dir <- "analysis_output"
file_rds <- file.path(out_dir, "airlaws_out_list.rds")
saveRDS(airlaws_out_list, file = file_rds, compress = "xz") 

# ------------------------ Graphic health warnings_2022 -----------------------

# Cigarette packs will be required to display graphic health warnings effective October 11th, 2022.

warnings_out_list <- list()
df_mortality.out <- data.frame()
df_prev.by.state <- data.frame()
l_combined_state_prev <- list()

if (warnings_policy == 1) {
  
  message("Running graphic‑health‑warning analysis")
  
  policyYear <- 2022 # first year with health warnings
  startbc <- 1908
  endyear <- 2200
  
  init_vec <- c(0.00, 0.05, 0.10, 0.15)  
  cess_vec <- c(0.00, 0.25, 0.50, 0.75) 
  
  scen_tbl <- expand.grid(init.reduce  = init_vec, cess.increase = cess_vec, KEEP.OUT.ATTRS = FALSE)
  
  for (s in v_statefips) {            
    for (ix in seq_len(nrow(scen_tbl))) {
      
      init.red <- scen_tbl$init.reduce[ix]
      cess.inc <- scen_tbl$cess.increase[ix]
      
      eff <- warnings_effectCalculation(init.reduce = init.red, cess.increase = cess.inc,
                                        startbc = startbc, endyear = endyear, policyYear = policyYear)
      
      print(paste0(fips(s, to = "Abbreviation"), " (", s, ") - graphic‑health‑warning analysis"))
      
      out_s <- runstates(s, eff$m.initiation.effect, eff$m.cessation.effect)
      
      scen_code <- sprintf("%s_warn_i%03d_c%03d", s, round(init.red  * 100), round(cess.inc * 100))
      
      out_s$init.red <- init.red
      out_s$cess.inc <- cess.inc
      out_s$policyYr <- policyYear
      
      warnings_out_list[[scen_code]] <- out_s
      df_mortality.out <- rbind(df_mortality.out, out_s$df_mort.outputs)
      df_prev.by.state <- rbind(df_prev.by.state, out_s$df_CSprevs.by.state)
      
      if (is.null(l_combined_state_prev[[s]]))
        l_combined_state_prev[[s]] <- list()
      l_combined_state_prev[[s]][[scen_code]] <- out_s$l_prev_out
    }
  }
}

# SAVE
out_dir <- "analysis_output"
file_rds <- file.path(out_dir, "warnings_out_list.rds")
saveRDS(warnings_out_list, file = file_rds, compress = "xz")  


# --- (Optionally) Run T21 Policy Analysis similarly ---
# If we need to run T21 policy analysis, we would compute its effect matrices and then run runstates()
# (or runstates_t21) using those matrices.
# For now, we leave t21_policy as 0 (baseline).
if (t21_policy == 1) {
  print("Running T21 policy analysis")
}

# Save outputs
save(df_mortality.out, 
     df_prev.by.state, 
     l_combined_state_prev,
     file = paste0("output/model_output_", date_variable, ".RData"))

Sys.time() - t_init


# ------------------------ Graphic health warnings_2025 -----------------------
warnings_out_list_2025 <- list()
df_mortality.out <- data.frame()
df_prev.by.state <- data.frame()
l_combined_state_prev <- list()

if (warnings_policy == 1) {
  
  message("Running graphic‑health‑warning analysis")
  
  policyYear <- 2025 # first year with health warnings
  startbc <- 1908
  endyear <- 2200
  
  init_vec <- c(0.00, 0.05, 0.10, 0.15)  
  cess_vec <- c(0.00, 0.25, 0.50, 0.75) 
  
  scen_tbl <- expand.grid(init.reduce  = init_vec, cess.increase = cess_vec, KEEP.OUT.ATTRS = FALSE)
  
  for (s in v_statefips) {            
    for (ix in seq_len(nrow(scen_tbl))) {
      
      init.red <- scen_tbl$init.reduce[ix]
      cess.inc <- scen_tbl$cess.increase[ix]
      
      eff <- warnings_effectCalculation(init.reduce = init.red, cess.increase = cess.inc,
                                        startbc = startbc, endyear = endyear, policyYear = policyYear)
      
      print(paste0(fips(s, to = "Abbreviation"), " (", s, ") - graphic‑health‑warning analysis"))
      
      out_s <- runstates(s, eff$m.initiation.effect, eff$m.cessation.effect)
      
      scen_code <- sprintf("%s_warn_i%03d_c%03d", s, round(init.red  * 100), round(cess.inc * 100))
      
      out_s$init.red <- init.red
      out_s$cess.inc <- cess.inc
      out_s$policyYr <- policyYear
      
      warnings_out_list_2025[[scen_code]] <- out_s
      df_mortality.out <- rbind(df_mortality.out, out_s$df_mort.outputs)
      df_prev.by.state <- rbind(df_prev.by.state, out_s$df_CSprevs.by.state)
      
      if (is.null(l_combined_state_prev[[s]]))
        l_combined_state_prev[[s]] <- list()
      l_combined_state_prev[[s]][[scen_code]] <- out_s$l_prev_out
    }
  }
}

# SAVE
out_dir <- "analysis_output"
file_rds <- file.path(out_dir, "warnings_out_list_2025.rds")
saveRDS(warnings_out_list_2025, file = file_rds, compress = "xz") 

#------------------------- LOAD Main Analysis Results -------------------------
baseline_results <- readRDS("analysis_output/baseline_results.rds")
tax_out_list <- readRDS("analysis_output/tax_out_list.rds")
airlaws_out_list <- readRDS("analysis_output/airlaws_out_list.rds")
tcexp_out_list <- readRDS("analysis_output/tcexp_out_list.rds")
warnings_out_list <- readRDS("analysis_output/warnings_out_list.rds")
warnings_out_list_2025 <- readRDS("analysis_output/warnings_out_list_2025.rds")
