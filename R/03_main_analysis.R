# Main Analysis
# Initialize data containers before the loop
# Output list: baseline_results, tax_out_list, tcexp_out_list, airlaws_out_list, warnings_out_list, warnings_out_list_2025

# t.init = Sys.time()
#code
# print(Sys.time() - t.init)

# Set policy flags (0 for baseline, 1 for policy analysis)
tax_policy <- 1
tcexp_policy <- 1
airlaws_policy <- 1 
warnings_policy <- 1
policyyear <- 2026
# SAVE
out_dir <- file.path("analysis_output", format(Sys.Date(), "%Y%m%d"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------ Baseline Analysis -----------------------------------

df_mortality.out    <- data.frame()
df_prev.by.state    <- data.frame()
l_combined_state_prev <- list()

baseline_initiation_effect <- matrix(1, nrow = 100, ncol = cohyears+100)
baseline_cessation_effect  <- matrix(1, nrow = 100, ncol = cohyears+100)

colnames(baseline_initiation_effect) <- colnames(baseline_cessation_effect) <- as.character(startbc:endyear)

baseline_results <- list()  # to store baseline outputs for each state

for (s in v_statefips) {
  t_state_start <- Sys.time()

  print(paste0(fips_to_abbr(s), " (", s, ") - Baseline"))
  
  # Run the baseline scenario using runstates() (which uses baseline AP->AC conversion)
  baseline_out <- runstates(s, baseline_initiation_effect, baseline_cessation_effect) 
  
  # Store outputs
  baseline_results[[s]] <- baseline_out
  df_mortality.out <- rbind(df_mortality.out, baseline_out$df_mort.outputs)
  df_prev.by.state <- rbind(df_prev.by.state, baseline_out$df_CSprevs.by.state)
  print(paste("Done in:", round(difftime(Sys.time(), t_state_start, units = "secs"), 2), "seconds"))
}

file_rds <- file.path(out_dir, "baseline_results.rds")
saveRDS(baseline_results, file = file_rds, compress = "xz")  


# ------------------------ Tobacco Tax Analysis --------------------------------

tax_out_list <- list()
df_mortality.out <- data.frame()
df_prev.by.state <- data.frame()
l_combined_state_prev <- list()

df_prices <- read.csv("state_tax/cigarette_prices_by_state_2025.csv", stringsAsFactors = FALSE) %>%
  mutate(State = toupper(trimws(State))) %>%
  mutate(fips_code = sprintf("%02d", sapply(State, function(x) fips(x, to = "FIPS"))))


if (tax_policy == 1) {
  v_policy_years <- c(2026:2030, 2035, 2040)
  v_tax_hikes <- c(0, 1.00, 1.50, 2.00, 2.50, 3.00)
  v_ppp_targets <- seq(8.00, 15.00, by = 0.50)
  
  # Scenario Grid (15 * 6 = 90)
  df_scenarios <- expand.grid(ppp_target = v_ppp_targets, tax_hike = v_tax_hikes)
  
  # RDS Directory
  tax_rds_dir <- file.path(out_dir, "tax_scenario_rds")
  dir.create(tax_rds_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Main Loop
  for (s in c("08")) {
    t_state_start <- Sys.time()
    state_results_list <- list() 
    
    # Get State Price
    initprice_s <- df_prices[df_prices$fips_code == s, "default_init_price"]
    initprice_s <- as.numeric(initprice_s[1])
    if (is.na(initprice_s)) next
    
    abbr <- fips_to_abbr(s)
    message(paste0("Running State: ", abbr, " | Base Price: $", initprice_s))

    # LEVEL 1: Loop Scenarios (The 90 CSV files) ---
    for (i in 1:nrow(df_scenarios)) {
      t_scen_start <- Sys.time()
      
      row <- df_scenarios[i, ]
      target_ppp <- row$ppp_target
      added_tax  <- row$tax_hike
      
      # Prepare Naming Tag (e.g., "4.00_t0.00")
      scenario_tag <- sprintf("%0.2f_t%0.2f", target_ppp, added_tax)
      
      # Containers to accumulate years for THIS scenario
      accum_deaths  <- list()
      accum_lyg     <- list()
      accum_results <- list()
      
      # Loop Years (The rows inside CSV) ---
      for (py in v_policy_years) {
        t1 <- Sys.time()
        
        # Calculation Logic
        gap_to_floor <- max(0, target_ppp - initprice_s)
        total_tax_to_apply <- gap_to_floor + added_tax
        
        tax_effects <- tax_effectCalculation(
          initprice = initprice_s, tax = total_tax_to_apply,
          startbc = startbc, endyear = endyear, policyYear = py,
          inidecay = 0.0, cesdecay = 0.2, iniagemod = 1, cesagemod = 1
        )
        t2 <- Sys.time()
        
        tax_out <- runstates(s, tax_effects$m.initiation.effect, tax_effects$m.cessation.effect)
        t3 <- Sys.time()
        
        # Call Generate Output but DO NOT WRITE (write_files = FALSE)
        tcp_data <- generate_TCPoutput(
          l_prev_out   = tax_out$l_prev_out,
          fipscode     = s,
          policyyear   = py,
          s_cohorts    = seq(1970, 2030, by = 10),
          policy_name  = "taxes",
          scenario_tag = scenario_tag,
          write_files  = FALSE
        )
        t4 <- Sys.time()
        
        message(paste("Year:", py, 
                      "| Calc:", round(difftime(t2, t1, units="secs"), 2),
                      "| RunStates:", round(difftime(t3, t2, units="secs"), 2), 
                      "| GenOutput:", round(difftime(t4, t3, units="secs"), 2)))
        
        # Accumulate data in list (Keyed by year to be safe)
        accum_deaths[[as.character(py)]]  <- tcp_data$deaths
        accum_lyg[[as.character(py)]]     <- tcp_data$lyg
        accum_results[[as.character(py)]] <- tcp_data$results
        
        # Save to RDS List (Memory)
        run_key <- paste0("y", py, "_p", target_ppp, "_t", added_tax)
        state_results_list[[run_key]] <- list(
          policy_year = py, ppp = target_ppp, tax_add = added_tax,
          outputs = tax_out$df_mort.outputs, prevs = tax_out$df_CSprevs.by.state
        )
        
        rm(tax_out, tax_effects, tcp_data)
      } 
      
      # Write Combined CSVs (After all years done for this scenario) ---
      
      # Combine list of dataframes into one big dataframe
      final_deaths  <- do.call(rbind, accum_deaths)
      final_lyg     <- do.call(rbind, accum_lyg)
      final_results <- do.call(rbind, accum_results)
      
      # Structure: tcp_tool/AL/taxes/deaths/deaths_4.00_t0.00.csv
      base_path <- file.path("tcp_tool", abbr, "taxes")
      dir.create(file.path(base_path, "deaths"),  recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(base_path, "lyg"),     recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(base_path, "results"), recursive = TRUE, showWarnings = FALSE)
      
      readr::write_csv(final_deaths,  file.path(base_path, "deaths",  paste0("deaths_",  scenario_tag, ".csv")))
      readr::write_csv(final_lyg,     file.path(base_path, "lyg",     paste0("lyg_",     scenario_tag, ".csv")))
      readr::write_csv(final_results, file.path(base_path, "results", paste0("results_", scenario_tag, ".csv")))
      
      gc()
      message(paste("Scenario", scenario_tag, "finished in:", round(difftime(Sys.time(), t_scen_start, units = "secs"), 2), "secs"))
    }
    
    # Save RDS Once Per State
    saveRDS(list(state = s, init_price = initprice_s, results = state_results_list),
            file = file.path(tax_rds_dir, paste0(s, "_all_scenarios.rds")),
            compress = "gzip")
  } 
}


# ------------------------ Tobacco Control Expenditures Analysis -------------------------
make_tcp_out <- 0
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
  policyYear <- 2026
  dir.create(file.path(out_dir, "tcexp_scenario_rds"), recursive = TRUE, showWarnings = FALSE)
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
        
        message(paste0(fips_to_abbr(s), " (", s, ") TCExp base=", sprintf("%0.2f", base_lvl),
                       " to=", sprintf("%0.2f", lvl)))
        out_s <- runstates(s,eff$m.initiation.effect,eff$m.cessation.effect)}
      
      out_s$exp_level   <- lvl
      out_s$base_level  <- base_lvl
      out_s$already_100 <- base_lvl >= 1.0
      scenario_tag <- paste0("initexp", sprintf("%0.2f", base_lvl), "_policyexp", sprintf("%0.2f", lvl))
      
      generate_TCPoutput(
        l_prev_out   = out_s$l_prev_out,
        fipscode     = s,
        policyyear   = 2026,
        s_cohorts    = seq(1970, 2030, by = 10),
        out_root     = "tcp_tool",
        policy_name  = "tcexp",
        scenario_tag = scenario_tag
      )
      
      saveRDS(
        list(
          state = s,
          base_level = base_lvl,
          exp_level = lvl,
          df_mort.outputs = out_s$df_mort.outputs,
          df_CSprevs.by.state = out_s$df_CSprevs.by.state
        ),
        file = file.path(out_dir, "tcexp_scenario_rds", paste0(s, "_", scenario_tag, ".rds")),
        compress = "xz"
      )
      
      rm(out_s)
      gc()

}}}

## ----------------------- Smoke-free Air Laws Analysis -------------------------

airlaws_out_list <- list()
df_mortality.out <- data.frame()
df_prev.by.state <- data.frame()
l_combined_state_prev <- list()

##    pacwp = workplace, pacr = restaurant, pacb = bar
df_airlaws <- read.csv("state_airlaws/airlaws_by_state_2025.csv",stringsAsFactors = FALSE) %>%  # State, pacwp, pacr, pacb (0–100)
  mutate(State = toupper(trimws(State)),
         fips_code = fips(State, to = "FIPS"),
         default_pacwp = default_pacwp,
         default_pacr = default_pacr,
         default_pacb = default_pacb)
          
names(df_airlaws)

if (airlaws_policy == 1) {
  
  message("Running smoke‑free air laws analysis")
  policyYear <- 2026                                    
  dir.create(file.path(out_dir, "air_scenario_rds"), recursive = TRUE, showWarnings = FALSE)
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
      
      message(paste0(fips_to_abbr(s), " (", s, ") Air wp=", Iwp, " r=", Ir, " b=", Ib))
      out_s <- runstates(s, eff$m.initiation.effect, eff$m.cessation.effect)
  
      scenario_name <- sprintf("%s_air_wp%d_r%d_b%d", s, Iwp, Ir, Ib)
      scenario_tag <- paste0(
        "w", Iwp, "_r", Ir, "_b", Ib,
        "_w", sprintf("%0.2f", pacwp),
        "_r", sprintf("%0.2f", pacr),
        "_b", sprintf("%0.2f", pacb)
      )
      
      
      generate_TCPoutput(
        l_prev_out   = out_s$l_prev_out,
        fipscode     = s,
        policyyear   = policyYear,
        s_cohorts    = seq(1970, 2030, by = 10),
        out_root     = "tcp_tool",
        policy_name  = "airlaws",
        scenario_tag = scenario_tag
      )
      
      saveRDS(
        list(
          state = s,
          pacwp = pacwp, pacr = pacr, pacb = pacb,
          Iwp = Iwp, Ir = Ir, Ib = Ib,
          df_mort.outputs = out_s$df_mort.outputs,
          df_CSprevs.by.state = out_s$df_CSprevs.by.state
        ),
        file = file.path(out_dir, "air_scenario_rds", paste0(s, "_", scenario_tag, ".rds")),
        compress = "xz"
      )
      
      rm(out_s, eff)
      gc()
    }
  }
}


# ------------------------ Graphic health warnings_2022 -----------------------

# Cigarette packs will be required to display graphic health warnings effective October 11th, 2022.
# 
# warnings_out_list <- list()
# df_mortality.out <- data.frame()
# df_prev.by.state <- data.frame()
# l_combined_state_prev <- list()
# 
# if (warnings_policy == 1) {
#   
#   message("Running graphic‑health‑warning analysis")
# 
#   policyYear <- 2022 # first year with health warnings
#   startbc <- 1908
#   endyear <- 2200
#   
#   init_vec <- c(0.00, 0.05, 0.10, 0.15)  
#   cess_vec <- c(0.00, 0.25, 0.50, 0.75) 
#   
#   scen_tbl <- expand.grid(init.reduce  = init_vec, cess.increase = cess_vec, KEEP.OUT.ATTRS = FALSE)
# 
#   for (s in v_statefips) {            
#     for (ix in seq_len(nrow(scen_tbl))) {
#       
#       init.red <- scen_tbl$init.reduce[ix]
#       cess.inc <- scen_tbl$cess.increase[ix]
#     
#       eff <- warnings_effectCalculation(init.reduce = init.red, cess.increase = cess.inc,
#                                         startbc = startbc, endyear = endyear, policyYear = policyYear)
#                                     
#       print(paste0(fips(s, to = "Abbreviation"), " (", s, ") - graphic‑health‑warning analysis"))
#       
#       out_s <- runstates(s, eff$m.initiation.effect, eff$m.cessation.effect)
#       
#       scen_code <- sprintf("%s_warn_i%03d_c%03d", s, round(init.red  * 100), round(cess.inc * 100))
#       
#       out_s$init.red <- init.red
#       out_s$cess.inc <- cess.inc
#       out_s$policyYr <- policyYear
#       
#       warnings_out_list[[scen_code]] <- out_s
#       df_mortality.out <- rbind(df_mortality.out, out_s$df_mort.outputs)
#       df_prev.by.state <- rbind(df_prev.by.state, out_s$df_CSprevs.by.state)
#                                              
#       if (is.null(l_combined_state_prev[[s]]))
#         l_combined_state_prev[[s]] <- list()
#       l_combined_state_prev[[s]][[scen_code]] <- out_s$l_prev_out
#     }
#   }
# }
# 
# # SAVE
# file_rds <- file.path(out_dir, "warnings_out_list.rds")
# saveRDS(warnings_out_list, file = file_rds, compress = "xz")  


# ------------------------ Graphic health warnings_2025 -----------------------
# warnings_out_list_2025 <- list()
# df_mortality.out <- data.frame()
# df_prev.by.state <- data.frame()
# l_combined_state_prev <- list()
# 
# if (warnings_policy == 1) {
#   
#   message("Running graphic‑health‑warning analysis")
#   
#   policyYear <- 2025 # first year with health warnings
#   startbc <- 1908
#   endyear <- 2200
#   
#   init_vec <- c(0.00, 0.05, 0.10, 0.15)  
#   cess_vec <- c(0.00, 0.25, 0.50, 0.75) 
#   
#   scen_tbl <- expand.grid(init.reduce  = init_vec, cess.increase = cess_vec, KEEP.OUT.ATTRS = FALSE)
#   
#   for (s in v_statefips) {            
#     for (ix in seq_len(nrow(scen_tbl))) {
#       
#       init.red <- scen_tbl$init.reduce[ix]
#       cess.inc <- scen_tbl$cess.increase[ix]
#       
#       eff <- warnings_effectCalculation(init.reduce = init.red, cess.increase = cess.inc,
#                                         startbc = startbc, endyear = endyear, policyYear = policyYear)
#       
#       print(paste0(fips(s, to = "Abbreviation"), " (", s, ") - graphic‑health‑warning analysis"))
#       
#       out_s <- runstates(s, eff$m.initiation.effect, eff$m.cessation.effect)
#       
#       scen_code <- sprintf("%s_warn_i%03d_c%03d", s, round(init.red  * 100), round(cess.inc * 100))
#       
#       out_s$init.red <- init.red
#       out_s$cess.inc <- cess.inc
#       out_s$policyYr <- policyYear
#       
#       warnings_out_list_2025[[scen_code]] <- out_s
#       df_mortality.out <- rbind(df_mortality.out, out_s$df_mort.outputs)
#       df_prev.by.state <- rbind(df_prev.by.state, out_s$df_CSprevs.by.state)
#       
#       if (is.null(l_combined_state_prev[[s]]))
#         l_combined_state_prev[[s]] <- list()
#       l_combined_state_prev[[s]][[scen_code]] <- out_s$l_prev_out
#     }
#   }
# }
# 
# # SAVE
# file_rds <- file.path(out_dir, "warnings_out_list_2025.rds")
# saveRDS(warnings_out_list_2025, file = file_rds, compress = "xz") 






