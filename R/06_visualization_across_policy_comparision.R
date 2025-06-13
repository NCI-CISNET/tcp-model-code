
# Across-Policy Comparison of Tobacco Control Impacts

# Key Outputs:
# 1. US Map + table: Displays baseline state-level smoking prevalence in 2025.
# 2. Deaths Averted: Calculates the number of deaths averted per state under full implementation of all policies.
# 3. 3x3 Grid Visualization: For each state, generates a 3x3 layout showing policy effects on:
#    - Rows: Smoking Prevalence Reduction, Life-Years Gained (LYG), and Deaths Averted
#    - Columns: Policy scenarios—Tax Increase, Smoke-Free Air Laws, and Expenditure Increase
# 4. Summary Table of Baseline Policy Parameters by State

library(dplyr)
library(tibble)
library(ggpubr)
library(knitr)
library(cowplot)
library(ggplot2)
library(readr)
library(purrr)
library(scales)
library(fips)
library(tidycensus)
library(cdlTools)

labellist=c('A.', 'B.', 'C.', 'D.', 'E.', 'F.', 'G', 'H', 'I')
v_statefips=c('01','02','04','05','06','08','09','10','11','12','13','15','16',
              '17','18','19','20','21','22','23','24','25','26','27','28','29',
              '30','31','32','33','34','35','36','37','38','39','40','41','42',
              '44','45','46','47','48','49','50','51','53','54','55','56')
state.abb.full <- c(state.abb, "DC")
state.name.full <- c(state.name, "District of Columbia")

# Load Main Analysis results
baseline_results <- readRDS("analysis_output/baseline_results.rds")
tax_out_list <- readRDS("analysis_output/tax_out_list.rds")
airlaws_out_list <- readRDS("analysis_output/airlaws_out_list.rds")
tcexp_out_list <- readRDS("analysis_output/tcexp_out_list.rds")
warnings_out_list <- readRDS("analysis_output/warnings_out_list.rds")
warnings_out_list_2025 <- readRDS("analysis_output/warnings_out_list_2025.rds")

# ---------- US Map: Smoking Prevalence 2025 ------------------------------------


df_prev <- bind_rows(lapply(names(baseline_results), function(fips) {
  df <- baseline_results[[fips]]$df_CSprevs.by.state
  df <- df %>% filter(gender == "Both", year == 2025) %>% 
    summarise(prev = mean(prev, na.rm = TRUE)) %>%
    mutate(state_abbr = unique(df$abbr),
           state_name = tolower(state.name.full[match(unique(df$abbr), state.abb.full)]))
  return(df)}))

us_map <- map_data("state")

map_df <- left_join(us_map, df_prev, by = c("region" = "state_name"))

centroids <- map_df %>%
  group_by(region) %>%
  summarise(long = mean(range(long)), lat = mean(range(lat))) %>%
  left_join(df_prev, by = c("region" = "state_name"))

ggplot(map_df, aes(long, lat, group = group, fill = prev)) +
  geom_polygon(color = "white", size = 0.2) +
  geom_text(data = centroids, aes(x = long, y = lat, label = state_abbr), inherit.aes = FALSE,
            color = "white", size = 2, fontface = "bold") +
  coord_fixed(1.3) +
  scale_fill_gradient(
    name = "Smoking Prevalence",
    low = "#e6f0ff", high = "#084594",na.value = "grey90",labels = scales::percent_format(accuracy = 1) ) + 
  labs(title = "Baseline Smoking Prevalence by U.S. State, 2025", x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.grid = element_blank(), legend.position = "right")

ggsave("figs/across_policy_comparison/us_smoking_prevalence_map_2025.jpeg", width = 10, height = 6, dpi = 300)

# ---------- Table: Smoking Prevalence 2025 ------------------------------------

df_prev <- bind_rows(lapply(names(baseline_results), function(fips) {
  df <- baseline_results[[fips]]$df_CSprevs.by.state
  df <- df %>% filter(gender == "Both", year == 2025) %>% 
    summarise(prev = mean(prev, na.rm = TRUE)) %>%
    mutate(state_abbr = unique(df$abbr),
           state_name = tolower(state.name.full[match(unique(df$abbr), state.abb.full)]))
  return(df)}))

smoking_prevalence_table_plot <- function(df_prev) {
  table_df <- df_prev %>%
    mutate(
      State = tools::toTitleCase(state_name),
      `Smoking Prevalence (2025)` = scales::percent(prev, accuracy = 0.1)) %>%
    select(State, `Smoking Prevalence (2025)`, prev) %>%
    arrange(desc(prev)) %>%
    select(-prev)
  
  tbl <- ggtexttable(table_df, rows = NULL,theme = ttheme("light", base_size = 8))
  
  title <- ggdraw() + draw_label("Baseline Smoking Prevalence by State (2025)", size = 12, hjust = 0.5, vjust = 0.5)
  
  final_plot <- plot_grid(title, tbl, ncol = 1, rel_heights = c(0.1,1))
  return(final_plot)
}

g <- smoking_prevalence_table_plot(df_prev)
ggsave("figs/across_policy_comparison/us_smoking_prevalence_table_2025.jpg", g, width = 6, height = 13, dpi = 600)

# ---------- Deaths averted in each state under full implementation of all policies ----
# -- Results are sorted in descending order

plot_deaths_averted_stacked <- function(fips_vec, 
                                        tax_list = tax_out_list,
                                        tcexp_list = tcexp_out_list,
                                        air_list = airlaws_out_list) {
  
  abbr_to_name <- function(ab) state.name[match(ab, state.abb)]
  
  df_all <- bind_rows(lapply(fips_vec, function(fips) {
    key_tax  <- sprintf("%s_3dollar", fips)
    key_exp  <- sprintf("%s_exp100", fips)
    key_air  <- sprintf("%s_air_wp1_r1_b1", fips)
    
    state_abbr <- tax_list[[sprintf("%s_1dollar", fips)]]$df_mort.outputs$abbr[1]
    state_name <- state_abbr
    if (is.na(state_name)) return(NULL)
    
    bind_rows(
      tax_list[[key_tax]]$df_mort.outputs  |> filter(gender == "Both", year == 2100) |>
        transmute(State = state_name, Policy = "Cigarette Taxes: Increase $3", DeathsAverted = SADsAvertedcum),
      tcexp_list[[key_exp]]$df_mort.outputs |> filter(gender == "Both", year == 2100) |>
        transmute(State = state_name, Policy = "Tobacco Control Expenditures: 100% of CDC recommendation level", DeathsAverted = SADsAvertedcum),
      air_list[[key_air]]$df_mort.outputs   |> filter(gender == "Both", year == 2100) |>
        transmute(State = state_name, Policy = "Smoke-free Air Laws: 100% coverage in workplaces, restaurant, bar", DeathsAverted = SADsAvertedcum))}))
  
  # Sort states by total deaths averted (optional)
  state_order <- df_all %>%
    group_by(State) %>%
    summarise(total = sum(DeathsAverted, na.rm = TRUE)) %>%
    arrange(desc(total)) %>%
    pull(State)
  
  df_all$State <- factor(df_all$State, levels = state_order)
  
  ggplot(df_all, aes(x = State, y = DeathsAverted, fill = Policy)) +
    geom_bar(stat = "identity", position = "stack") +
    labs( title = "Deaths Averted from Combined Policy Implementation by State",
          y = "Smoking-attributable Deaths Averted", x = NULL) +
    scale_fill_manual(values = c("Cigarette Taxes: Increase $3" = "#1b9e77", "Tobacco Control Expenditures: 100% of CDC recommendation level" = "#d95f02", "Smoke-free Air Laws: 100% coverage in workplaces, restaurant, bar" = "#7570b3")) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), axis.text.x = element_text(hjust = 1))}

fig_stacked <- plot_deaths_averted_stacked(v_statefips)
ggsave("figs/across_policy_comparison/deaths_averted_stacked.jpeg", fig_stacked, width = 22, height = 8, dpi = 300)



# ---------- Generate a 3x3 grid for all states: -------------------------------------
#      Rows represent outcomes: Prevalence Reduction, Life-Years Gained (LYG), Deaths Averted;
#      Columns represent policies: Tax Increase, Smoke-Free Air Laws, Tobacco Control Expenditures.

strip_title <- function(plot_obj) {
  if (is.null(plot_obj)) return(NULL)
  df  <- plot_obj$data
  pal <- NULL
  
  if ("tax_scen" %in% names(df)) {
    df  <- df %>% filter(tax_scen %in% c("1dollar", "2dollar", "3dollar"))
    pal <- c("1dollar" = "#1b9e77","2dollar" = "#d95f02",  "3dollar" = "#7570b3")
    plot_obj <- plot_obj %+% df +
      scale_colour_manual(values = pal, labels = c("$1", "$2", "$3"), name   = NULL)
    
  } else if ("scenario" %in% names(df) && all(grepl("^wp", df$scenario))) {
    allowed <- c("wp1_r0_b0", "wp1_r1_b0", "wp1_r1_b1")
    pal <- c("wp1_r0_b0" = "#1b9e77", "wp1_r1_b0" = "#d95f02", "wp1_r1_b1" = "#7570b3")
    df <- df %>% filter(scenario %in% allowed) %>%
      mutate(scenario = factor(scenario, levels = allowed)) %>%
      arrange(match(scenario, allowed))
    plot_obj <- plot_obj %+% df + scale_colour_manual(values = pal, drop = FALSE, name = NULL)
    
  } else {
    exp_col <- intersect(c("exp_scen", "scenario"), names(df))
    if (length(exp_col) == 1) {
      df <- df %>% filter(grepl("^exp(60|80|100)(_diff)?$", .data[[exp_col]])) %>%
        mutate(exp_scen = sub("_diff$", "", .data[[exp_col]]),
               exp_scen = factor(exp_scen, levels = c("exp60","exp80","exp100")))
      levels_vec <- c("exp60","exp80","exp100")
      pal <- setNames(c("#1b9e77","#d95f02","#7570b3"),levels_vec)
      legend_labs <- paste0(sub("^exp","",levels_vec), "%")
      
      plot_obj <- plot_obj %+% df +
        scale_colour_manual(values = pal, breaks = levels_vec, labels = legend_labs,name   = NULL)}}
  plot_obj + labs(title = NULL)
}

# 1. Prevalence Reduction
PrevRed_tax <- function(fip) {
  strip_title(plot_PrevRed_tax(fip)) +
    ylab("Prevalence reduction (pp)") +
    theme(axis.title.y = element_text(face = "bold"))}

PrevRed_airlaws <- function(fip) {
  strip_title(plot_PrevRed_airlaws(fip)) +
    ylab(NULL)}

PrevRed_tcexp <- function(fip, out_list_diff = tcexp_prevDiff) {
  strip_title(plot_PrevRed_tcexp(fip, out_list_diff)) +
    ylab(NULL)}

# 2. Life-years Gained
LYG_tax <- function(fip) {
  strip_title(plot_LYG_tax(fip)) +
    ylab("Life-years Gained") +
    theme(legend.position = "none",
          axis.title.y = element_text(face = "bold"))}

LYG_airlaws <- function(fip) {
  strip_title(plot_LYG_airlaws(fip)) +
    ylab(NULL) +
    theme(legend.position = "none")}

LYG_tcexp <- function(fip, out_list = tcexp_out_list) {
  strip_title(plot_LYG_tcexp(fip, out_list)) +
    ylab(NULL) +
    theme(legend.position = "none")}

# 3. Deaths Averted
DeathAverted_tax <- function(fip) {
  strip_title(plot_DeathAvoided_tax(fip)) +
    ylab("Deaths avoided (cumulative)") +
    theme(legend.position = "none",
          axis.title.y = element_text(face = "bold"))}

DeathAverted_airlaws <- function(fip) {
  strip_title(plot_DeathAvoided_airlaws(fip)) +
    ylab(NULL) +
    theme(legend.position = "none")}

DeathAverted_tcexp <- function(fip, out_list = tcexp_out_list) {
  strip_title(plot_DeathAvoided_tcexp(fip, out_list)) +
    ylab(NULL) +
    theme(legend.position = "none")}

make_snapshot <- function(panels, title_txt, tag_vec = labellist,
                          col_titles = c("Tax Increase","Smoke-free Air Laws", "Tobacco Control Expenditures"),
                          row_titles = c("Prevalence Reduction","Life-years Gained","Deaths Averted")) {
  
  panels_tagged <- map2(panels, tag_vec,
                        \(p, tag) p + labs(tag = tag))
  
  rows <- vector("list", 3)
  for (i in 1:3) {
    rows[[i]] <- plot_grid(
      plotlist = panels_tagged[((i - 1) * 3 + 1):(i * 3)],
      ncol = 3, align = "hv")}
  body <- plot_grid(plotlist = rows, ncol = 1, align = "hv")
  
  header <- plot_grid(
    ggdraw() + draw_label(col_titles[1], fontface = "bold", size = 12, hjust = 0.5) + theme(plot.margin = margin(b = 0)),
    ggdraw() + draw_label(col_titles[2], fontface = "bold", size = 12, hjust = 0.5) + theme(plot.margin = margin(b = 0)),
    ggdraw() + draw_label(col_titles[3], fontface = "bold", size = 12, hjust = 0.5) + theme(plot.margin = margin(b = 0)),
    ncol = 3, rel_widths = c(1, 1, 1))
  
  core <- plot_grid(header, body, ncol = 1, rel_heights = c(0.06, 1))
  
  plot_grid(ggdraw() + draw_label(title_txt, fontface = "bold", size = 15, hjust = 0.5),
            core, ncol = 1, rel_heights = c(0.03, 0.94))}

fignum <- 1
for (s in v_statefips) {
  state_name <- fips(as.numeric(s), to = "Name")
  
  p_tax_prev <- PrevRed_tax(s)
  p_tax_lyg <- LYG_tax(s)
  p_tax_death <- DeathAverted_tax(s)
  
  lim_prev <- max(p_tax_prev$data$prev_red_pp, na.rm = TRUE)
  lim_lyg <- max(p_tax_lyg$data$LYGcum, na.rm = TRUE)
  lim_death <- max(p_tax_death$data$SADsAvertedcum, na.rm = TRUE)
  
  row1 <- list(p_tax_prev, PrevRed_airlaws(s) +
                 scale_y_continuous(limits = c(0, lim_prev),
                                    labels = \(x) sprintf("%.1f pp", x)), PrevRed_tcexp(s, tcexp_prevDiff) +
                 scale_y_continuous(limits = c(0, lim_prev),
                                    labels = \(x) sprintf("%.1f pp", x)))
  row2 <- list(p_tax_lyg, LYG_airlaws(s) +
                 scale_y_continuous(limits = c(0, lim_lyg), labels = comma) + 
                 theme(legend.position = "none"),
               LYG_tcexp(s, tcexp_out_list) +
                 scale_y_continuous(limits = c(0, lim_lyg), labels = comma) +
                 theme(legend.position = "none"))
  row3 <- list(p_tax_death,
               DeathAverted_airlaws(s) +
                 scale_y_continuous(limits = c(0, lim_death),
                                    labels = comma) +
                 theme(legend.position = "none"),
               DeathAverted_tcexp(s, tcexp_out_list) +
                 scale_y_continuous(limits = c(0, lim_death),
                                    labels = comma) +
                 theme(legend.position = "none"))
  
  panels <- c(row1, row2, row3)
  title_txt <- sprintf("eFigure %d. %s — Across Policy Comparison", fignum, state_name)
  page <- make_snapshot(panels, title_txt)
  
  jpeg(sprintf("figs/across_policy_comparison/grid/%s_%s_AcrossPolicy.jpg",s, gsub(" ", "_", state_name)),
       width = 11, height = 11, units = "in", res = 600)
  print(page)
  dev.off()
  fignum <- fignum + 1 }


# ---------- Summary Table of Baseline Policy Parameters by State --------------

df_prices <- read.csv("state_tax/cigarette_prices_by_state_2025.csv", stringsAsFactors = FALSE) %>%
  mutate(State = toupper(trimws(State))) %>%
  mutate(fips_code = sprintf("%02d", sapply(State, function(x) fips(x, to = "FIPS"))))

df_exp <- read.csv("state_tcexp/tcexp_by_state_2025.csv", stringsAsFactors = FALSE) %>%
  mutate(State = toupper(trimws(State)), fips_code = sprintf("%02d", sapply(State, function(x) fips(x, to = "FIPS"))),
         default_level = parse_number(default_level) / 100)

df_airlaws <- read.csv("state_airlaws/airlaws_by_state_2025.csv", stringsAsFactors = FALSE) %>% 
  mutate(State = toupper(trimws(State)),
         fips_code = sprintf("%02d", sapply(State, function(x) fips(x, to = "FIPS"))),
         pacwp_workers = parse_number(pacwp_workers) / 100,
         pacb = parse_number(pacb) / 100,
         pacr = parse_number(pacr) / 100 ) %>% select(-fipscode)

extract_state_row <- function(fipscode) {
  state_name <- fips(fipscode, to = "Name")
  
  price <- df_prices %>% filter(fips_code == fipscode) %>%
    pull(price_per_pack_incl_taxes) %>%
    { sprintf("$%.2f", as.numeric(gsub("[^0-9.]", "", .[1]))) }
  
  cdc_pct <- df_exp %>% filter(fips_code == fipscode) %>%
    pull(percent_cdc) %>%
    { paste0(round(as.numeric(gsub("[^0-9.]", "", .[1]))), "%") }
  
  air_raw <- df_airlaws %>% filter(fips_code == fipscode)
  air <- if (nrow(air_raw) >= 1) air_raw[1, ] else tibble(pacwp_workers = 0, pacb = 0, pacr = 0)
  
  pacwp <- percent(air$pacwp_workers, accuracy = 1)
  pacb <- percent(air$pacb, accuracy = 1)
  pacr <- percent(air$pacr, accuracy = 1)
  
  pac_values <- paste0(pacwp, " / ", pacb, " / ", pacr)
  
  tibble(State = state_name, 
         `Cigarette taxes (*price_per_pack_inc_tax*)` = price,
         `Tobacco expenditures (*% of CDC rec.*)` = cdc_pct,
         `Smoke-free air laws (*pacwp / pacb / pacr*)` = pac_values)}

baseline_summary_table <- function(target_fips, df_prices, df_exp, df_airlaws) {
  summary_df <- map_dfr(target_fips, extract_state_row)
  colnames(summary_df) <- c(
    "State",
    "Cigarette taxes\n(price_per_pack_inc_tax)",
    "Tobacco expenditures\n(% of CDC rec.)",
    "Smoke-free air laws\n(pacwp / pacb / pacr)")
  
  note_text <- paste0(
    "Notes: pacwp = % coverage of workplace smoke-free laws (0–100%)\n",
    "pacb = % coverage of bar smoke-free laws (0–100%)\n",
    "pacr = % coverage of restaurant smoke-free laws (0–100%)\n",
    "% of CDC rec = initial spending as % of CDC recommendation (0–100%)")
  
  tbl <- ggtexttable(summary_df, rows = NULL, theme = ttheme("light", base_size = 8)) %>%
    tab_add_footnote(text = note_text, size = 7)
  
  title <- ggdraw() + draw_label("Baseline Policy Parameters Across States",
                                 fontface = "bold", size = 15, hjust = 0.5, vjust = 0.5) 
  
  page <- plot_grid(title, tbl, ncol = 1, rel_heights = c(0.1, 1))
  
  return(page)}

g <- baseline_summary_table(v_statefips, df_prices, df_exp, df_airlaws)
ggsave("figs/across_policy_comparison/us_baseline_parameters_table.jpg", g, width = 8.5, height = 15, dpi = 600)


