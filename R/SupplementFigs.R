### Generate supplementary figures for tax effects on smoking prevalence and mortality
setwd("/Users/wangmengyao/Documents/GitHub/tcp-model-code/")

library(ggpubr)
library(stringr)
library(readr)
library(openxlsx)
library(reshape2)
library(dplyr)
library(grid)
library(gridBase)
library(gridExtra)
library(ggplot2)
library(cdlTools)

# Set constants
ages <- 18.99
minage <- 18
maxage <- 99
labellist <- c('A.', 'B.', 'C.', 'D.')

# Define state FIPS codes for all states
statefips <- c('01','02','04','05','06','08','09','10','11','12','13','15','16',
               '17','18','19','20','21','22','23','24','25','26','27','28','29',
               '30','31','32','33','34','35','36','37','38','39','40','41','42',
               '44','45','46','47','48','49','50','51','53','54','55','56')

# For testing, you can use a smaller set of states
# statefips <- c('06', '21', '25', '55')  # CA, KY, MA, WI

# Date variable for file naming
date_variable <- format(Sys.Date(), "%m.%d.%y")

# Load state population data from Census
statetotals <- read.xlsx('data-raw/Census State Population Total 2003-2023.xlsx', sheet='statetotals')
statetotals$fips <- fips(statetotals$statename, to="FIPS")
statetotals <- statetotals[-1,]

# Load tax policy data
load("data/tax_policy_inputs.RData.Rda")

# Load model output with tax effects
load(paste0('output/model_output_', date_variable, '.RData'))

#---------------------- Figure Functions --------------------------------------------------------

# Baseline vs Tax smoking prevalence - overall comparison
plot_tax_prevalence <- function(fipscode, labelval) {
  # Get state-specific data for adults
  thisstate <- subset(df_prev.by.state, state == fipscode & gender == "Both" & age == "18.99")
  thisstate$policy.scen <- factor(thisstate$policy.scen, levels=c("baseline", "tax"))
  
  # Create plot
  fig <- ggplot(data = thisstate) + 
    geom_line(aes(x = year, y = prev*100, color = policy.scen), lwd = 0.8) +
    scale_color_manual(name = "Scenario",
                       labels = c("Baseline", "With Tax"),
                       values = c("#0072B2", "#E69F00")) +
    scale_y_continuous(name = "Smoking Prevalence (%)", 
                       limits = c(0, 30), 
                       breaks = seq(0, 30, 5)) +
    scale_x_continuous(name = "Year", 
                       limits = c(2025, 2100), 
                       breaks = seq(2030, 2100, 10)) +
    theme_light() + 
    theme(legend.text = element_text(size = 9),
          text = element_text(size = 9),
          axis.text.x = element_text(angle = 60, hjust = 1),
          legend.background = element_rect(fill = alpha('white', 0.4)),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right") +
    labs(title = paste0(labelval, ' Smoking prevalence with tax effects, ages 18-99'))
  
  return(fig)
}

# Tax effect on smoking prevalence by gender
plot_tax_prevalence_by_gender <- function(fipscode, labelval) {
  # Get state-specific data with gender breakdown
  thisstate <- subset(df_prev.by.state, state == fipscode & age == "18.99" & (gender == "Men" | gender == "Women"))
  thisstate$policy.scen <- factor(thisstate$policy.scen, levels=c("baseline", "tax"))
  
  # Create plot
  fig <- ggplot(data = thisstate) + 
    geom_line(aes(x = year, y = prev*100, color = gender, linetype = policy.scen), lwd = 0.8) +
    scale_color_manual(name = "Gender",
                       values = c("Men" = "#0072B2", "Women" = "#D55E00")) +
    scale_linetype_manual(name = "Scenario",
                          values = c("baseline" = "solid", "tax" = "dashed"),
                          labels = c("Baseline", "With Tax")) +
    scale_y_continuous(name = "Smoking Prevalence (%)", 
                       limits = c(0, 30), 
                       breaks = seq(0, 30, 5)) +
    scale_x_continuous(name = "Year", 
                       limits = c(2025, 2100), 
                       breaks = seq(2030, 2100, 10)) +
    theme_light() + 
    theme(legend.text = element_text(size = 9),
          text = element_text(size = 9),
          axis.text.x = element_text(angle = 60, hjust = 1),
          legend.background = element_rect(fill = alpha('white', 0.4)),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right") +
    labs(title = paste0(labelval, ' Tax effects by gender, ages 18-99'))
  
  return(fig)
}

# Tax effect on smoking prevalence by age group
plot_tax_prevalence_by_age <- function(fipscode, labelval) {
  # Get state-specific data with age breakdown
  thisstate <- subset(df_prev.by.state, state == fipscode & gender == "Both")
  thisstate$policy.scen <- factor(thisstate$policy.scen, levels=c("baseline", "tax"))
  
  # Create plot
  fig <- ggplot(data = thisstate) + 
    geom_line(aes(x = year, y = prev*100, color = age, linetype = policy.scen), lwd = 0.8) +
    scale_linetype_manual(name = "Scenario",
                          values = c("baseline" = "solid", "tax" = "dashed"),
                          labels = c("Baseline", "With Tax")) +
    scale_y_continuous(name = "Smoking Prevalence (%)", 
                       limits = c(0, 30), 
                       breaks = seq(0, 30, 5)) +
    scale_x_continuous(name = "Year", 
                       limits = c(2025, 2100), 
                       breaks = seq(2030, 2100, 10)) +
    theme_light() + 
    theme(legend.text = element_text(size = 9),
          text = element_text(size = 9),
          axis.text.x = element_text(angle = 60, hjust = 1),
          legend.background = element_rect(fill = alpha('white', 0.4)),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right") +
    labs(title = paste0(labelval, ' Tax effects by age group'))
  
  return(fig)
}

# Tax effect on mortality (smoking-attributable deaths averted)
plot_tax_mortality <- function(fipscode, labelval) {
  # Get state-specific mortality data
  thisstate <- subset(df_mort.policy, state == fipscode & gender == "Both")
  thisstate$policy.scen <- factor(thisstate$policy.scen, levels=c("baseline", "tax"))
  
  # Create plot
  fig <- ggplot(data = thisstate) + 
    geom_line(aes(x = year, y = SADsAvertedcum, color = policy.scen), lwd = 0.8) +
    scale_color_manual(name = "Scenario",
                       labels = c("Baseline", "With Tax"),
                       values = c("#0072B2", "#E69F00")) +
    scale_y_continuous(name = "Cumulative Deaths Averted", 
                       labels = scales::comma,
                       breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(name = "Year", 
                       limits = c(2025, 2100), 
                       breaks = seq(2030, 2100, 10)) +
    theme_light() + 
    theme(legend.text = element_text(size = 9),
          text = element_text(size = 9),
          axis.text.x = element_text(angle = 60, hjust = 1),
          legend.background = element_rect(fill = alpha('white', 0.4)),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right") +
    labs(title = paste0(labelval, ' Cumulative smoking-attributable deaths averted'))
  
  return(fig)
}

# Function to create a tax effects summary table
create_tax_summary_table <- function(fipscode, labelval) {
  # Get state-specific data for year 2100
  mortality_data <- subset(df_mort.policy, state == fipscode & gender == "Both" & year == 2100)
  prevalence_data <- subset(df_prev.by.state, state == fipscode & gender == "Both" & age == "18.99" & year == 2100)
  
  # Calculate metrics
  baseline_prevalence <- prevalence_data$prev[prevalence_data$policy.scen == "baseline"]
  tax_prevalence <- prevalence_data$prev[prevalence_data$policy.scen == "tax"]
  prevalence_reduction <- baseline_prevalence - tax_prevalence
  percent_reduction <- (prevalence_reduction / baseline_prevalence) * 100
  
  deaths_averted <- mortality_data$SADsAvertedcum[mortality_data$policy.scen == "tax"]
  life_years_gained <- mortality_data$LYGcum[mortality_data$policy.scen == "tax"]
  
  # Format numeric values
  baseline_prev_formatted <- sprintf("%.2f%%", baseline_prevalence * 100)
  tax_prev_formatted <- sprintf("%.2f%%", tax_prevalence * 100)
  reduction_formatted <- sprintf("%.2f%%", percent_reduction)
  deaths_formatted <- format(round(deaths_averted), big.mark = ",")
  lyg_formatted <- format(round(life_years_gained), big.mark = ",")
  
  # Create table data
  df <- data.frame(
    Metric = c("Baseline smoking prevalence (2100)", 
               "Tax scenario smoking prevalence (2100)", 
               "Percent reduction in prevalence",
               "Cumulative deaths averted by 2100",
               "Cumulative life years gained by 2100"),
    Value = c(baseline_prev_formatted, 
              tax_prev_formatted, 
              reduction_formatted,
              deaths_formatted,
              lyg_formatted)
  )
  
  # Get size of the state population in 2023 based on Census Bureau data
  popsize <- format(statetotals[statetotals$fips == as.numeric(fipscode), "2023"], big.mark = ",", scientific = FALSE)
  
  # Create table visualization
  g <- ggtexttable(df, theme = ttheme("light", base_size = 9))
  g <- g %>%
    tab_add_title(text = paste0(labelval, " Tax effects summary for ", fips(fipscode, to = 'Name')), 
                 face = "bold", size = 10) %>%
    tab_add_footnote(text = paste0("2023 Census population estimate: ", popsize), 
                    size = 8)
  
  return(g)
}

# Generate supplementary figures and save them
fignum <- 1  # Starting figure number

# Create directory for supplementary figures if it doesn't exist
if(!dir.exists("figs/Supp_figs")) {
  dir.create("figs/Supp_figs", recursive = TRUE)
}

# Loop through all states to create and save figures
for (s in statefips) {
  # Create a 2x2 arrangement of plots
  plot <- ggarrange(
    create_tax_summary_table(s, labellist[1]), 
    plot_tax_prevalence(s, labellist[2]),
    plot_tax_prevalence_by_gender(s, labellist[3]),
    plot_tax_mortality(s, labellist[4]),
    ncol = 2, nrow = 2
  )
  
  # Add title with state name
  plot <- annotate_figure(plot, 
                         top = text_grob(paste0("Supplementary Figure ", fignum, ". ", 
                                               fips(as.numeric(s), to = "Name"), 
                                               " Tax Effect Model Outcomes"), 
                                        face = "bold", size = 12)
  )
  
  # Save the figure
  jpeg(filename = paste0("figs/Supp_figs/", sprintf("%03d", fignum), ".jpg"), 
       width = 11, height = 8.5, units = "in", res = 1000)
  print(plot)
  dev.off()
  
  # Increment figure number
  fignum <- fignum + 1
}

