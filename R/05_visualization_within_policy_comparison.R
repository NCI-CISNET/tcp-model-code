
# Within-policy comparison for each tobacco control policy:
# cigarette tax, smoke-free air laws, and tobacco control expenditures.

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(stringr)
library(purrr)

#-------- Cigarette tax:stacked mortality reduction bar plots all states ---------------- 
parse_tax_name <- function(x) {
  tibble(fipscode = str_extract(x, "^[0-9]{2}"),
         tax_scen = str_extract(x, "(?<=_)[0-9]+dollar"))}

tax_mort_df <- imap_dfr(tax_out_list, function(el, nm) {
  meta <- parse_tax_name(nm)
  el[["df_mort.outputs"]] |>
    filter(year == 2100) |>             
    mutate(fipscode = meta$fipscode,tax_scen  = meta$tax_scen) }) |>
  filter(tax_scen %in% c("1dollar", "2dollar", "3dollar"), gender == "Both")              

tax_abs <- tax_mort_df |>
  group_by(abbr, tax_scen) |>
  summarise(SADsAvertedcum = sum(SADsAvertedcum), .groups = "drop") |>
  pivot_wider(names_from = tax_scen, values_from = SADsAvertedcum) |>
  mutate(incr_1 = `1dollar`, incr_2 = `2dollar` - `1dollar`, incr_3 = `3dollar` - `2dollar`, total  = `3dollar`) |>
  select(abbr, incr_1, incr_2, incr_3, total) |>
  pivot_longer(cols = starts_with("incr"), names_to  = "step",values_to = "SADsAverted") |>
  mutate(step = factor(step, levels = c("incr_3", "incr_2", "incr_1"),
                       labels = c("$3", "$2", "$1")))

tax_rel <- tax_mort_df |> group_by(abbr, tax_scen) |>
  summarise(SADsAvertedcum = sum(SADsAvertedcum), SADbase = sum(SADcum),.groups = "drop") |>
  pivot_wider(names_from = tax_scen, values_from = SADsAvertedcum) |>
  mutate(across(c(`1dollar`,`2dollar`,`3dollar`), ~replace_na(.x, 0))) |>
  mutate( pct_1 = 100 * `1dollar` / SADbase, pct_2 = 100 * `2dollar` / SADbase, pct_3 = 100 * `3dollar` / SADbase,
          incr_1 = pct_1, incr_2 = pct_2 - pct_1, incr_3 = pct_3 - pct_2, total = pct_3) |>
  pivot_longer(starts_with("incr"), names_to  = "step", values_to = "SADreduc_pct") |>
  mutate(step = factor(step, levels = c("incr_3","incr_2","incr_1"),labels = c("$3","$2","$1")))

pal <- c("$1" = "#1b9e77", "$2" = "#d95f02", "$3" = "#7570b3")

bar_abs <- ggplot(tax_abs, aes(x = reorder(abbr, total), y = SADsAverted, fill = step)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = pal, breaks =  c("$1","$2","$3")) +
  scale_y_continuous(labels = comma) +
  labs(x = "State", y = "Smoking-attributable deaths averted",
       fill = "Tax increase", title = "A. Absolute mortality reductions by state") + theme_light()

bar_rel <- ggplot(tax_rel, aes(x = reorder(abbr, total), y = SADreduc_pct,fill = step)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = pal, breaks = c("$1","$2","$3")) +
  scale_y_continuous(limits = c(0, 8), breaks = 0:8) +
  labs(x = "State", y = "% reduction in smoking−attributable deaths",
       fill = "Tax increase", title = "B. Relative mortality reductions by state") + theme_light()

fig_tax_bars <- ggarrange(bar_abs, bar_rel,nrow = 1,common.legend = TRUE, legend = "bottom")

ggsave(filename = "figs/within_policy_comparison/SADs_tax_allstates.jpg", plot = fig_tax_bars, width = 8, height = 6, 
       dpi = 600, device = "jpeg")

#-------- Smoke-free air laws:stacked mortality reduction bar plots all states ---------------- 

parse_air_name <- function(x) {
  tibble(fipscode = str_extract(x, "^[0-9]{2}"),
         air_scen = str_extract(x, "(wp1_r[01]_b[01])$"))}

air_mort_df <- imap_dfr(airlaws_out_list, function(el, nm) {
  meta <- parse_air_name(nm)
  el[["df_mort.outputs"]] |>
    filter(year == 2100, gender == "Both") |>
    mutate(fipscode = meta$fipscode, air_scen = meta$air_scen)}) |> 
  filter(air_scen %in% c("wp1_r0_b0", "wp1_r1_b0", "wp1_r1_b1"))

air_abs <- air_mort_df |> 
  group_by(abbr, air_scen) |> 
  summarise(SADsAvertedcum = sum(SADsAvertedcum), .groups = "drop") |> 
  pivot_wider(names_from = air_scen, values_from = SADsAvertedcum) |> 
  mutate(across(everything(), ~replace_na(.x, 0))) |> 
  mutate(incr_1 = `wp1_r0_b0`, incr_2 = `wp1_r1_b0` - `wp1_r0_b0`, incr_3 = `wp1_r1_b1` - `wp1_r1_b0`, total = `wp1_r1_b1`) |> 
  select(abbr, incr_1, incr_2, incr_3, total) |> 
  pivot_longer(cols = starts_with("incr"), names_to = "step", values_to = "SADsAverted") |> 
  mutate(step = factor(step, levels = c("incr_3", "incr_2", "incr_1"), labels = c("wp1_r1_b1", "wp1_r1_b0", "wp1_r0_b0")))

air_rel <- air_mort_df |> 
  group_by(abbr, air_scen) |> 
  summarise(SADsAvertedcum = sum(SADsAvertedcum), SADbase = sum(SADcum), .groups = "drop") |> 
  pivot_wider(names_from = air_scen, values_from = SADsAvertedcum) |> 
  mutate(across(c(`wp1_r0_b0`, `wp1_r1_b0`, `wp1_r1_b1`), ~replace_na(.x, 0))) |> 
  mutate(pct_1 = 100 * `wp1_r0_b0` / SADbase, pct_2 = 100 * `wp1_r1_b0` / SADbase, pct_3 = 100 * `wp1_r1_b1` / SADbase,
         incr_1 = pct_1, incr_2 = pct_2 - pct_1, incr_3 = pct_3 - pct_2, total  = pct_3) |> 
  pivot_longer(cols = starts_with("incr"), names_to = "step", values_to = "SADreduc_pct") |> 
  mutate(step = factor(step, levels = c("incr_3", "incr_2", "incr_1"), labels = c("wp1_r1_b1", "wp1_r1_b0", "wp1_r0_b0")))

air_order <- air_rel |>
  group_by(abbr) |>
  summarise(total_sum = sum(SADreduc_pct, na.rm = TRUE), .groups = "drop") |>
  arrange(total_sum,abbr) |>
  pull(abbr)   

air_rel <- air_rel |>
  mutate(abbr = factor(abbr, levels = air_order))

pal_air <- c("wp1_r0_b0" = "#1b9e77", "wp1_r1_b0" = "#d95f02", "wp1_r1_b1" = "#7570b3")

bar_abs_air <- ggplot(air_abs, aes(x = reorder(abbr, total), y = SADsAverted, fill = step)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = pal_air, breaks = c("wp1_r0_b0", "wp1_r1_b0", "wp1_r1_b1")) +
  scale_y_continuous(labels = comma) +
  labs(x = "State", y = "Smoking-attributable deaths averted",
       fill = "Air Law Scenario", title = "A. Absolute mortality reductions by state") + theme_light()

bar_rel_air <- ggplot(air_rel, aes(x = abbr, y = SADreduc_pct, fill = step)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = pal_air, breaks = c("wp1_r0_b0", "wp1_r1_b0", "wp1_r1_b1")) +
  scale_y_continuous(limits = c(0, 8), breaks = 0:8) +
  labs( x = "State", y = "% reduction in smoking-attributable deaths",
        fill = "Air Law Scenario", title = "B. Relative mortality reductions by state") + theme_light()

fig_air_bars <- ggarrange(bar_abs_air, bar_rel_air, nrow = 1, common.legend = TRUE, legend = "bottom")

ggsave(filename = "figs/within_policy_comparison/SADs_airlaws_allstates.jpg", plot = fig_air_bars, width = 8, height = 6, 
       dpi = 600, device = "jpeg")

#-------- Tobacco control expenditures:stacked mortality reduction bar plots all states --------------

parse_exp_name <- function(x) {
  tibble(fipscode = str_extract(x, "^[0-9]{2}"), 
         exp_scen = str_extract(x, "(?<=_exp)[0-9]+"))}

tcexp_mort_df <- imap_dfr(tcexp_out_list, function(el, nm) {
  meta <- parse_exp_name(nm)
  el[["df_mort.outputs"]] |>
    filter(year == 2100, gender == "Both") |>
    mutate(fipscode = meta$fipscode,
           exp_scen  = paste0(meta$exp_scen, "%"))}) |>
  filter(exp_scen %in% c("60%", "80%", "100%"))

exp_abs <- tcexp_mort_df |>
  group_by(abbr, exp_scen) |>
  summarise(SADsAvertedcum = sum(SADsAvertedcum), .groups = "drop") |>
  pivot_wider(names_from = exp_scen, values_from = SADsAvertedcum) |>
  mutate(across(c(`60%`, `80%`, `100%`), ~replace_na(.x, 0))) |>
  mutate(incr_1 = `60%`, incr_2 = `80%` - `60%`, incr_3 = `100%` - `80%`, total  = `100%`) |>
  select(abbr, incr_1, incr_2, incr_3, total) |>
  pivot_longer(cols = starts_with("incr"), names_to  = "step", values_to = "SADsAverted") |>
  mutate(step = factor(step, levels = c("incr_3", "incr_2", "incr_1"), labels = c("100%", "80%", "60%")))

exp_rel <- tcexp_mort_df |>
  group_by(abbr, exp_scen) |>
  summarise(SADsAvertedcum = sum(SADsAvertedcum),
            SADbase = sum(SADcum), .groups = "drop") |>
  pivot_wider(names_from = exp_scen, values_from = SADsAvertedcum) |>
  mutate(across(c(`60%`, `80%`, `100%`), ~replace_na(.x, 0))) |>
  mutate(pct_1  = 100 * `60%` / SADbase, pct_2  = 100 * `80%` / SADbase, pct_3  = 100 * `100%` / SADbase,
         incr_1 = pct_1, incr_2 = pct_2 - pct_1, incr_3 = pct_3 - pct_2, total  = pct_3) |>
  pivot_longer(cols = starts_with("incr"), names_to  = "step", values_to = "SADreduc_pct") |>
  mutate(step = factor(step, levels = c("incr_3", "incr_2", "incr_1"), labels = c("100%", "80%", "60%")))

pal_exp <- c("60%" = "#1b9e77", "80%" = "#d95f02", "100%" = "#7570b3")

bar_abs_exp <- ggplot(exp_abs, aes(x = reorder(abbr, total), y = SADsAverted, fill = step)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = pal_exp, breaks = c("60%", "80%", "100%")) +
  scale_y_continuous(labels = comma) +
  labs(x = "State", y = "Smoking-attributable deaths averted",
       fill = "Spending level", title = "A. Absolute mortality reductions by state") + theme_light()

bar_rel_exp <- ggplot(exp_rel, aes(x = reorder(abbr, total), y = SADreduc_pct, fill = step)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = pal_exp, breaks = c("60%", "80%", "100%")) +
  scale_y_continuous(limits = c(0, 8), breaks = 0:8) +
  labs(x = "State", y = "% reduction in smoking-attributable deaths",
       fill = "Spending level", title = "B. Relative mortality reductions by state") + theme_light()

fig_exp_bars <- ggarrange(bar_abs_exp, bar_rel_exp, nrow = 1, common.legend = TRUE, legend = "bottom")

ggsave(filename = "figs/within_policy_comparison/SADs_tcexp_allstates.jpg", plot = fig_exp_bars, width = 8, height = 6, 
       dpi = 600, device = "jpeg")



