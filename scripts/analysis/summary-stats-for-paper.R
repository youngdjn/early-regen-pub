#### Summary statistics to report in paper

library(tidyverse)
library(here)

## Constants

# Minimum distance (m) from an interior plot to a green tree (sight line must also go that far)
minimum_dist_green = 60


# The root of the data directory
datadir = readLines(here("data_dir.txt"), n=1)

source("scripts/analysis/main-field-data-analysis_functions.R")

# Load data
d = read_csv(file.path(datadir,"field-data/processed/plot-data-prepped_v2.csv"))

# Prep the plots used for the "all species" analyses
d_sp = prep_d_sp("ALL")

### In interior plots: overall seedling densities, and count of scorched/torched plots
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > minimum_dist_green) & sight_line > minimum_dist_green),
         plot_type %in% c("core", "delayed")) |>
  mutate(intens_cat = ifelse(fire_intens > 85, "torched", "scorched")) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            n_torched = sum(intens_cat == "torched"),
            n_scorched = sum(intens_cat == "scorched"))


## Specifically early-burn plots
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > minimum_dist_green) & sight_line > minimum_dist_green),
         plot_type %in% c("core", "delayed"),
         day_of_burning <= 210) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            n = n())

## Specifically later-burned interior plots
d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > minimum_dist_green) & sight_line > minimum_dist_green),
         plot_type %in% c("core", "delayed"),
         day_of_burning > 210) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            n = n())


## Seed wall plots burning BEfore 1 Aug
d_sp |>
  filter(plot_type == "seedwall") |>
  filter(dist_sw <= 60,
         day_of_burning < 210) |>
  summarize(dens_mean = mean(seedl_dens_sp),
            dens_median = median(seedl_dens_sp),
            nplots = n(),
            med_dist_grn = median(dist_grn_sp))




### Species proportions in later-burned low vs high torch plots
# for low intensity plots
d_spcomp =  d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > minimum_dist_green) & sight_line > minimum_dist_green),
         day_of_burning > 210,
         between(fire_intens, 0, 50),
         plot_type %in% c("core", "delayed")) |>
  summarize(across(c(seedl_dens_ABIES,seedl_dens_CADE,seedl_dens_PILA,seedl_dens_PSME,seedl_dens_PICO,seedl_dens_YLWPINES), median),
            nplots = n())
tot = d_spcomp |>
  select(starts_with("seedl_dens_")) |>
  mutate(tot = rowSums(across(everything()))) |>
  pull(tot)

d_spcomp_lowintens = d_spcomp |>
  mutate(across(starts_with("seedl_dens_"), ~. / tot)) |>
  mutate(across(starts_with("seedl_dens_"), ~round(.*100, 1))) |>
  mutate(intensity = "low")
d_spcomp_lowintens

# for high intensity plots
d_spcomp =  d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > minimum_dist_green) & sight_line > minimum_dist_green),
         day_of_burning > 210,
         between(fire_intens, 80, 100),
         plot_type %in% c("core", "delayed")) |>
  summarize(across(c(seedl_dens_ABIES,seedl_dens_CADE,seedl_dens_PILA,seedl_dens_PSME,seedl_dens_PICO,seedl_dens_YLWPINES), median),
            nplots = n())
tot = d_spcomp |>
  select(starts_with("seedl_dens_")) |>
  mutate(tot = rowSums(across(everything()))) |>
  pull(tot)

d_spcomp_highintens = d_spcomp |>
  mutate(across(starts_with("seedl_dens_"), ~. / tot)) |>
  mutate(across(starts_with("seedl_dens_"), ~round(.*100, 1))) |>
  mutate(intensity = "high")
d_spcomp_highintens

d_spcomp_intens = bind_rows(d_spcomp_lowintens, d_spcomp_highintens)
d_spcomp_intens







### proportions by species of seedlings (any canopy burn fraction)
# exclude the early burn plots, compute by median

# for core area plots
d_spcomp =  d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > minimum_dist_green) & sight_line > minimum_dist_green),
         day_of_burning > 210,
         plot_type %in% c("core", "delayed")) |>
  summarize(across(c(seedl_dens_ABIES,seedl_dens_CADE,seedl_dens_PILA,seedl_dens_PSME,seedl_dens_PICO,seedl_dens_YLWPINES), median))
d_spcomp$tot = rowSums(d_spcomp)

d_spcomp_core = d_spcomp |>
  mutate(across(everything(), ~. / tot)) |>
  mutate(across(everything(), ~round(.*100, 1))) |>
  mutate(type = "core")
d_spcomp_core


# and seed wall plots
d_spcomp =  d_sp |>
  filter(plot_type == "seedwall") |>
  filter(dist_sw <= 60) |>
  summarize(across(c(seedl_dens_ABIES,seedl_dens_CADE,seedl_dens_PILA,seedl_dens_PSME,seedl_dens_PICO,seedl_dens_YLWPINES), median))

d_spcomp$tot = rowSums(d_spcomp)

d_spcomp_sw = d_spcomp |>
  mutate(across(everything(), ~. / tot)) |>
  mutate(across(everything(), ~round(.*100, 1))) |>
  mutate(type = "seedwall")
d_spcomp_sw

# combine and write
spcomp = bind_rows(d_spcomp_core, d_spcomp_sw)
spcomp
write_csv(spcomp, file.path(datadir, "tables/regen_species_comp.csv"))



### Get mean pre-fire overstory species comp across all core plots burning in Aug or later
d_ovrspcomp = d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > minimum_dist_green) & sight_line > minimum_dist_green),
         day_of_burning > 210,
         plot_type %in% c("core", "delayed")) |>
  summarize(across(starts_with("prefire_prop_"), mean)) |>
  arrange(decreasing = TRUE)
d_ovrspcomp



### Get max seedling density by species
d_max_seedl_dens =  d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > minimum_dist_green) & sight_line > minimum_dist_green),
         plot_type %in% c("core", "delayed")) |>
  summarize(across(starts_with("seedl_dens_"), max))
d_max_seedl_dens




### Interior plot vs edge plot seedling density for early burned, late burned, mid-burned (for Table S1)

# get the interior, low canopy burn plots to consider
d_int = d_sp |>
  filter(grn_vol_abs_sp == 0,
         ((is.na(dist_grn_sp) | dist_grn_sp > minimum_dist_green) & sight_line > minimum_dist_green),
         plot_type %in% c("core", "delayed"),
         fire_intens < 85) |>
  select(seedl_dens_sp, day_of_burning) |>
  mutate(plot_type_simp = "interior")
  
# get the edge plots to consider
d_edge = d_sp |>
  filter(plot_type == "seedwall") |>
  filter(dist_sw <= 60) |>
  select(seedl_dens_sp, day_of_burning, dist_grn_sp) |>
  mutate(plot_type_simp = "edge")

d_pre = bind_rows(d_int, d_edge)

# First early burned
d_relative_dens_early = d_pre |>
  filter(day_of_burning < 210) |>
  group_by(plot_type_simp) |>
  summarize(median_dens_early = median(seedl_dens_sp),
            n_early = n(),
            median_dist_grn_early = median(dist_grn_sp))

# Mid burned
d_relative_dens_mid = d_pre |>
  filter(between(day_of_burning, 219, 219+11)) |>
  group_by(plot_type_simp) |>
  summarize(median_dens_mid = median(seedl_dens_sp),
            n_mid = n(),
            median_dist_grn_mid = median(dist_grn_sp))
 
# Late burned
d_relative_dens_late = d_pre |>
  filter(day_of_burning > 210) |>
  group_by(plot_type_simp) |>
  summarize(median_dens_late = median(seedl_dens_sp),
            n_late = n(),
            median_dist_grn_late = median(dist_grn_sp))

d_rel_dens = d_relative_dens_early |>
  left_join(d_relative_dens_late) |>
  left_join(d_relative_dens_mid)

d_rel_dens_rel = (d_rel_dens |> filter(plot_type_simp == "interior") |> select(-plot_type_simp, -n_early, -n_mid, -n_late)) /
  (d_rel_dens |> filter(plot_type_simp == "edge") |> select(-plot_type_simp, -n_early, -n_mid, -n_late)) *
  100
d_rel_dens_rel

d_rel_dens |> select(plot_type_simp, n_early, n_late, n_mid, median_dist_grn_early, median_dist_grn_late, median_dist_grn_mid)



## Make observed data summary table for supp

# First a function to compute the median and IQR and format it nicely
median_iqr = function(x, mult = 10000) {
  x = x*mult
  median = median(x)
  lwr = quantile(x, 0.25)
  upr = quantile(x, 0.75)
  paste0(round(median, 0), " (", round(lwr, 0), ", ", round(upr, 0), ")")
}

d_sp

summ_pre = d_sp |>
  filter(((grn_vol_abs_sp == 0) & (((is.na(dist_grn_sp) | dist_grn_sp > minimum_dist_green) & sight_line > minimum_dist_green)) & (plot_type %in% c("core", "delayed"))) |
          (plot_type == "seedwall" & dist_sw <= 60)) |>  
  mutate(burn_date_cat = ifelse(day_of_burning < 210, "Early", "Late")) |>
  mutate(burn_date_cat = factor(burn_date_cat, levels = c("Early", "Late"))) |>
  mutate(intens_cat = ifelse(fire_intens > 85, "High", "Low")) |>
  mutate(intens_cat = factor(intens_cat, levels = c("Low", "High"))) |>
  mutate(plot_type_cat = ifelse(plot_type == "seedwall", "Edge", "Interior")) |>
  mutate(plot_type_cat = factor(plot_type_cat, levels = c("Interior", "Edge")))
  
# Interlude: get unique burn days
table(d_sp$day_of_burning)


# Burn date is irrelevant for edge plots
summ_pre[summ_pre$plot_type_cat == "Edge", "burn_date_cat"] = NA

summ_perdate = summ_pre |> 
  group_by(plot_type_cat, intens_cat, burn_date_cat) |>
  mutate(across(starts_with("under_cones_new_"), ~ifelse(.=="low", 0, 1))) |>
  summarize(plot_count = n(),
            seedl_dens_pipj = median_iqr(seedl_dens_YLWPINES),
            #seedl_dens_pila = median_iqr(seedl_dens_PILA), #zero
            #seedl_dens_psme = median_iqr(seedl_dens_PSME), #zero
            seedl_dens_abies = median_iqr(seedl_dens_ABIES),
            seedl_dens_cade = median_iqr(seedl_dens_CADE),
            seedl_dens_all = median_iqr(seedl_dens_sp),
            cone_dens_pipj = median_iqr(cone_dens_YLWPINES),
            #cone_dens_pila = median_iqr(cone_dens_PILA), #zero
            cone_dens_psme = median_iqr(cone_dens_PSME),
            under_cones_new_PIPJ = (mean(under_cones_new_PIPJ) * 100) |> round(0),
            under_cones_new_PSME = (mean(under_cones_new_PILA) * 100) |> round(0))

# summ_overall = summ_pre |>
#   group_by(plot_type_cat, intens_cat) |>
#   summarize(plot_count = n(),
#             seedl_dens_pipj = median(seedl_dens_YLWPINES),
#             #seedl_dens_pila = median(seedl_dens_PILA), #zero
#             #seedl_dens_psme = median(seedl_dens_PSME), #zero
#             seedl_dens_abies = median(seedl_dens_ABIES),
#             seedl_dens_cade = median(seedl_dens_CADE),
#             seedl_dens_all = median(seedl_dens_sp),
#             cone_dens_pipj = median(cone_dens_YLWPINES),
#             #cone_dens_pila = median(cone_dens_PILA), #zero
#             cone_dens_psme = median(cone_dens_PSME)) |>
#   # Exclude edge plots because we don't need an overall summary for them (not grouping by burn date)
#   filter(plot_type_cat == "Interior") |>
#   mutate(burn_date_cat = "Overall")

# Optionally add overall (across burn dates) summary: If do, need to update "median" to "median_iqr"
#summ = bind_rows(summ_perdate, summ_overall)

# Or alternatively do not include the overall summary
summ = summ_perdate

summ = summ |>
  mutate(burn_date_cat = factor(burn_date_cat, levels = c("Early", "Late", "Overall"))) |>
  arrange(plot_type_cat, intens_cat, burn_date_cat)

summ

# Write CSV
write_csv(summ, file.path(datadir, "tables/observed_densities.csv"))
