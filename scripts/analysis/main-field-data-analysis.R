# Performs the main analyses of seedling density and its drivers, including GAM modeling and data vis

library(tidyverse)
library(mgcv)
library(here)
library(scales)
library(ggpubr)
library(lubridate)

source(here("scripts/analysis/main-field-data-analysis_functions.R"))

# The root of the data directory
datadir = readLines(here("data_dir.txt"), n=1)


# Load data
d = read_csv(file.path(datadir, "field-data", "processed",  "plot-data-prepped_v2.csv"))



#### Constants

## Define the date windows over which to summarize raw data for figure (by day of year)
windows = data.frame(fire = c("Caldor","Caldor", "Dixie", "Dixie", "Dixie"),
                     start = c(229, 239, 203, 225, 252),
                     end =   c(231, 242, 205, 230, 252))

## Minimum distance (m) from an interior plot to a green tree (sight line must also go that far)
minimum_dist_green = 60

# When plotting model fits, trim to range of data minus the following percentile of data points at both extremes
percentile_exclude = 0.025

# Fire intensity (torching intensity) threshold beyond which plots are classified as "Torched" as opposed to "Scorched"
intensity_threshold = 85

##### Analysis

#### Summarize/display environmental context for interpreting results

### For ALL SPECIES
# Prep data
d_sp = prep_d_sp("ALL")

# plot distance to seed wall vs climate for seed wall plots
d_sw = prep_d_sw_mod(d_sp, max_sw_dist = 60)

p = ggplot(d_sw, aes(x = ppt, y = dist_sw, color = day_of_burning, shape = fire)) +
  geom_point(size = 3) +
  theme_bw(15) +
  scale_shape(name = "Fire") +
  scale_color_viridis_c(na.value = NA, breaks = c(182, 196, 213, 227, 244, 258), labels = c("01-Jul", "15-Jul", "01-Aug","15-Aug", "01-Sep", "15-Sep"), name = "Day of burning") +
  labs(x = "Mean annual precipitation (mm)", y = "Distance to edge (m)")

png(file.path(datadir, "figures/supp_dist_ppt.png"), res = 200, width = 1500, height = 1100)
print(p)
dev.off()


# Plot seed wall plot seed source distance by day of burning and fire
p = ggplot(d_sw, aes(x = day_of_burning, y = dist_sw, color = fire, shape = fire)) +
  geom_point(size = 3) +
  theme_bw(15) +
  scale_shape(name = "Fire") +
  scale_color_viridis_d(name = "Fire", begin = 0.2, end = 0.8) +
  scale_x_continuous(breaks = c(182, 196, 213, 227, 244, 258), labels = c("01-Jul", "15-Jul", "01-Aug","15-Aug", "01-Sep", "15-Sep"), name = "Date of burning") +
  labs(y = "Distance to edge (m)")

png(file.path(datadir, "figures/supp_dist_dob.png"), res = 200, width = 1500, height = 1100)
print(p)
dev.off()  


# Plot raw data for all species. This function saves to figure files.
d_sp = prep_d_sp("ALL")
plot_raw_data(d_sp, axis_label = bquote(Conifer~seedlings~m^-2), plot_title = NULL, filename = "all")



##### Fit GAMs for predicting seedling density using fire intensity and climate

### Core
## For ALL species, core
d_sp = prep_d_sp("ALL")

# Need integer response var (number of seedl at 10 m radius plot scale), and assign tags for which run this is
d_mod_all_core = prep_d_core_mod(d_sp) |>
  mutate(seedl_dens_sp = round(seedl_dens_sp * 314)) |>
  mutate(species = "All conifers", type = "Interior")

m = gam(seedl_dens_sp ~ s(fire_intens, k = 3) + s(ppt, k = 3) , data = d_mod_all_core, family = nb, method = "REML") # full model
m_nointens = gam(seedl_dens_sp ~ s(ppt, k = 3), data = d_mod_all_core, family = nb, method = "REML") # model without fire intensity, for understanding how much that predictor matters
all_core_intens_dev_exp = (100 *(m$null.deviance - m$deviance) / m$null.deviance) |> round(1) # deviance explained for full model
all_core_nointens_dev_exp = (100 *(m_nointens$null.deviance - m_nointens$deviance) / m_nointens$null.deviance) |> round(1) # deviance explained for reduced model
sum(influence(m)) # model effective degrees of freedom
# Prep scenario plotting data
predictors = c("fire_intens", "ppt")
scenario_preds_all = get_scenario_preds(m, d_mod_all_core, predictors, sp = "All conifers", percentile_exclude = percentile_exclude) |> mutate(type = "Interior")



## For PINES, core
d_sp = prep_d_sp("PINES")
d_mod_pines_core = prep_d_core_mod(d_sp) |>
  mutate(seedl_dens_sp = round(seedl_dens_sp * 314)) |>
  mutate(species = "Pines", type = "Interior")
# Plot raw data
# Fit GAM
m = gam(seedl_dens_sp ~ s(fire_intens, k = 3) + s(ppt, k = 3) + s(prefire_prop_sp, k = 3), data = d_mod_pines_core, family = nb, method = "REML")
m_nointens = gam(seedl_dens_sp ~ s(ppt, k = 3) + s(prefire_prop_sp, k = 3), data = d_mod_pines_core, family = nb, method = "REML")
pines_core_intens_dev_exp = (100 *(m$null.deviance - m$deviance) / m$null.deviance) |> round(1)
pines_core_nointens_dev_exp = (100 *(m_nointens$null.deviance - m_nointens$deviance) / m_nointens$null.deviance) |> round(1)
sum(influence(m))
# Prep scenario plotting data
predictors = c("fire_intens", "ppt")
scenario_preds_pines = get_scenario_preds(m, d_mod_pines_core, predictors, sp = "Pines", percentile_exclude = percentile_exclude) |> mutate(type = "Interior")


## For ALL SPECIES, seedwall
d_sp = prep_d_sp("ALL")
# Select the "very near" seed wall plots
d_mod_all_sw = prep_d_sw_mod(d_sp, max_sw_dist = 30) |>
  mutate(seedl_dens_sp = round(seedl_dens_sp * 314)) |>
  mutate(species = "All conifers", type = "Edge")
m = gam(seedl_dens_sp ~ + s(ppt, k = 3) + s(fire_intens, k = 3), data = d_mod_all_sw, family = nb, method = "REML")
m_nointens = gam(seedl_dens_sp ~ + s(ppt, k = 3), data = d_mod_all_sw, family = nb, method = "REML")
all_sw_intens_dev_exp = (100 *(m$null.deviance - m$deviance) / m$null.deviance) |> round(1)
all_sw_nointens_dev_exp = (100 *(m_nointens$null.deviance - m_nointens$deviance) / m_nointens$null.deviance) |> round(1)
sum(influence(m))
# Prep scenario plotting data
predictors = c("fire_intens", "ppt")
scenario_preds_all_sw = get_scenario_preds(m, d_mod_all_sw, predictors, sp = "All conifers", percentile_exclude = percentile_exclude) |> mutate(type = "Edge")


## For PINES seedwall GAM fits 
d_sp = prep_d_sp("PINES")
d_mod_pines_sw = prep_d_sw_mod(d_sp, max_sw_dist = 30) |>
  mutate(seedl_dens_sp = round(seedl_dens_sp * 314)) |>
  mutate(species = "Pines", type = "Edge")
m = gam(seedl_dens_sp ~ s(ppt, k = 3) + s(fire_intens, k = 3) + s(grn_vol_sp, k = 3), data = d_mod_pines_sw, family = nb, method = "REML")
m_nointens = gam(seedl_dens_sp ~ s(ppt, k = 3) + s(grn_vol_sp, k = 3), data = d_mod_pines_sw, family = nb, method = "REML")
pines_sw_intens_dev_exp = (100 *(m$null.deviance - m$deviance) / m$null.deviance) |> round(1)
pines_sw_nointens_dev_exp = (100 *(m_nointens$null.deviance - m_nointens$deviance) / m_nointens$null.deviance) |> round(1)
sum(influence(m))
# Prep scenario plotting data
predictors = c("fire_intens", "ppt", "cone_dens_sp", "prefire_prop_sp", "grn_vol_sp")
scenario_preds_pines_sw = get_scenario_preds(m, d_mod_pines_sw, predictors, sp = "Pines", percentile_exclude = percentile_exclude) |> mutate(type = "Edge")


# Combine the scenario plots for each species group together so we can plot the fits of multiple species in one panel
scenario_preds = bind_rows(scenario_preds_all, scenario_preds_pines, scenario_preds_all_sw, scenario_preds_pines_sw)
d_mods = bind_rows(d_mod_all_core,
                  d_mod_pines_core,
                  d_mod_all_sw,
                  d_mod_pines_sw)


## Make scenario plots

# Define y-axis range
ymin = 0.05
ymax = 25

p1 = make_scenario_ggplot(scenario_preds, d_mods, "fire_intens", "Canopy burn fraction (%)", ymin = ymin, ymax = ymax)
p2 = make_scenario_ggplot(scenario_preds, d_mods, "ppt", "Mean annual precipitation (mm)", ymin = ymin, ymax = ymax)

p = ggarrange(p1, p2 + rremove("ylab") + rremove("y.text"), common.legend = TRUE, widths = c(1.2,1))

png(file.path(datadir, "figures/main_model_fits.png"), res = 350, width = 2000, height = 1100)
print(p)
dev.off()

# Save figure with torching panel only

p1a = p1 +
  theme(legend.box = "horizontal",
        legend.position = c(0.32,0.175),
        legend.box.background = element_rect(fill="white", color = "black", linewidth = 0.6),
        legend.spacing.x = unit(0.0, "cm")
  )
p1a

png(file.path(datadir, "figures/main_model_fits_torching-only.png"), res = 700, width = 2800, height = 2400)
print(p1a)
dev.off()

# Save figure with ppt panel only

p2a = p2
p2a

png()
dev.off()
png(file.path(datadir, "figures/main_model_fits_ppt-only.png"), res = 700, width = 3600, height = 2400)
print(p2a)
dev.off()


### Repeat for a figure with 'all species' only
p1 = make_scenario_ggplot_allsponly(scenario_preds, d_mods, "fire_intens", "Canopy burn fraction (%)", ymin = ymin, ymax = ymax)
p1

png(file.path(datadir, "figures/main_model_fits_torching-only_allsp-only.png"), res = 700, width = 2800*1.2, height = 2400)
print(p1)
dev.off()



#### Make a table of deviance explained 

dev = data.frame(species = c("All conifers - core", "Pines - core", "All conifers - SW", "Pines - SW"),
                 dev_expl_intens = c(all_core_intens_dev_exp, pines_core_intens_dev_exp, all_sw_intens_dev_exp, pines_sw_intens_dev_exp),
                 dev_expl_nointens = c(all_core_nointens_dev_exp, pines_core_nointens_dev_exp, all_sw_nointens_dev_exp, pines_sw_nointens_dev_exp)) |>
  mutate(difference = dev_expl_intens - dev_expl_nointens)
dev


#### Look up the model-fitted seedling densities at specific points along the continuum of fire intensity

inspect = scenario_preds |>
  filter(species %in% c("All conifers", "Pines"),
         between(fire_intens, 24.5, 25.5) | between(fire_intens, 94.5, 95.5),
         predictor_foc == "fire_intens",
         #between(ppt, 1634, 1636),
         type == "Interior") |>
  select(fire_intens, preds, ppt, species, type)
inspect


#### Make a density~torching (color: ppt) figure with the data points, and fit lines for two ppt scenarios
d_sp = prep_d_sp("ALL")
d_mod_all_core = prep_d_core_mod(d_sp) |>
  mutate(seedl_dens_sp = round(seedl_dens_sp * 314),
         cone_dens_sp = cone_dens_sp * 314) |>
  mutate(species = "All conifers", type = "Interior")

m = gam(seedl_dens_sp ~ s(fire_intens, k = 3) + s(ppt, k = 3) , data = d_mod_all_core, family = nb, method = "REML")
ppt_split = 1200 # split between low and high precip
scenario_preds = get_scenario_preds(m, d_mod_all_core, "fire_intens", sp = "All conifers", percentile_exclude = percentile_exclude, interacting_predictor = "ppt", interacting_splits = ppt_split) |> mutate(type = "Interior")
p1 = make_scenario_w_ppt_ggplot(scenario_preds, d_mod_all_core, "fire_intens", "Canopy burn fraction (%)", ymin = NULL, ymax = NULL, interacting_splits = ppt_split, show_data = TRUE)

png(file.path(datadir, "figures/fits_w_data.png"), res = 700, width = 2800, height = 2400)
print(p1)
dev.off()



## Get the climate summary of the plots
plots_for_clim = bind_rows(d_mod_all_core, d_mod_all_sw) |>
  select(plot_id, fire, ppt_wy2022_4km, ppt_norm_4km, tmean_wy2022_4km, tmean_norm_4km)

clim_summ = plots_for_clim |>
  summarize(ppt_wy2022_4km = mean(ppt_wy2022_4km, na.rm = TRUE),
            ppt_norm_4km = mean(ppt_norm_4km, na.rm = TRUE),
            tmean_wy2022_4km = mean(tmean_wy2022_4km, na.rm = TRUE),
            tmean_norm_4km = mean(tmean_norm_4km, na.rm = TRUE))





### Plot relationship between cone density and seedling density by species, and by scorched vs torched

d_ylwpines = prep_d_sp("YLWPINES") |> prep_d_core_mod() |> mutate(species = "Yellow pine")
d_sps = bind_rows(d_ylwpines)
# We can use d_sps for modeling. Next we will modify it to cause some data loss by turning 0s to nonzero so we can plot on log scale (thus this can't be used for poisson modeling due to manipulating the zeros)

# Get median cone density by sp
medians = d_sps |>
  group_by(species) |>
  summarize(median_dens = median(cone_dens_sp))
medians
# Bind this to the data so we can compute whether each obs was above or below the median
d_sps = left_join(d_sps,medians)

# Prep a data frame for plotting where we make zero values nonzero so they fit on the log-scale axis, and adding labels for the cone density classes
d_fig = d_sps |>
  # Put counts back on per sq m scale
  mutate(seedl_dens_sp = ifelse(seedl_dens_sp < 0.5/314, 0.0005, seedl_dens_sp)) |> # zeros get set to 0.0005 (less than all nonzero densities), which is later relabeled as 0
  mutate(cone_dens_sp = ifelse(cone_dens_sp < 0.5/314, 0.0005, cone_dens_sp)) |>
  mutate(cone_dens_sp_cat = ifelse(cone_dens_sp >= median_dens, "High", "Low")) |>
  mutate(cone_dens_sp_cat = factor(cone_dens_sp_cat, levels = c("Low","High"))) |>
  mutate(under_cones_new_sp = ifelse(under_cones_new_sp == "low", "Low", "High")) |>
  mutate(under_cones_new_sp = factor(under_cones_new_sp, levels = c("Low","High")))

# To report in paper, what is median seedling density for low vs high cone density
d_fig |>
  group_by(cone_dens_sp_cat) |>
  summarize(seedl_dens = median(seedl_dens_sp))
medians # The split between low and high cone density

# To report in paper, what is median seedling density for low vs high under-tree cone density
d_fig |>
  group_by(under_cones_new_sp) |>
  summarize(seedl_dens = median(seedl_dens_sp))


d_summ_cone_dens = d_fig |>
  group_by(cone_dens_sp_cat) |>
  summarize(median = median(seedl_dens_sp),
            lwr = quantile(seedl_dens_sp, 0.25),
            upr = quantile(seedl_dens_sp, 0.75))
  
d_summ_under_cones_new = d_fig |>
  group_by(under_cones_new_sp) |>
  summarize(median = median(seedl_dens_sp),
            lwr = quantile(seedl_dens_sp, 0.25),
            upr = quantile(seedl_dens_sp, 0.75))


# Set up plotting parameters
color_breaks = c(0.0005, 0.001, 0.01, 0.1, 1, 10, 100)
color_labels = c("[0]", "0.001", "0.01", "0.1", "1", "10", "100")

p1 = ggplot(d_fig, aes(x = cone_dens_sp_cat, y = seedl_dens_sp, color = cone_dens_sp)) +
  geom_linerange(data = d_summ_cone_dens, aes(ymin = lwr, ymax = upr, y = NULL), size = 1.2, position = position_nudge(x = +0.15), color = "black") +
  geom_jitter(alpha = 1, size = 1.5, aes(x = as.numeric(cone_dens_sp_cat) - 0.15), position = position_jitter(width = 0.1, height = 0)) +
  geom_point(data = d_summ_cone_dens, size = 4, aes(x = cone_dens_sp_cat, y = median), position = position_nudge(x = +0.15), shape = 18, color = "black") +
  scale_color_viridis_c(trans = "log", name = bquote(Cones~m^-2), breaks = color_breaks, labels = color_labels, oob = squish) +
  coord_trans(y = "log") +
  scale_y_continuous(breaks = c(0.0005, .001,.01,.1,1,10,100, 1000), minor_breaks = c(0.005, 0.05, 0.5, 5.0, 50, 500), labels = c("[0]", "0.001", "0.01", "0.1", "1", "10", "100", "1000")) +
  labs(x = "Plot cone\ndensity", y = bquote(Yellow~pine~seedlings~m^-2)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())
p1

p2 = ggplot(d_fig, aes(x = under_cones_new_sp, y = seedl_dens_sp)) +
  geom_linerange(data = d_summ_under_cones_new, aes(ymin = lwr, ymax = upr, y = NULL), size = 1.2, position = position_nudge(x = +0.15), color = "black") +
  geom_jitter(alpha = 1, size = 1.5, aes(x = as.numeric(under_cones_new_sp) - 0.15), position = position_jitter(width = 0.1, height = 0), , color = "gray60") +
  geom_point(data = d_summ_under_cones_new, size = 4, aes(x = under_cones_new_sp, y = median), position = position_nudge(x = +0.15), shape = 18, color = "black") +
  coord_trans(y = "log") +
  scale_y_continuous(breaks = c(0.0005, .001,.01,.1,1,10,100, 1000), minor_breaks = c(0.005, 0.05, 0.5, 5.0, 50, 500), labels = c("[0]", "0.001", "0.01", "0.1", "1", "10", "100", "1000")) +
  labs(x = "Single-tree cone\ndensity", y = bquote(Yellow~pine~seedlings~m^-2)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

p = ggarrange(p2, p1 + rremove("ylab") + rremove("y.text"), widths = c(1,1.25))

png(file.path(datadir, "figures/cone_dens_boxplots.png"), res = 800, width = 3600, height = 2800)
print(p)
dev.off()
