# Make Fig. S1 from the data published by Davis et al. (2023)

library(tidyverse)
library(here)

# The root of the data directory
datadir = readLines(here("data_dir.txt"), n=1)

d = read_csv(file.path(datadir, "Davis_et_al_regen_plot_data.csv")) # This data file is publicly available as supporting data to the article here: https://www.pnas.org/doi/10.1073/pnas.2208120120

## exclude plots that are only PICO, and sum all species but PICO
d = d |>
  filter(pico_mod == 0 & ((abco_abgr_mod + abla_mod + pien_mod + pipo_pije_mod + psme_mod) > 0)) |>
  mutate(dens_all = dens_ABLA + dens_ABCO_ABGR + dens_PIEN + dens_PIPO_PIJE + dens_PSME) |>
  mutate(dens_all = dens_all / 10000)

## Set plots with 0 seedlings to a number that is nonzero but less than all observed seedling densities, so they can be plotted on log axis (will be labeled as 0)
min_nonzero = .0001

# include a random jitter
d = d |>
  mutate(dens_all_nozero = ifelse(dens_all == 0, min_nonzero + runif(n = nrow(d), min = 0, max = .0004), dens_all))

# Remove observations that are >= 400 m because those can't be measured by laser, must be shorthand for "not observed"
# Filter to recent < 10 y, high-severity, and classify plots by large vs small
d = d |>
  filter(distance_seed_source < 400) |>
  filter(time_since_fire < 10) |>
  filter(fire_severity_rbr > 300) |>
  mutate(plot_size_cat = ifelse(plot_size > 100, "> 100 sq m", "All")) |>
  mutate(plot_size_cat = factor(plot_size_cat, levels = c("All", "> 100 sq m"))) |>
  arrange(plot_size_cat) |>
  mutate(plot_size_cat = factor(plot_size_cat, levels = c("> 100 sq m", "All")))

p = ggplot(d, aes(x = distance_seed_source, y = dens_all_nozero, color = plot_size_cat)) +
  geom_point(pch = 1) +
  scale_y_continuous(breaks = c(.0001, .0005, 0.001, 0.01,.1,1,10,100, 1000), labels = c("[0]", "[0]","0.001","0.01","0.1","1","10","100","1000" ), minor_breaks = c(0.005, 0.05, 0.5, 5.0, 50, 500)) +
  coord_trans(y = "log") +
  theme_bw(14) +
  labs(x = "Distance to live tree (m)",
       y = "Conifer seedlings / sq m") +
  geom_hline(yintercept = 0.0007, linewidth = 1, linetype = "dashed", color = "cornflowerblue") +
  scale_color_viridis_d(begin=.2, end = .8, name = "Plot size")
p

png(file.path(datadir, "figures/kdavis-seedl-dens.png"), res = 300, width = 1.3*1600, height = 1.5*1000)
p
dev.off()

# calc the median density in a few distance bins

d_summ_all = d |>
  mutate(dist_bins = cut(distance_seed_source, breaks = c(-1,50, 100, 150, 200, 500), labels = c(50, 100, 150, 200, 500))) |>
  group_by(dist_bins) |>
  summarize(median_dens = median(dens_all),
            mean_dens = mean(dens_all))

d_summ_large = d |>
  filter(plot_size > 100) |>
  mutate(dist_bins = cut(distance_seed_source, breaks = c(-1,50, 100, 150, 200, 500), labels = c(50, 100, 150, 200, 500))) |>
  group_by(dist_bins) |>
  summarize(median_dens = median(dens_all),
            mean_dens = mean(dens_all))


