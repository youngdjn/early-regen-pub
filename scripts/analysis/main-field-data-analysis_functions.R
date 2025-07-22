# Functions that support analysis in the main analysis workflow script

## Take the full raw data frame with one row per plot and prepare it for analysis focused on a specific species or species group.
# Primarily, for the species-specific questions (or species group-specific questions), prepare a series of columns ending in _sp that have the data summarized specifically for that species.
# Also removes seed wall plots with < 20% species comp in the seed wall of the focal species, removes core area plots with < 20% pre-fire overstory species comp of the focal species, removes seed wall plots with > 20% green canopy remaining within 10m
prep_d_sp = function(sp) {
  
  dist_grn_var = paste0("dist_grn_", sp)
  seedl_dens_var = paste0("seedl_dens_",sp)
  untorched_vol_abs_var = paste0(sp, "_untorched_vol_abs")
  grn_vol_var = paste0(sp, "_green_vol")
  grn_vol_abs_var = paste0(sp, "_green_vol_abs")
  under_cones_new_var = paste0("under_cones_new_", sp)
  cone_dens_var = paste0("cone_dens_", sp)
  prefire_prop_var = paste0("prefire_prop_", sp)
  
  d_sp = d |>
    select(fire, plot_id, shrub_cover, shrub_ht, capable_growing_area, litter_cover,
           litter_depth, moss_cover, branches_cover, plot_type,
           vol_grn_50m, vol_brn_50m, vol_blk_50m, vol_grn_10m, sight_line, mean_tree_dbh, seedwall_density_cat,
           mean_seedwall_height, day_of_burning, ppt, tmean, ppt_wy2022_4km, tmean_wy2022_4km, ppt_norm_4km, tmean_norm_4km,
           fire_intens, PINES_green_vol,
           dist_sw = dist_grn_ALL, # seed wall seed sources are only ever recorded under ALL. This var is not meaningful for core area plots.
           dist_grn_sp = starts_with(dist_grn_var),
           seedl_dens_sp = starts_with(seedl_dens_var),
           untorched_vol_abs_sp = starts_with(untorched_vol_abs_var),
           grn_vol_sp = matches(paste0("^",grn_vol_var,"$")),
           grn_vol_abs_sp = starts_with(grn_vol_abs_var),
           under_cones_new_sp = starts_with(under_cones_new_var),
           cone_dens_sp = starts_with(cone_dens_var),
           prefire_prop_sp = matches(paste0("^", prefire_prop_var, "$"), ignore.case = FALSE),
           starts_with("seedl_dens_"),
           starts_with("cone_dens_"),
           starts_with("prefire_prop_")) |>
    filter(ifelse(plot_type == "seedwall", grn_vol_sp > 20, prefire_prop_sp > 20)) |> # the criteria for keeping the plot based on its species comp depend on whether it's a seed wall or core area plot (for seed wall use seed wall comp)
    filter(ifelse(plot_type == "seedwall", vol_grn_10m < 20, TRUE)) # seed wall plots need to have < 20% pre-fire green canopy within 10 m

  # For species that have cones, compute log cone density, setting log(0) equal to the density of half a cone in the (8 m radius) plot
  if(! sp %in% c("ABIES", "ABCO", "CADE")) {
    d_sp = d_sp |>
      mutate(cone_dens_sp_log = log(ifelse(cone_dens_sp == 0, 0.5/(3.14*8^2), cone_dens_sp)))
  }
  
  return(d_sp)
}


#### Plot raw seedling density observations, primarily:
# seedling density data vs day of burning scatterplot, with points colored by plot type, and
# an alternative version that classifies plots as early/late edge/interior_low/interior_high, as
# opposed to plotting along a timeline of day of burning

plot_raw_data = function(d_sp, axis_label, plot_title, filename) {
  
  # Make zeros nonzero so they can be displayed on the log scale axis
  d_sp = d_sp |>
    mutate(seedl_dens_sp = ifelse(seedl_dens_sp < 0.00001, 0.0005, seedl_dens_sp))
  
  ## Turn day of burning to a date
  d_sp = d_sp |>
    mutate(date_of_burning = ymd("2021-01-01") + day_of_burning-1) # For fires the burned in other years, will need more flexible here, at least for leap years
  
  # Get the core area plots, far from any green of the focal species
  d_sp_nogrn = d_sp |>
    filter(grn_vol_abs_sp == 0,
           ((is.na(dist_grn_sp) | dist_grn_sp > minimum_dist_green) & sight_line > minimum_dist_green),
           plot_type %in% c("core", "delayed"))
  
  # Get seed wall plots
  d_sp_sw = d_sp |>
    filter(plot_type == "seedwall") |>
    filter(dist_sw <= 60) |>
    # classify into near and far seed wall plots
    mutate(dist_sw_cat = ifelse(dist_sw < 30, "Very near", "Near")) |>
    mutate(dist_sw_cat = as.factor(dist_sw_cat)) |>
    mutate(dist_sw_cat = factor(dist_sw_cat, levels = c("Very near", "Near")))
  
  # Classify fire intens
  d_sp_nogrn_fig = d_sp_nogrn |>
    mutate(fire_intens_cat = ifelse(fire_intens < intensity_threshold, "Interior, low canopy\nburn fraction", "Interior, high canopy\nburn fraction"))
  
  
  ### Before making the main figure, use the same data to make a context figure: plot day of burning, precip, and plot type
  allplots = bind_rows(d_sp_nogrn_fig, d_sp_sw) |>
    mutate(plot_type = recode(plot_type, "delayed" = "core")) # this may select some delayed mortality plots that behave as core plots because they're > 100 m from green of the focal species.
  
  # Mutate some values as we want them to be displaye in figure
  d_fig = allplots |>
    mutate(plot_type = recode(plot_type, "core" = "Interior", "seedwall" = "Edge"))
  
  # Make a col that combines plot type and for edge, whether near or very near to edge
  d_fig$plot_type_w_prox = d_fig$plot_type
  d_fig[d_fig$plot_type == "Edge" & d_fig$dist_sw_cat == "Near",]$plot_type_w_prox = "Edge near"
  d_fig[d_fig$plot_type == "Edge" & d_fig$dist_sw_cat == "Very near",]$plot_type_w_prox = "Edge very near"
  
  d_fig = d_fig |>
    mutate(plot_type_w_prox = factor(plot_type_w_prox, levels = c("Edge very near", "Edge near", "Interior")))
  
  p = ggplot(d_fig, aes(x = date_of_burning, y = ppt, color = fire, shape = plot_type_w_prox)) +
    geom_jitter(width = 3, size = 3) +
    theme_bw(15) +
    scale_shape_manual(name = "Plot type", values = c(19, 1, 17)) +
    scale_color_viridis_d(name = "Fire", begin = 0.2, end = 0.8) +
    labs(x = "Date of burning", y = "Mean annual precipitation (mm)")
  
  png(file.path(datadir, paste0("figures/supp_ppt_dob_", filename, ".png")), res = 200, width = 1500, height = 1100)
  print(p)
  dev.off()  
  
  
  # Make a copy of the constant df (defined at top of main analysis script) defining the date windows, which we will use to store the median seedling density vals for this species within the different date windows
  windows_foc = windows |>
  # convert the windows day of year to date
    mutate(across(c(start,end), ~ (ymd("2021-01-01") + . - 1)))
    
  # compute median seedl density by seedwall, scorch, torch within each predefined date window
  for(i in 1:nrow(windows_foc)) {
    
    window = windows_foc[i,]
    
    core_blk_foc = d_sp_nogrn_fig |>
      filter(fire == window$fire) |>
      filter(between(date_of_burning, window$start, window$end)) |>
      filter(fire_intens_cat == "Interior, high canopy\nburn fraction",)
    core_blk_median = median(core_blk_foc$seedl_dens_sp)
    
    core_brn_foc = d_sp_nogrn_fig |>
      filter(fire == window$fire) |>
      filter(between(date_of_burning, window$start, window$end)) |>
      filter(fire_intens_cat == "Interior, low canopy\nburn fraction")
    core_brn_median = median(core_brn_foc$seedl_dens_sp)
    
    sw_foc = d_sp_sw |>
      filter(fire == window$fire) |>
      filter(between(date_of_burning, window$start, window$end))
    sw_median = median(sw_foc$seedl_dens_sp)
    
    windows_foc[i,"seedwall_median"] = sw_median
    windows_foc[i,"core_blk_median"] = core_blk_median
    windows_foc[i,"core_brn_median"] = core_brn_median
    
  }
  
  d_fig_tmp = bind_rows(d_sp_nogrn_fig, d_sp_sw) |>
    mutate(fire_intens_cat = ifelse(plot_type == "seedwall", "Edge", fire_intens_cat))

  # Make fig
  p = ggplot(d_fig_tmp, aes(x = date_of_burning, y = seedl_dens_sp)) +
    geom_hline(yintercept = 0.0173, linetype = "dashed", color="gray70") +
    #geom_hline(yintercept = 0.0005, color = "orange") +
    geom_jitter(size=2.5, height = 0, width=2, aes(color = fire_intens_cat)) +
    #geom_jitter(data = d_sp_sw, color="#A2D435", size=2.5, height=0, width=2, show.legend = TRUE, aes(color = "test")) + # , aes(shape=dist_sw_cat)


    labs(shape = "Edge") +
    scale_color_manual(values = c(`Interior, high canopy\nburn fraction` = "black", `Interior, low canopy\nburn fraction` = "#b06d1b", `Edge` = "#A2D435"), name = "Plot type") +
    facet_grid(~fire) +
    theme_bw(15) +
    theme(strip.background = element_rect(fill = "white"),
          strip.text.x = element_text(size = 16),
          legend.position = c(0.12,.67),
          legend.background = element_blank(),
          legend.box.background = element_rect(fill="white", color = "black", linewidth = 0.3)  #,           legend.key.size = unit(1.5, 'lines')
          ) +
    labs(x = "Day of Burning", y = axis_label, title = plot_title) +
    scale_y_continuous(breaks = c(0.0005, .001,.01,.1,1,10,100), minor_breaks = c(0.005, 0.05, 0.5, 5.0, 50), labels = c("[0]", "0.001","0.01", "0.1", "1", "10","100")  ) +
    scale_x_date(date_labels = "%d-%b", minor_breaks = NULL) +
    coord_trans(y = "log") +
    guides(colour = guide_legend(order = 1, keyheight = 2), 
             shape = guide_legend(order = 2)) +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = seedwall_median, yend = seedwall_median), linewidth = 2, color = "white") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = core_blk_median, yend = core_blk_median), linewidth = 2, color = "white") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = core_brn_median, yend = core_brn_median), linewidth = 2, color = "white") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = seedwall_median, yend = seedwall_median), linewidth = 1.5, color = "#A2D435") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = core_blk_median, yend = core_blk_median), linewidth = 1.5, color = "black") +
    geom_segment(data = windows_foc,aes(x = start-2, xend = end+2, y = core_brn_median, yend = core_brn_median), linewidth = 1.5, color = "#b06d1b")

  png(file.path(datadir, paste0("figures/raw_data_", filename, ".png")), res = 350*2, width = 4500*2, height = 2400*2)
  print(p)
  dev.off()
  
  
  
  # Make alternative version that classifies plots as early/late edge/interior_low/interior_high and
  # plots panels
  
  ### Combine core and seed wall into one DF so can plot as separate facets
  
  # Classify fire intens
  d_sp_nogrn_fig = d_sp_nogrn_fig |>
    mutate(plot_type = ifelse(fire_intens < intensity_threshold, "Interior, low canopy\nburn fraction", "Interior, high canopy\nburn fraction"))
  
  d_sp_sw = d_sp_sw |>
    mutate(plot_type = "Edge")
  
  d_fig = bind_rows(d_sp_nogrn_fig, d_sp_sw) |>
    mutate(plot_type = factor(plot_type, levels = c("Edge", "Interior, low canopy\nburn fraction", "Interior, high canopy\nburn fraction")))
  
  # define early and later burned
  d_fig = d_fig |>
    mutate(burn_date_cat = ifelse(date_of_burning > ymd("2021-08-01"), "Late", "Early") |> as.factor())
  
  # compute the median and interquartile range seedling density for each plot type X burn date
  d_summ = d_fig |>
    group_by(plot_type, burn_date_cat) |>
    summarize(median = median(seedl_dens_sp),
              lwr = quantile(seedl_dens_sp, 0.25),
              upr = quantile(seedl_dens_sp, 0.75))
  
  ### Make figure: for each plot type * burn date:
  p = ggplot(d_fig, aes(x = burn_date_cat, y = seedl_dens_sp, color = plot_type)) +
    geom_hline(yintercept = 0.0173, linetype = "dashed", color="gray70") +
    geom_linerange(data = d_summ, aes(ymin = lwr, ymax = upr, y = NULL), size = 2, position = position_nudge(x = +0.15)) +
    geom_jitter(alpha = 1, size = 2.5, aes(x = as.numeric(burn_date_cat) - 0.15), position = position_jitter(width = 0.1, height = 0)) +
    geom_point(data = d_summ, size = 6, aes(x = burn_date_cat, y = median), position = position_nudge(x = +0.15), shape = 18) +
    facet_wrap(~plot_type) +
    scale_y_continuous(breaks = c(0.0005, .001,.01,.1,1,10,100), minor_breaks = c(0.005, 0.05, 0.5, 5.0, 50), labels = c("[0]", "0.001","0.01", "0.1", "1", "10","100")) +
    scale_colour_manual(values = c("Edge" = "#A2D435",
                              "Interior, low canopy\nburn fraction" = "#b06d1b",
                              "Interior, high canopy\nburn fraction" = "black"), guide = "none") +
    labs(y = axis_label, x = "Burn date") +
    coord_trans(y = "log") +
    theme_bw(18) +
    theme(strip.background = element_rect(fill = "white"),
          panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

  png(file.path(datadir, paste0("figures/raw_seedl_dens_full_", filename, ".png")), res = 350*4, width = 2600*4, height = 1800*4)
  print(p)
  dev.off()
  
}


# Prep core-area data for modeling: filter to later-burned, no green of focal species, and no surviving trees within 100 m
prep_d_core_mod = function(d_sp) {
  d_mod = d_sp |>
    filter(day_of_burning > 220) |>
    filter(grn_vol_abs_sp == 0,
           ((is.na(dist_grn_sp) | dist_grn_sp > minimum_dist_green) & sight_line > minimum_dist_green),
           plot_type %in% c("core", "delayed"))
  
  return(d_mod)
}

# Prep seedwall data for modeling: filter to seed wall plots within a specified distance of seed wall
prep_d_sw_mod = function(d_sp, max_sw_dist) {
  
  d_mod = d_sp |>
    filter(plot_type == "seedwall") |>
    filter(dist_sw <= max_sw_dist)
  
  return(d_mod)
}

# For a fitted model, make predictions along the axis of the variables supplied as the "predictors" parameter (separately for each), holding the remainder at their medians. Optionally, supply a predictor to interact (interacting_predictor), and supply values split it into classes (interactin_splits) (e.g. value splitting precip into high precip and low precip, to interact with fire intensity).
get_scenario_preds = function(m, d_mod, predictors, sp, percentile_exclude, interacting_predictor = NA, interacting_splits = NA) { # interacting_predictor is for a predictor var that you want to predict multiple levels for, and interacting_levels is a vector of quantiles on which to split the predictor for separate predictions

  predictor_foc_preds = data.frame()
  for(predictor_foc in predictors) {
    
    ## truncate each prediction range to exclude the upper and lower x percentile extremes of observed data
    range = d_mod |>
      summarize(lwr = quantile(!!ensym(predictor_foc),percentile_exclude),
                upr = quantile(!!ensym(predictor_foc), 1-percentile_exclude))
    
    # get fitted line for hypothetical variation along focal var (all else set to mean)
    # start with a data frame of all predictors set at their means
    newdat_predictor_foc = d_mod |>
      select(fire_intens, ppt, prefire_prop_sp, vol_brn_50m, cone_dens_sp, cone_dens_sp_log, dist_grn_sp, dist_sw, grn_vol_sp) |>
      summarize_all(mean) |>
      mutate(under_cones_new_sp = "high") |> # for predictions, assume high density of cones under nearby trees (assuming it's included in the model) (need to do this manually instead of mean because it's categorical)
      mutate(seedwall_density_cat = "H") |>
      slice(rep(row_number(), 100)) # get 100 identical (repeated) rows
    
    # now make the focal var a col with a seq from the min obs to max obs
    newdat_predictor_foc[,predictor_foc] =  seq(range$lwr, range$upr, length.out = 100)
    
    # now, replicate that for every level of the interacting predictor, if specified
    # for now, assuming two levels (one quantile split)
    if(!is.na(interacting_predictor)) {
      
      pred_vals = d_mod[,interacting_predictor]
      
      min = min(pred_vals)
      max = max(pred_vals)
      split = interacting_splits
      
      
      lwr_mid = median(pred_vals[pred_vals < split])
      upr_mid = median(pred_vals[pred_vals >= split])
      
      cat("Upper interacting predictor val (median):", upr_mid, "\n")
      cat("Lower interacting predictor val (median):", lwr_mid, "\n")
      
      newdat_predictor_foc_l1 = newdat_predictor_foc |>
        mutate(across(matches(paste0("^",interacting_predictor,"$")), ~lwr_mid),
               interacting_level = "low")
      
      newdat_predictor_foc_l2 = newdat_predictor_foc |>
        mutate(across(matches(paste0("^",interacting_predictor,"$")), ~upr_mid),
               interacting_level = "high")
      
      newdat_predictor_foc = bind_rows(newdat_predictor_foc_l1, newdat_predictor_foc_l2)
      
    }
    
    pred = predict(m, newdat_predictor_foc, type = "link", se.fit=TRUE)
    newdat_predictor_foc$preds = pred$fit
    newdat_predictor_foc$preds_lwr = pred$fit - 2*pred$se.fit
    newdat_predictor_foc$preds_upr = pred$fit + 2*pred$se.fit
    
    newdat_predictor_foc = newdat_predictor_foc |>
      mutate(across(c(preds, preds_lwr, preds_upr), exp)) |>
      mutate(across(c(preds, preds_lwr, preds_upr), ~./314)) |> # divide by plot area to get seedl per sq m
      mutate(predictor_foc = predictor_foc,
             species = sp)
    
    predictor_foc_preds = bind_rows(predictor_foc_preds, newdat_predictor_foc)
  }
  
  return(predictor_foc_preds)
}


# Plot GAM predictions along an axis of one predictor (i.e., torching extent)
make_scenario_ggplot = function(scenario_preds, d_mod, focal_predictor, predictor_label, ymin, ymax) {
  
  # Prep data frame for plotting
  d_mod = d_mod |>
    mutate(type = as.factor(type))
  d_fig = scenario_preds |> filter(predictor_foc == focal_predictor) |>
    mutate(type = factor(type, levels = c("Interior", "Edge")))
    
  p = ggplot(data = d_fig, mapping = aes(x = !!ensym(focal_predictor), y = preds, color = type, fill = type, linetype = species)) +
    geom_ribbon(aes(ymin = preds_lwr, ymax = preds_upr, fill=type), color=NA, alpha = .3, show.legend = FALSE) +
    scale_linetype(name = "Taxon", limits = c("All conifers", "Pines")) +
    scale_color_viridis_d(begin = .2, end = .8, name = "Plot type") +
    scale_fill_viridis_d(begin = .2, end = .8, name = "Plot type") +
    geom_line() +
    scale_y_continuous(breaks = c(.001,.01,.1,1,10,100, 1000), minor_breaks = c(0.0005,0.005, 0.05, 0.5, 5.0, 50, 500), labels = label_comma()) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    coord_trans(y = "log") +
    theme_bw() +
    labs(y = bquote(Seedlings~m^-2), x = predictor_label)
  
  return(p)

}

# Replicate of the above function, but plot only the "all species" predictions (for a presentation figure)
make_scenario_ggplot_allsponly = function(scenario_preds, d_mod, focal_predictor, predictor_label, ymin, ymax) {
  
  # Prep data frame for plotting
  d_mod = d_mod |>
    mutate(type = as.factor(type))
    
  d_fig = scenario_preds |> filter(predictor_foc == focal_predictor) |>
    mutate(type = factor(type, levels = c("Edge", "Interior"))) |>
    filter(species == "All conifers")
    
  p = ggplot(data = d_fig, mapping = aes(x = !!ensym(focal_predictor), y = preds, color = type, fill = type)) +
    geom_line() +
    geom_ribbon(aes(ymin = preds_lwr, ymax = preds_upr, fill=type), color=NA, alpha = .3, show.legend = FALSE) +
    # scale_linetype(name = "Taxon", limits = c("All conifers", "Pines")) +
    scale_color_manual(values = c("Edge" = "#A2D435", "Interior" = "#b06d1b")) +
    scale_fill_manual(values = c("Edge" = "#A2D435", "Interior" = "#b06d1b")) +

    scale_y_continuous(breaks = c(.001,.01,.1,1,10,100, 1000), minor_breaks = c(0.0005,0.005, 0.05, 0.5, 5.0, 50, 500), labels = label_comma(), limits = c(0.1, 30)) +
    coord_cartesian(ylim = c(ymin, ymax)) +
    coord_trans(y = "log") +
    theme_bw() +
    labs(y = bquote(Seedlings~m^-2), x = predictor_label)
  
  return(p)

}


# Plot GAM predictions along an axis of one predictor (i.e., torching extent), interacting with precip (2 levels, split as specified by 'interacting_splits')
make_scenario_w_ppt_ggplot = function(scenario_preds, d_mod, focal_predictor, predictor_label, ymin, ymax, interacting_splits = NA, show_data = FALSE) {

  # prep observed data points for plot
  d_mod = d_mod |> # in d_mod, seedl_dens_sp is actually count of seedlings in a 10 m radius plot (for poisson purposes)
    mutate(seedl_dens_sp = ifelse(seedl_dens_sp < 0.5, 0.0005*314, seedl_dens_sp) / 314) |> # get seedlings per sq m, but with zeros set to 0.0005 so they fit on the log-axis (will be labeled as 0)
    mutate(ppt_cat = ifelse(ppt >= interacting_splits, paste0("≥ ", interacting_splits, " mm"), paste0("< ", interacting_splits, " mm"))) # create a categorical version of precip, split as specified
  
  # prep model predictions for plot
  d_fig = scenario_preds |> filter(predictor_foc == focal_predictor) |>
    mutate(ppt_cat = ifelse(ppt >= interacting_splits, paste0("≥ ", interacting_splits, " mm"), paste0("< ", interacting_splits, " mm")))
  
  d_mod = d_mod |>
    mutate(ppt_cat = as.factor(ppt_cat)) |>
    mutate(ppt_cat = factor(ppt_cat, levels = rev(levels(ppt_cat)))) 
  d_fig = d_fig |>
    mutate(ppt_cat = as.factor(ppt_cat)) |>
    mutate(ppt_cat = factor(ppt_cat, levels = rev(levels(ppt_cat)))) 
  
  p = ggplot(data = d_fig, mapping = aes(x = !!ensym(focal_predictor), y = preds, color = ppt_cat, fill = ppt_cat))
  
  if(show_data) {
    p = p + geom_point(data = d_mod, mapping = aes(y = seedl_dens_sp), size = 1.2)
  }
  
  p2 = p +
    scale_color_viridis_d(option = "plasma", begin = .2, end = .8, name = "Normal annual\nprecipitation") +
    scale_fill_viridis_d(option = "plasma", begin = .2, end = .8, name = "Normal annual\nprecipitation") +
    geom_ribbon(aes(ymin = preds_lwr, ymax = preds_upr), color=NA, alpha = .3, show.legend = FALSE) +
    geom_line() +
    scale_y_continuous(breaks = c(0.0005, .001,.01,.1,1,10,100, 1000), minor_breaks = c(0.005, 0.05, 0.5, 5.0, 50, 500), limits = c(ymin, ymax), labels = c("[0]", "0.001","0.01", "0.1", "1", "10","100", "1000")) +
    coord_trans(y = "log") +
    theme_bw() +
    theme(legend.position = c(0.2,.2),
          legend.background = element_blank(),
          legend.box.background = element_rect(fill="white", color = "black", linewidth = 0.3)) +
    labs(y = bquote(Conifer~seedlings~m^-2), x = predictor_label)
  
  return(p2)
  
}
