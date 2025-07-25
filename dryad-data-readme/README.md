# Data files supporting the paper "Aerial seedbanking: an unexpected source of forest resilience to extreme wildfire"

## `fire_daily_area_2002_2021.csv`

Total area burned by year and day of year for California fires burning between 2002 and 2021. Tabular data was derived by summarizing fire progression maps for the California fires burning between 2002 and 2020 as published by Coop et al. (2022), produced using the methods described above, and provided by S. Parks. We combined these 2002-2020 data with day of burning maps that we calculated, using the same methods, for California fires > 404 ha burning in 2021 based on perimeters from the Monitoring Trends in Burn Severity (MTBS) database (Eidenshink et al. 2007).

## `plot-data-prepped.csv`

Field data from high-severity burned areas of the 2021 Caldor and Dixie Fires collected for the
paper. Each row represents a field plot. The dataset includes all field plots sampled by the
project, including those excluded from the analyses supporting the paper, based on the plot
selection criteria described in the paper. The dataset also includes field plot attributes beyond
those analyzed in the paper, to enable future extensions of the analysis. Data were collected by a
crew who visited each plot and recorded their observations. Percent covers and cone densities are
recorded for the circle with 8 m radius surrounding plot center. Seedling densities are estimated by
counting seedlings within a radius of plot center determined by the seedling abundance. Note that
this dataset includes the full set of surveyed plots, from which a subset was selected for the
analyses supporting the accompanying paper (following the plot inclusion criteria described in the
published paper). For full plot selection, subsetting, and mensuration methodology, please see the
methods section of the accompanying paper. Fields (columns) include taxon abbreviations as follows:

*   YLWPINES: yellow pines, i.e. *Pinus ponderosa* and *Pinus jeffreyi*
*   PILA: *Pinus lambertiana*
*   ABCO: *Abies concolor*
*   PSME: *Pseudotsuga menziesii*
*   CADE: *Calocedrus decurrens*
*   PINES: *Pinus* spp.
*   ABIES: *Abies* spp.
*   ALL: all species
*   FIRS: *Abies* spp. and *Pseudotsuga menziesii*

The fields (columns) are as follows:

*   fire: The name of the fire where the plot was sampled
*   plot_id: A unique plot ID
*   plot_type: `seedwall` for edge plots, `core` for interior plots, and `delayed` for additional interior plots. The latter plot type refers to "delayed mortality" plots, which were designed to capture highly injured by not completely killed trees that were expected to die in future years. Some of these plot were far enough from the highly injured green trees that they satisfied the criteria for interior plots for the paper. Edge plots are those near an unburned forest edge, and interior plots are those far from healthy surviving green trees.
*   survey_date: The date the plot was surveyed
*   lat and lon: The geographic coordinates of the plot, in decimal degrees, rounded to the nearest thousandth
*   shrub_cover: The shrub cover in the plot (%)
*   shrub_height: The mean height of shrubs in the plot (cm). Only recorded if shrub cover > 1%
*   herb_cover: The cover of herbaceous vegetation in the plot (%)
*   herb_height: The mean height of herbaceous vegetation in the plot (cm). Only recorded if herb cover > 1%
*   litter_cover: The cover of litter in the plot (%)
*   litter_depth: The depth of litter in the plot, sampled at a representative point where litter was present
*   moss_cover: The cover of moss in the plot (%)
*   branches_cover: The cover of the plot by dropped branches (%)
*   vol_*aaa*_*bbb*: The estimated percentage of the pre-fire foliage volume that is now green (*aaa* = grn), brown (*aaa* = brn), or missing (*aaa* = blk) within eith a 50 m radius (*bbb* = 50m) or 10 m radius (*bbb* = 10m) surrounding the plot
*   sight_line: The distance to the nearest obstruction (in any direction) that would prevent viewing a surviving or scorched tree behind it (m). Measured using a laser rangefinder
*   mean_tree_dbh: The diameter at breast height of the the (fire-killed) tree that most closely matches the mean tree size (of trees > 20 cm DBH), within 50 m of the plot center (cm)
*   mean_seedwall_height: The mean height of trees on the near side of the nearest unburned forest edge (recorded for edge plots only) (m). Measured using a laser rangefinder
*   prefire_prop_*aaaa*: The estimated prefire proportion of trees of taxon *aaaa* (%)
*   seedwall_density_cat: The categorical density/abundance of trees the seed wall (surviving forest edge): L for < 25 trees, H for an entire continuous intact forest, and M for an intermediate value
*   dom_yellpine_cone_sp: The species of the most abundant yellow pine cone in the plot. pije = *Pinus jeffreyi*, pipo = *Pinus ponderosa*
*   seedl_dens_*aaaa*: The density of seedlings of taxon *aaaa* within the field plot (seedlings m^-2)
*   cone_dens_*aaaa*: The density of cones of taxon *aaaa* within the field plot (cones m^-2)
*   under_cones_new_*aaaa*: The categorical density of cones of taxon *aaaa* under the nearest canopy-dominant tree of the taxon. The value is recorded as "low" if there were < 10 conspecific cones (except < 5 for *Pinus lambertiana*) underneath the tree, and "high" otherwise
*   dist_grn_*aaaa*: The distance to the nearest visible tree of taxon *aaaa* with >= 5% of its prefire green canopy volume remaining (m). For edge plots, the distance to the green edge is recorded in the column dist_grn_ALL
*   *aaaa*_green_vol: Of the current green canopy volume, the proportion contributed by taxon *aaaa* (%)
*   *aaaa*_green_vol_abs: Of the estimated prefire green canopy volume, the proportion that is currently green and is of taxon *aaaa* (%)
*   ba: Live and dead tree basal area at the plot, estimated using a basal area gauge (variable-radius method) (ft^2 ac^-1)
*   cabable_growing_area: The percentage of the plot area that is not covered by large woody debris on the ground and rocks > 10 cm in the short dimension (%)
*   fire_intens: Canopy burn fraction, computed by subtracting from 100 the average of the percent litter cover and scorched canopy foliage percent (%)
*   day_of_burning: The day of year (1 to 365) that the plot was estimated to have burned, following the methods described above under the folder `day-of-burning-rasters/`
*   ppt: Normal annual precipitation (1981-2010 mean) extracted from the 800-m resolution PRISM dataset (PRISM Climate Group 2022) using bilinear interpolation (mm)
*   tmean: Normal annual temperature (1981-2010 mean) extracted from the 800-m resolution PRISM dataset (PRISM Climate Group 2022) using bilinear interpolation (deg C)

