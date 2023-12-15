# early-regen-pub

Analysis of early post-fire forest regeneration dynamics and aerial seedbanking, supporting the paper "Aerial seedbanking as a mechanism for forest resilience to megafires".

## Data management for this repository

The code files in this repository are set up to access a data folder located anywhere on the same machine. The path to the data folder should be saved in the file `data-dir.txt`, which each of the scripts reads prior to reading data files. The dataset, with the file names as expected by the code in this repository, is published in the Dryad repository at [link to be provided upon publication]().

## Terminology

The terms "fire intensity" and "torching extent" appearing in variables and comments in the scripts are used synonymously with "canopy burn fraction" referred to in the paper. The terms "core" and "seedwall" refer to the plot types "interior" and "edge", respectively, referred to in the paper.

## Code files in the `scripts/analysis/` folder

**main-field-data-analysis.R:** Code for fitting GAMs to explain seedling density using fire intensity and ancillary predictors, for reporting GAM fit statistics, for plotting GAM fits and raw data, and for extracting some raw data summary statistics.

**main-field-data-analysis_functions.R:** Custom data summary and plotting functions supporting the `main-field-data-analysis.R` script.

**summary-stats-for-paper.R:** Code for computing summary statistics (e.g., seedling densities in specific burn date ranges) reported in the paper.

**plot-kdavis-regen.R:** Code for filtering and plotting post-fire tree seedling density data from a large compilation of post-fire vegetation plots published by [Davis et al. (2023)](https://www.pnas.org/doi/10.1073/pnas.2208120120).

**burn-seasonality-summary.R:** Code for summarizing the percentage of burned area in California that burned within specific date ranges.

**afe-pres-figs.R:** Code to make figures used for oral presentations.
