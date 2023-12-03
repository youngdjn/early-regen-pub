## Make figs for AFE pres

library(ggplot2)
library(here)

datadir = readLines(here("data_dir.txt"), n=1)

# Bar plot of percent pine and fir in scorched and torched

d = data.frame(prop = c(76, 24, 27, 73),
               taxon = c("Fir", "Pine", "Fir", "Pine"),
               intens = c("Scorched", "Scorched", "Torched", "Torched"))

p = ggplot(d, aes(fill = intens, x = taxon, y = prop)) +
    geom_bar(stat = "identity", width = 0.5) +
    facet_grid(~intens) +
    theme_bw() +
    scale_fill_manual(values = c("Scorched" = "#b06d1b", "Torched" = "black"), guide = FALSE) +
    theme(strip.background = element_rect(fill = NA), panel.grid = element_blank()) +
    labs(x = NULL, y = "Relative abundance (%)")

png(file.path(datadir, "figures/afe-pres-species.png"), res = 300, width = 800, height = 1000)
print(p)
dev.off()



## Make the hypothetical dispersal plot

d = data.frame(dist = c(30, 100, 200, 300, 400),
               theoretical = c(100,22.2, 3.4, 0.7, 0.1))

p = ggplot(d , aes(x = dist, y = theoretical)) +
    geom_rect(xmin = 100, xmax = 600, ymin = 15.0, ymax = 48.0, fill = "#71c571", alpha = 0.2) +
    geom_line(linewidth = 2, color = "slateblue") +
    theme_bw() +
    geom_hline(yintercept = 0.38, lty = 2) +
    geom_hline(yintercept = 15, lty = 2) +
    geom_hline(yintercept = 48, lty = 2) +
    labs(x = "Distance from edge (m)", y = "Relative seed/seedling density (% of edge)")

png(file.path(datadir, "figures/afe-pres-theoretical-dens.png"), res = 200, width = 1200, height = 1000)
print(p)
dev.off()
