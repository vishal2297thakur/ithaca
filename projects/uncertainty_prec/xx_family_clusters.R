# Supplementary figure: Correlation network
source("source/uncertainty_prec.R")

library(corrr)
options(ggrepel.max.overlaps = Inf) 

## Load data
prec_month <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_month.rds"))

prec_years <- readRDS(paste0(PATH_SAVE_UNCERTAINTY_PREC, "prec_data_years.rds"))

## Analyses
prec_month <- prec_month[dataset != "ncep-doe" & dataset != "ncep-ncar"]

prec_years <- prec_years[dataset != "ncep-doe" & dataset != "ncep-ncar"]

prec_month <- dcast(prec_month, lon + lat + date ~ dataset, value.var = 'prec')

prec_years <- dcast(prec_years, lon + lat + date ~ dataset, value.var = 'prec')

cor_matrix_month <- correlate(prec_month[, -(1:3)])

cor_matrix_years <- correlate(prec_years[, -(1:3)])

## Plot
png(filename = paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES,
                      "cor_network_month.png"), units = "cm", res = 600,
    width = 18*GOLDEN_RATIO, height = 18)
network_plot(cor_matrix_month, min_cor = 0, legend = 'range',
             colors = hcl.colors(10, "Viridis"))
dev.off()

png(filename = paste0(PATH_SAVE_UNCERTAINTY_PREC_FIGURES,
                      "cor_network_years.png"), units = "cm", res = 600,
    width = 18*GOLDEN_RATIO, height = 18)
network_plot(cor_matrix_years, min_cor = 0, legend = 'range',
             colors = hcl.colors(10, "Viridis"))
dev.off()
