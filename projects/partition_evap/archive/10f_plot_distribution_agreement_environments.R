# Area fraction of agreement index over environments ----

source('source/partition_evap.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data ----
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))

# Input produced in 
evap_grid <- as.data.table(readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_gridwise.rds")))
evap_grid <-  evap_grid[!is.na(agreement_index),]
evap_grid[, summary(agreement_index)]

## Analysis ----
### Relative Distribution\nagreement at quantile 0.1, 0.3, 0.7. 0.9 ----
quant_thr_0_1 <- quantile(evap_grid$agreement_index, c(0.1))
quant_thr_0_3 <- quantile(evap_grid$agreement_index, c(0.3))
quant_thr_0_7 <- quantile(evap_grid$agreement_index, c(0.7))
quant_thr_0_9 <- quantile(evap_grid$agreement_index, c(0.9))

evap_grid[agreement_index > quant_thr_0_9, agreement_fac := ordered(1, labels = "High")]
evap_grid[agreement_index > quant_thr_0_7 & agreement_index <= quant_thr_0_9, agreement_fac := ordered(2, labels = "Above average")]
evap_grid[agreement_index > quant_thr_0_3 & agreement_index <= quant_thr_0_7, agreement_fac := ordered(3, labels = "Average")]
evap_grid[agreement_index > quant_thr_0_1 & agreement_index <= quant_thr_0_3, agreement_fac := ordered(4, labels = "Below average")]
evap_grid[agreement_index <= quant_thr_0_1, agreement_fac := ordered(5, labels = "Low")] 


cols_agreement <- c("Low" = colset_RdBu_5[1], "Below average" = colset_RdBu_5[2], 
                    "Average" = colset_RdBu_5[3], 
                    "Above average" = colset_RdBu_5[4], "High" = colset_RdBu_5[5])

grid_cell_area <- unique(evap_grid[, .(lon, lat)]) %>% grid_area() # m2
evap_grid <- grid_cell_area[evap_grid, on = .(lon, lat)]

land_cover_class <- merge(evap_mask[, .(lat, lon, land_cover_short_class)], evap_grid[, .(lon, lat, area, agreement_index, agreement_fac)], by = c("lon", "lat"))
biome_class <- merge(evap_mask[, .(lat, lon, biome_class)], evap_grid[, .(lon, lat, area, agreement_index, agreement_fac)], by = c("lon", "lat"))
elevation_class <- merge(evap_mask[, .(lat, lon, elev_class)], evap_grid[, .(lon, lat, area, agreement_index, agreement_fac)], by = c("lon", "lat"))
evap_quant <- merge(evap_mask[, .(lat, lon, evap_quant)], evap_grid[, .(lon, lat, area, agreement_index, agreement_fac)], by = c("lon", "lat"))
IPCC_ref_regions <- merge(evap_mask[, .(lat, lon, IPCC_ref_region)], evap_grid[, .(lon, lat, area, agreement_index, agreement_fac)], by = c("lon", "lat"))

### Land use ----
land_cover_agreement <- land_cover_class[, .(evap_sum = sum(area)), .(agreement_fac, land_cover_short_class)]
land_cover_agreement <- land_cover_agreement[complete.cases(land_cover_agreement)]
land_cover_agreement <- land_cover_agreement[order(agreement_fac, land_cover_short_class), ]
land_cover_agreement[, land_cover_sum := sum(evap_sum), land_cover_short_class]
land_cover_agreement[, land_cover_fraction := evap_sum / land_cover_sum]

### Biome types
biome_class[grepl("Tundra", biome_class) == TRUE, biome_short_class := "Tundra"]
biome_class[grepl("Boreal Forests", biome_class) == TRUE, biome_short_class := "B. Forests"]
biome_class[grepl("Dry Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Dry BL Forests"]
biome_class[grepl("Moist Broadleaf Forests", biome_class) == TRUE, biome_short_class := "T/S Moist BL Forests"]
biome_class[grepl("Subtropical Coniferous Forests", biome_class) == TRUE, biome_short_class := "T/S Coni. Forests"]
biome_class[grepl("Temperate Conifer Forests", biome_class) == TRUE, biome_short_class := "T. Coni. Forests"]
biome_class[grepl("Temperate Broadleaf & Mixed Forests", biome_class) == TRUE, biome_short_class := "T. BL Forests"]
biome_class[grepl("Temperate Grasslands", biome_class) == TRUE, biome_short_class := "T. Grasslands"]
biome_class[grepl("Subtropical Grasslands", biome_class) == TRUE, biome_short_class := "T/S Grasslands"]
biome_class[grepl("Montane Grasslands", biome_class) == TRUE, biome_short_class := "M. Grasslands"]
biome_class[grepl("Flooded", biome_class) == TRUE, biome_short_class := "Flooded"]
biome_class[grepl("Mangroves", biome_class) == TRUE, biome_short_class := "Mangroves"]
biome_class[grepl("Deserts", biome_class) == TRUE, biome_short_class := "Deserts"]
biome_class[grepl("Mediterranean", biome_class) == TRUE, biome_short_class := "Mediterranean"]
biome_class[grepl("N/A", biome_class) == TRUE, biome_short_class := NA]
biome_class[, biome_short_class := factor(biome_short_class)]

biome_agreement <- biome_class[, .(evap_sum = sum(area)), .(agreement_fac, biome_short_class)]
biome_agreement <- biome_agreement[complete.cases(biome_agreement)]
biome_agreement <- biome_agreement[order(agreement_fac, biome_short_class), ]
biome_agreement[, biome_sum := sum(evap_sum), biome_short_class]
biome_agreement[, biome_fraction := evap_sum / biome_sum]

### Elevation ----

elevation_agreement <- elevation_class[, .(evap_sum = sum(area)), .(agreement_fac, elev_class)]
elevation_agreement <- elevation_agreement[complete.cases(elevation_agreement)]
elevation_agreement <- elevation_agreement[order(agreement_fac, elev_class), ]
elevation_agreement[, elev_sum := sum(evap_sum), elev_class]
elevation_agreement[, elev_fraction := evap_sum / elev_sum]

### Evaporation quantiles ----

evap_quant_agreement <- evap_quant[, .(evap_sum = sum(area)), .(agreement_fac, evap_quant)]
evap_quant_agreement <- evap_quant_agreement[complete.cases(evap_quant_agreement)]
evap_quant_agreement <- evap_quant_agreement[order(agreement_fac, evap_quant), ]
evap_quant_agreement[, evap_quant_sum := sum(evap_sum), evap_quant]
evap_quant_agreement[, evap_quant_fraction := evap_sum / evap_quant_sum]


### IPCC ----
IPCC_agreement <- IPCC_ref_regions[, .(evap_sum = sum(area)), .(agreement_fac, IPCC_ref_region)]
IPCC_agreement <- IPCC_agreement[complete.cases(IPCC_agreement)]
IPCC_agreement <- IPCC_agreement[order(agreement_fac, IPCC_ref_region), ]
IPCC_agreement[, IPCC_ref_region_sum := sum(evap_sum), IPCC_ref_region]
IPCC_agreement[, IPCC_ref_region_fraction := evap_sum / IPCC_ref_region_sum]

## Save data
save(land_cover_agreement, biome_agreement, 
      elevation_agreement,evap_quant_agreement, IPCC_agreement,
     file = paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_masks.Rdata"))
load(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_masks.Rdata"))


### Land Use

land_cover_agreement$land_cover_short_class <- factor(land_cover_agreement$land_cover_short_class, 
                                                      levels = c("Forests", "Savannas", "Croplands", "Shrublands", "Grasslands", "Water", "Barren", "Snow/Ice", "Other"))
fig_land_cover_partition_fraction <- ggplot(land_cover_agreement[land_cover_short_class != "Other"]) +
  geom_bar(aes(x = land_cover_short_class, y = land_cover_fraction, fill = agreement_fac), stat = "identity") +
  xlab('Land cover type')  +
  ylab('Fraction')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

### Biomes

fig_biome_partition_fraction <- ggplot(biome_agreement) +
  geom_bar(aes(x = biome_short_class, y = biome_fraction, fill = agreement_fac), stat = "identity") +
  xlab('Biome')  +
  ylab('Fraction')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

### Elevation

fig_elevation_partition_fraction <- ggplot(elevation_agreement) +
  geom_bar(aes(x = elev_class, y = elev_fraction, fill = agreement_fac), stat = "identity") +
  xlab('Elevation [m]')  +
  ylab('Fraction')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

### Evaporation classes

fig_evap_partition_fraction <- ggplot(evap_quant_agreement) +
  geom_bar(aes(x = evap_quant, y = evap_quant_fraction, fill = agreement_fac), stat = "identity") +
  xlab('Evaporation intensity class')  +
  ylab('Fraction')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

IPCC_Africa <- c("ARP", "CAF", "ESAF", "MDG", "NEAF", "SAH", "SEAF", "WAF", "WSAF")
IPCC_Asia <-   c("EAS", "ECA", "ESB",  "RFE", "RAR",  "SAS", "SEA",  "TIB", "WCA", "WSB")
IPCC_Australasia <- c("CAU", "EAU", "NAU", "NZ", "PAC", "SAU")
IPCC_Europe <- c("EEU", "GIC","MED", "NEU", "WCE")
IPCC_Namerica <- c("CAR", "CNA", "ENA", "NCA","NEN", "NWN", "SCA", "WNA")
IPCC_Samerica <- c("NES","NSA","NWS","SAM","SES", "SSA","SWS")

IPCC_agreement[IPCC_ref_region %in% IPCC_Africa, region:= "Africa"]
IPCC_agreement[IPCC_ref_region %in% IPCC_Asia, region:= "Asia"]
IPCC_agreement[IPCC_ref_region %in% IPCC_Australasia, region:= "Australasia"]
IPCC_agreement[IPCC_ref_region %in% IPCC_Europe, region:= "Europe"]
IPCC_agreement[IPCC_ref_region %in% IPCC_Namerica, region:= "North America"]
IPCC_agreement[IPCC_ref_region %in% IPCC_Samerica, region:= "South America"]

IPCC_agreement[, IPCC_ref_region := factor(IPCC_ref_region, levels = c(IPCC_Africa,
                                                                       IPCC_Asia,
                                                                       IPCC_Australasia,
                                                                       IPCC_Europe,
                                                                       IPCC_Namerica,
                                                                       IPCC_Samerica))]

fig_ipcc_fraction_Africa <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Africa]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = agreement_fac), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  theme_light() +
  ggtitle("Africa")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

fig_ipcc_fraction_Asia <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Asia]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = agreement_fac), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  theme_light() +
  ggtitle("Asia")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

fig_ipcc_fraction_Australasia <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Australasia]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = agreement_fac), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  theme_light() +
  ggtitle("Australasia")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))


fig_ipcc_fraction_Europe <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Europe]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = agreement_fac), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  ggtitle("Europe")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

fig_ipcc_fraction_Namerica <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Namerica]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = agreement_fac), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  ggtitle("North America")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

fig_ipcc_fraction_Samerica <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Samerica]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = agreement_fac), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction')  +
  labs(fill = 'Distribution\nagreement')  +
  scale_fill_manual(values = cols_agreement) +
  ggtitle("South America")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))



## Composite Figures ---- 
gg_fig_land <- ggarrange(fig_land_cover_partition_fraction,fig_biome_partition_fraction,
                      labels = c('a', 'b'), align = 'hv',
                      common.legend = T, legend = 'right', 
                      nrow = 1, ncol = 2, widths = c(0.75, 1.1))

gg_ipcc <-  ggarrange(fig_ipcc_fraction_Africa, fig_ipcc_fraction_Asia, fig_ipcc_fraction_Australasia,
                      fig_ipcc_fraction_Europe, fig_ipcc_fraction_Namerica, fig_ipcc_fraction_Samerica,
                      labels = c('c', 'd', 'e', 'f', 'g', 'h'),
                      ncol = 3, nrow = 2, common.legend = T, align = 'hv', legend = 'none')

gg_fig_main <- ggarrange(gg_fig_land, gg_ipcc,
                          common.legend = T, legend = 'right', 
                          nrow = 2, heights = c(0.7,1),
                          labels = c('', ''))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, 
              "fig3_main_environments_distribution_agreement.png"), width = 10, height = 10)


gg_fig_SI <- ggarrange(fig_elevation_partition_fraction, fig_evap_partition_fraction,
                        labels = c('a', 'b'), align = 'hv',
                        common.legend = T, legend = 'right', 
                        nrow = 1, ncol = 2)

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "fig3_SI_environments_distribution_agreement.png"), width = 8, height = 5)
