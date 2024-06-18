# Partitions total evaporation to different classes and creates the bar plots 
# of climate types and Quartile\nagreement

source('source/partition_evap.R')
source('source/graphics.R')

## Data 
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
evap_grid <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))

## Variables
evap_mask[, KG_class_1_name := factor(KG_class_1_name, levels = levels(evap_mask$KG_class_1_name)[c(5, 4, 2, 3, 1)])]
levels(evap_mask$rel_dataset_agreement) <- c("High", "Above average", "Average", "Below average", "Low")

land_cover_class <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, land_cover_short_class, KG_class_1_name)], evap_grid[, .(lon, lat, evap_volume)], by = c("lon", "lat"))
biome_class <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, biome_short_class, KG_class_1_name)], evap_grid[, .(lon, lat, evap_volume)], by = c("lon", "lat"))
elevation_class <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, elev_class, KG_class_1_name)], evap_grid[, .(lon, lat, evap_volume)], by = c("lon", "lat"))
evap_quant <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, evap_quant, KG_class_1_name)], evap_grid[, .(lon, lat, evap_volume)], by = c("lon", "lat"))
IPCC_ref_regions <- merge(evap_mask[, .(lat, lon, rel_dataset_agreement, IPCC_ref_region, KG_class_1_name)], evap_grid[, .(lon, lat, evap_volume)], by = c("lon", "lat"))

## Analysis
### Land use
land_cover_evap <- land_cover_class[, .(evap_sum = sum(evap_volume)), .(KG_class_1_name, land_cover_short_class)]
land_cover_evap <- land_cover_evap[complete.cases(land_cover_evap)]
land_cover_evap <- land_cover_evap[order(KG_class_1_name, land_cover_short_class), ]

land_cover_agreement <- land_cover_class[, .(evap_sum = sum(evap_volume)), .(rel_dataset_agreement, land_cover_short_class)]
land_cover_agreement <- land_cover_agreement[complete.cases(land_cover_agreement)]
land_cover_agreement <- land_cover_agreement[order(rel_dataset_agreement, land_cover_short_class), ]
land_cover_agreement[, land_cover_sum := sum(evap_sum), land_cover_short_class]
land_cover_agreement[, land_cover_fraction := evap_sum / land_cover_sum]

data_high <- land_cover_agreement[rel_dataset_agreement == "High" | rel_dataset_agreement == "Above average", 
                             .(fraction = sum(land_cover_fraction)), land_cover_short_class]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

land_cover_agreement[, land_cover_short_class := factor(land_cover_short_class, levels = data_high$land_cover_short_class)]

### Biome types
biome_evap <- biome_class[, .(evap_sum = sum(evap_volume)), .(KG_class_1_name, biome_short_class)]
biome_evap <- biome_evap[complete.cases(biome_evap)]
biome_evap <- biome_evap[order(KG_class_1_name, biome_short_class), ]

biome_agreement <- biome_class[, .(evap_sum = sum(evap_volume)), .(rel_dataset_agreement, biome_short_class)]
biome_agreement <- biome_agreement[complete.cases(biome_agreement)]
biome_agreement <- biome_agreement[order(rel_dataset_agreement, biome_short_class), ]
biome_agreement[, biome_sum := sum(evap_sum), biome_short_class]
biome_agreement[, biome_fraction := evap_sum / biome_sum]


data_high <- biome_agreement[rel_dataset_agreement == "High" | rel_dataset_agreement == "Above average", 
                              .(fraction = sum(biome_fraction)), biome_short_class]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

biome_agreement[, biome_short_class := factor(biome_short_class, levels = data_high$biome_short_class)]

### Elevation
elevation_evap <- elevation_class[, .(evap_sum = sum(evap_volume)), .(KG_class_1_name, elev_class)]
elevation_evap <- elevation_evap[complete.cases(elevation_evap)]
elevation_evap <- elevation_evap[order(KG_class_1_name, elev_class), ]

elevation_agreement <- elevation_class[, .(evap_sum = sum(evap_volume)), .(rel_dataset_agreement, elev_class)]
elevation_agreement <- elevation_agreement[complete.cases(elevation_agreement)]
elevation_agreement <- elevation_agreement[order(rel_dataset_agreement, elev_class), ]
elevation_agreement[, elev_sum := sum(evap_sum), elev_class]
elevation_agreement[, elev_fraction := evap_sum / elev_sum]

data_high <- elevation_agreement[rel_dataset_agreement == "High" | rel_dataset_agreement == "Above average", 
                             .(fraction = sum(elev_fraction)), elev_class]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

elevation_agreement[, elev_class := factor(elev_class, levels = data_high$elev_class)]

### Evaporation quantiles
evap_quant_evap <- evap_quant[, .(evap_sum = sum(evap_volume)), .(KG_class_1_name, evap_quant)]
evap_quant_evap <- evap_quant_evap[complete.cases(evap_quant_evap)]
evap_quant_evap <- evap_quant_evap[order(KG_class_1_name, evap_quant), ]

evap_quant_agreement <- evap_quant[, .(evap_sum = sum(evap_volume)), .(rel_dataset_agreement, evap_quant)]
evap_quant_agreement <- evap_quant_agreement[complete.cases(evap_quant_agreement)]
evap_quant_agreement <- evap_quant_agreement[order(rel_dataset_agreement, evap_quant), ]
evap_quant_agreement[, evap_quant_sum := sum(evap_sum), evap_quant]
evap_quant_agreement[, evap_quant_fraction := evap_sum / evap_quant_sum]

data_high <- evap_quant_agreement[rel_dataset_agreement == "High" | rel_dataset_agreement == "Above average", 
                                 .(fraction = sum(evap_quant_fraction)), evap_quant]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

evap_quant_agreement[, evap_quant := factor(evap_quant, levels = data_high$evap_quant)]

### IPCC

IPCC_ref_regions_evap <- IPCC_ref_regions[, .(evap_sum = sum(evap_volume)), .(KG_class_1_name, IPCC_ref_region)]
IPCC_ref_regions_evap <- IPCC_ref_regions_evap[complete.cases(IPCC_ref_regions_evap)]
IPCC_ref_regions_evap <- IPCC_ref_regions_evap[order(KG_class_1_name, IPCC_ref_region), ]

IPCC_agreement <- IPCC_ref_regions[, .(evap_sum = sum(evap_volume)), .(rel_dataset_agreement, IPCC_ref_region)]
IPCC_agreement <- IPCC_agreement[complete.cases(IPCC_agreement)]
IPCC_agreement <- IPCC_agreement[order(rel_dataset_agreement, IPCC_ref_region), ]
IPCC_agreement[, IPCC_ref_region_sum := sum(evap_sum), IPCC_ref_region]
IPCC_agreement[, IPCC_ref_region_fraction := evap_sum / IPCC_ref_region_sum]

data_high <- IPCC_agreement[rel_dataset_agreement == "High" | rel_dataset_agreement == "Above average", 
                                  .(fraction = sum(IPCC_ref_region_fraction)), IPCC_ref_region]
data_high[, rank := rank(-fraction)]
data_high <- data_high[order(fraction)]

IPCC_agreement[, IPCC_ref_region := factor(IPCC_ref_region, levels = data_high$IPCC_ref_region)]

## Save data
save(land_cover_evap, land_cover_agreement, biome_evap, biome_agreement, 
     elevation_evap, elevation_agreement, evap_quant_evap, evap_quant_agreement, IPCC_ref_regions_evap, IPCC_agreement,
     file = paste0(PATH_SAVE_PARTITION_EVAP, "partition_evap.Rdata"))
load(paste0(PATH_SAVE_PARTITION_EVAP, "partition_evap.Rdata"))

## Figures Main
### Land Use

fig_land_cover_partition_fraction <- ggplot(land_cover_agreement[land_cover_short_class != "Other"]) +
  geom_bar(aes(x = land_cover_short_class, y = land_cover_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Land cover type')  +
  ylab('Fraction')  +
  labs(fill = 'Quartile\nagreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))
### Biomes

fig_biome_partition_fraction <- ggplot(biome_agreement) +
  geom_bar(aes(x = biome_short_class, y = biome_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Biome')  +
  ylab('Fraction')  +
  labs(fill = 'Quartile\nagreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

### Elevation

fig_elevation_partition_fraction <- ggplot(elevation_agreement) +
  geom_bar(aes(x = elev_class, y = elev_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Elevation [m]')  +
  ylab('Fraction')  +
  labs(fill = 'Quartile\nagreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

### Evaporation classes

fig_evap_partition_fraction <- ggplot(evap_quant_agreement) +
  geom_bar(aes(x = evap_quant, y = evap_quant_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Evaporation intensity class')  +
  ylab('Fraction')  +
  labs(fill = 'Quartile\nagreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

### IPCC 
# IPCC ####
IPCC_Africa <- c("ARP", "CAF", "ESAF", "MDG", "NEAF", "SAH", "SEAF", "WAF", "WSAF")
IPCC_Asia <-   c("EAS", "ECA", "ESB",  "RFE", "RAR",  "SAS", "SEA",  "TIB", "WCA", "WSB")
IPCC_Australasia <- c("CAU", "EAU", "NAU", "NZ", "PAC", "SAU")
IPCC_Europe <- c("EEU", "GIC","MED", "NEU", "WCE")
IPCC_Namerica <- c("CAR", "CNA", "ENA", "NCA","NEN", "NWN", "SCA", "WNA")
IPCC_Samerica <- c("NES","NSA","NWS","SAM","SES", "SSA","SWS")

fig_ipcc_fraction_Africa <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Africa]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction')  +
  labs(fill = 'Quartile\nagreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  theme_light() +
  ggtitle("Africa")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

fig_ipcc_fraction_Asia <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Asia]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction')  +
  labs(fill = 'Quartile\nagreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  theme_light() +
  ggtitle("Asia")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

fig_ipcc_fraction_Australasia <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Australasia]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction')  +
  labs(fill = 'Quartile\nagreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  theme_light() +
  ggtitle("Australasia")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))


fig_ipcc_fraction_Europe <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Europe]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction')  +
  labs(fill = 'Quartile\nagreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  ggtitle("Europe")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

fig_ipcc_fraction_Namerica <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Namerica]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction')  +
  labs(fill = 'Quartile\nagreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  ggtitle("North America")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

fig_ipcc_fraction_Samerica <- ggplot(IPCC_agreement[IPCC_ref_region %in% IPCC_Samerica]) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction')  +
  labs(fill = 'Quartile\nagreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  ggtitle("South America")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

fig_ipcc_fraction <- ggplot(IPCC_agreement) +
  geom_bar(aes(x = IPCC_ref_region, y = IPCC_ref_region_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('IPCC reference regions')  +
  ylab('Fraction')  +
  labs(fill = 'Quartile\nagreement')  +
  scale_fill_manual(values = rev(colset_RdBu_5)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

### Figure 2
gg_land <- ggarrange(fig_land_cover_partition_fraction,fig_biome_partition_fraction,
                           labels = c('a', 'b'), align = 'hv',
                           common.legend = T, legend = 'right', 
                           nrow = 1, ncol = 2, widths = c(0.75, 1.1))

gg_ipcc <-  ggarrange(fig_ipcc_fraction_Africa, fig_ipcc_fraction_Asia, fig_ipcc_fraction_Australasia,
                      fig_ipcc_fraction_Europe, fig_ipcc_fraction_Namerica, fig_ipcc_fraction_Samerica,
                      labels = c('c', 'd', 'e', 'f', 'g', 'h'),
                      ncol = 3, nrow = 2, common.legend = T, align = 'hv', legend = 'none')

gg_fig_main <- ggarrange(gg_land, gg_ipcc,
                          common.legend = T, legend = 'right', 
                          nrow = 2, heights = c(0.7,1),
                          labels = c('', ''))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "main/fig3_main_partition_fraction_agreement_rel_dataset_agreement.png"), width = 10, height = 10)

gg_fig_SI <- ggarrange(fig_elevation_partition_fraction, fig_evap_partition_fraction,
                        labels = c('a', 'b'), align = 'hv',
                        common.legend = T, legend = 'right', 
                        nrow = 1, ncol = 2)

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "supplement/fig3_SI_partition_fraction_agreement_rel_dataset_agreement.png"), 
       width = 8, height = 3)
