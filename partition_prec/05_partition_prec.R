# Partition precipitation to Koppen-Geiger classes and quantify their uncertainty

source('source/partition_prec.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
prec_grid <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_mean_grid.rds"))

## Variables
elev_class <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, elev_class)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))
koppen_class <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, KG_class_1)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))
land_use_class <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, land_use_class)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))
prec_class <- merge(prec_mask[, .(lat, lon, rel_dataset_agreement, prec_class)], prec_grid[, .(lon, lat, prec_volume_year)], by = c("lon", "lat"))

## Analysis
### Climate 
dataset_agreement_koppen <- koppen_class[, .N, .(rel_dataset_agreement, KG_class_1)]
dataset_agreement_koppen <- dataset_agreement_koppen[complete.cases(dataset_agreement_koppen)]
dataset_agreement_koppen <- dataset_agreement_koppen[order(rel_dataset_agreement, KG_class_1), ]
dataset_agreement_koppen[, koppen_sum := sum(N), KG_class_1]
dataset_agreement_koppen[, koppen_fraction := N/koppen_sum]

dataset_agreement_koppen_prec <- koppen_class[, .(prec_sum = sum(prec_volume_year)), .(rel_dataset_agreement, KG_class_1)]
dataset_agreement_koppen_prec <- dataset_agreement_koppen_prec[complete.cases(dataset_agreement_koppen_prec)]
dataset_agreement_koppen_prec <- dataset_agreement_koppen_prec[order(rel_dataset_agreement, KG_class_1), ]

### Elevation
dataset_agreement_elevation <- elev_class[, .N, .(rel_dataset_agreement, elev_class)]
dataset_agreement_elevation <- dataset_agreement_elevation[complete.cases(dataset_agreement_elevation)]
dataset_agreement_elevation <- dataset_agreement_elevation[order(rel_dataset_agreement, elev_class), ]
dataset_agreement_elevation[, elev_sum := sum(N), elev_class]
dataset_agreement_elevation[, elev_fraction := N/elev_sum]

dataset_agreement_elevation_prec <- elev_class[, .(prec_sum = sum(prec_volume_year)), .(rel_dataset_agreement, elev_class)]
dataset_agreement_elevation_prec <- dataset_agreement_elevation_prec[complete.cases(dataset_agreement_elevation_prec)]
dataset_agreement_elevation_prec <- dataset_agreement_elevation_prec[order(rel_dataset_agreement, elev_class), ]

### Land use
dataset_agreement_land_use <- land_use_class[, .N, .(rel_dataset_agreement, land_use_class)]
dataset_agreement_land_use <- dataset_agreement_land_use[complete.cases(dataset_agreement_land_use)]
dataset_agreement_land_use <- dataset_agreement_land_use[order(rel_dataset_agreement, land_use_class), ]
dataset_agreement_land_use[, land_use_sum := sum(N), land_use_class]
dataset_agreement_land_use[, land_use_fraction := N/land_use_sum]

dataset_agreement_land_use_prec <- land_use_class[, .(prec_sum = sum(prec_volume_year)), .(rel_dataset_agreement, land_use_class)]
dataset_agreement_land_use_prec <- dataset_agreement_land_use_prec[complete.cases(dataset_agreement_land_use_prec)]
dataset_agreement_land_use_prec <- dataset_agreement_land_use_prec[order(rel_dataset_agreement, land_use_class), ]

### Prec class
dataset_agreement_prec <- prec_class[, .N, .(rel_dataset_agreement, prec_class)]
dataset_agreement_prec <- dataset_agreement_prec[complete.cases(dataset_agreement_prec)]
dataset_agreement_prec <- dataset_agreement_prec[order(rel_dataset_agreement, prec_class), ]
dataset_agreement_prec[, prec_sum := sum(N), prec_class]
dataset_agreement_prec[, prec_fraction := N/prec_sum]

dataset_agreement_prec_prec <- prec_class[, .(prec_sum = sum(prec_volume_year)), .(rel_dataset_agreement, prec_class)]
dataset_agreement_prec_prec <- dataset_agreement_prec_prec[complete.cases(dataset_agreement_prec_prec)]
dataset_agreement_prec_prec <- dataset_agreement_prec_prec[order(rel_dataset_agreement, prec_class), ]

## Figures
fig_elevation_partition_prec_volume <- ggplot(dataset_agreement_elevation_prec) +
  geom_bar(aes(x = elev_class, y = prec_sum, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Elevation [m]')  +
  ylab('Precipitation sum [km3/year]')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_mid[c(10, 9, 6, 3, 1)]) +
  theme_light()

fig_elevation_partition_fraction <- ggplot(dataset_agreement_elevation) +
  geom_bar(aes(x = elev_class, y = elev_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Elevation [m]')  +
  ylab('Area fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_mid[c(10, 9, 6, 3, 1)]) +
  theme_light()

fig_koppen_partition_prec_volume <- ggplot(dataset_agreement_koppen_prec) +
  geom_bar(aes(x = KG_class_1, y = prec_sum, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Koppen-Geiger class')  +
  ylab('Precipitation sum [km3/year]')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_mid[c(10, 9, 6, 3, 1)]) +
  theme_light()

fig_koppen_partition_fraction <- ggplot(dataset_agreement_koppen) +
  geom_bar(aes(x = KG_class_1, y = koppen_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Koppen-Geiger class')  +
  ylab('Area fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_mid[c(10, 9, 6, 3, 1)]) +
  theme_light()

fig_prec_partition_prec_volume <- ggplot(dataset_agreement_prec_prec) +
  geom_bar(aes(x = prec_class, y = prec_sum, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Precipitation type')  +
  ylab('Precipitation sum [km3/year]')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_mid[c(10, 9, 6, 3, 1)]) +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

fig_prec_partition_fraction <- ggplot(dataset_agreement_prec) +
  geom_bar(aes(x = prec_class, y = prec_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Precipitation type')  +
  ylab('Area fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_mid[c(10, 9, 6, 3, 1)]) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

fig_land_use_partition_prec_volume <- ggplot(dataset_agreement_land_use_prec) +
  geom_bar(aes(x = land_use_class, y = prec_sum, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Land use type')  +
  ylab('Precipitation sum [km3/year]')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_mid[c(10, 9, 6, 3, 1)]) +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

fig_land_use_partition_fraction <- ggplot(dataset_agreement_land_use) +
  geom_bar(aes(x = land_use_class, y = land_use_fraction, fill = rel_dataset_agreement), stat = "identity") +
  xlab('Land use type')  +
  ylab('Area fraction')  +
  labs(fill = 'Dataset agreement')  +
  scale_fill_manual(values = colset_mid[c(10, 9, 6, 3, 1)]) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

gg_all <- ggarrange(fig_prec_partition_prec_volume, fig_prec_partition_fraction,
                    fig_koppen_partition_prec_volume, fig_koppen_partition_fraction, 
                    fig_elevation_partition_prec_volume, fig_elevation_partition_fraction,
                    fig_land_use_partition_prec_volume, fig_land_use_partition_fraction,
                    labels = c('a', '', 'b', '', 'c', '', 'd', ''),
                    common.legend = T, legend = 'right', 
                    nrow = 4, ncol = 2)

ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "01_prec_partition.png"), width = 12, height = 15)
