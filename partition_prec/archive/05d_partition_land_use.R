# Partition precipitation to Koppen-Geiger classes and quantify their uncertainty

source('source/blueprint.R')
source('source/graphics.R')
source('source/geo_functions.R')

## Data 
prec_mask <- readRDS(paste0(PATH_SAVE_BLUEPRINT, "prec_masks.rds"))
prec_mean_datasets <- readRDS(paste0(PATH_SAVE_BLUEPRINT, "prec_mean_datasets.rds"))

## Variables
needed_for_cumsum <- expand.grid(prec_mask[, unique(rel_dataset_agreement)], prec_mask[, unique(land_use_class)])
colnames(needed_for_cumsum) <- c("rel_dataset_agreement", 'land_use_class')

### Weighted Average
prec_weights <- prec_mask[, .(lon, lat)] %>% spatial_weight()
prec_mask <- merge(prec_mask, prec_weights, by = c("lon", "lat"))
prec_mask <- prec_mask[, prec_weight := prec_mean * weight, by = .(lon, lat)
][, weight := NULL]

## Analysis
land_use_class <- prec_mask[, .(sum_land_use_class = sum(prec_weight)), 
                        .(land_use_class, rel_dataset_agreement)]
land_use_class <-  merge(land_use_class, needed_for_cumsum, by = c('land_use_class', "rel_dataset_agreement"), all.y = TRUE)
land_use_class <- land_use_class[order(land_use_class, rel_dataset_agreement), ]
land_use_class[is.na(sum_land_use_class), sum_land_use_class := 0]

land_use_class_cum <- land_use_class[, .(cumsum_land_use_class = cumsum(sum_land_use_class), 
                                 rel_dataset_agreement), .(land_use_class)]
land_use_class_cum[, fraction_bias := cumsum_land_use_class  / sum(cumsum_land_use_class), rel_dataset_agreement]

land_use_class_prec <- prec_mask[, .(lon, lat, prec_mean, land_use_class, rel_dataset_agreement)]

land_use_class_datasets <- merge(prec_mask[, .(lon, lat, land_use_class, rel_dataset_agreement)], prec_mean_datasets, by = c("lat", "lon"))
land_use_class_datasets <- land_use_class_datasets[, .(sum_land_use_class = sum(prec_mean)), .(land_use_class, rel_dataset_agreement, dataset)]
land_use_class_datasets <-  merge(land_use_class_datasets, needed_for_cumsum, by = c('land_use_class', "rel_dataset_agreement"), all.y = TRUE)
land_use_class_datasets <- land_use_class_datasets[order(land_use_class, rel_dataset_agreement, dataset), ]
land_use_class_datasets_cum <- land_use_class_datasets[, .(cumsum_land_use_class = cumsum(sum_land_use_class), 
                                                   rel_dataset_agreement), .(land_use_class, dataset)]
land_use_class_datasets_cum[, fraction_bias := cumsum_land_use_class  / sum(cumsum_land_use_class), .(rel_dataset_agreement, dataset)]

## Validation
ggplot() +
  geom_raster(data = prec_mask, aes(lon, lat, fill = land_use_class)) +
  geom_point(data = prec_mask[rel_dataset_agreement == 'high', .(lon, lat)],  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid_qual) +
  coord_sf(default_crs = sf::st_crs(3395)) + #Mercator
  labs(fill = 'Precipitation')  +
  theme_light()

ggplot(prec_mask) +
  geom_bar(aes(x = land_use_class)) + 
  theme_light()

## Figures
### Main: Partition (%) of total precipitation per climatology for different levels of dataset agreement 
ggplot(land_use_class_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = land_use_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'KG class')  +
  scale_fill_manual(values = colset_mid_qual) +
  theme_light()

ggplot(land_use_class_prec) +
  geom_boxplot(aes(y = prec_mean, fill = rel_dataset_agreement)) +
  facet_wrap(~land_use_class, scales = 'free') +
  labs(fill = 'Dataset agreement')  +
  ylab('Mean monthly precipitation')  +
  scale_fill_manual(values = colset_mid_qual[c(1, 4, 3, 2, 5)]) +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

ggplot(land_use_class_datasets_cum[rel_dataset_agreement == 'high']) +
  geom_bar(aes(x = dataset, y = fraction_bias , fill = land_use_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'KG class')  +
  scale_fill_manual(values = colset_mid_qual) +
  theme_light()

ggplot(land_use_class_datasets_cum[rel_dataset_agreement == 'average' & dataset %in% PREC_DATASETS_OBS]) +
  geom_bar(aes(x = dataset, y = fraction_bias, fill = dataset), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  facet_wrap(~land_use_class, scales = 'free') +
  scale_fill_manual(values = colset_mid_qual) +
  labs(fill = 'KG class')  +
  theme_light()

ggplot(land_use_class_datasets_cum[rel_dataset_agreement == 'average' & dataset %in% PREC_DATASETS_REANAL]) +
  geom_bar(aes(x = dataset, y = fraction_bias , fill = land_use_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'KG class')  +
  scale_fill_manual(values = colset_mid_qual) +
  theme_light()

ggplot(land_use_class_datasets_cum[rel_dataset_agreement == 'average' & dataset %in% PREC_DATASETS_REMOTE]) +
  geom_bar(aes(x = dataset, y = fraction_bias, fill = dataset), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  facet_wrap(~land_use_class, scales = 'free') +
  scale_fill_manual(values = colset_mid_qual) +
  labs(fill = 'KG class')  +
  theme_light()

