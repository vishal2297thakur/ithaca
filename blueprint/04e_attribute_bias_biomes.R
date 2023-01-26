source('source/blueprint.R')
source('source/graphics.R')

## Read data 
prec_stats <- readRDS(paste0(path_save_blueprint, "prec_ensemble_stats.rds"))
prec_mask_biomes <- readRDS(paste0(path_save_blueprint, "prec_masks_biomes.rds"))

## Variables
needed_for_cumsum <- expand.grid(prec_mask_biomes[, unique(rel_dataset_agreement)], prec_mask_biomes[, unique(biome_class)])
colnames(needed_for_cumsum) <- c("rel_dataset_agreement", 'biome_class')

## Mask the data *** DO NOT RUN ***
fname_shape <- list.files(path = masks_dir_ecoregions, full.names = TRUE, pattern = "mask_biomes_dinerstein.shp")
shape_mask <- st_read(paste0(fname_shape[1]))
shape_mask <- st_make_valid(shape_mask)

shape_mask_raster <- rasterize(shape_mask, prec_era5_kenya[[1]]) #directly rasterized; no cropping
shape_mask_df <- shape_mask_raster %>% as.data.frame(xy = TRUE, long = TRUE, na.rm = TRUE)
shape_mask_df <- subset(shape_mask_df, select = c('x', 'y', 'layer_BIOME_NAME'))
colnames(shape_mask_df) <- c('lon', 'lat', 'biome_class')
shape_mask_df$biome_class <- factor(shape_mask_df$biome_class)

prec_mask_biomes <- merge(prec_stats[, .(lat, lon, prec_mean = ens_mean_mean), rel_dataset_agreement], shape_mask_df, by = c('lon', 'lat'))

## Analysis
biome_class <- prec_mask_biomes[, .(sum_biome_class = sum(prec_mean)), 
                      .(biome_class, rel_dataset_agreement)]
biome_class <-  merge(biome_class, needed_for_cumsum, by = c('biome_class', "rel_dataset_agreement"), all.y = TRUE)
biome_class <- biome_class[order(biome_class, rel_dataset_agreement), ]
biome_class[is.na(sum_biome_class), sum_biome_class := 0]

biome_class_cum <- biome_class[, .(cumsum_biome_class = cumsum(sum_biome_class), 
                             rel_dataset_agreement), .(biome_class)]
biome_class_cum[, fraction_bias := cumsum_biome_class  / sum(cumsum_biome_class), rel_dataset_agreement]

# Save for further use 
saveRDS(prec_mask_biomes, paste0(path_save_blueprint, "prec_masks_biomes.rds"))

## Quick validation
ggplot() +
  geom_raster(data = prec_mask_biomes, aes(lon, lat, fill = biome_class)) +
  geom_point(data = prec_mask_biomes[rel_dataset_agreement == 'high', .(lon, lat)],  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid_qual[3:7]) +
  coord_sf(default_crs = sf::st_crs(3395)) + #Mercator
  labs(fill = 'Precipitation')  +
  theme_light()

ggplot(prec_mask_biomes) +
  geom_bar(aes(x = biome_class)) + 
  theme_light()

## Plot
# Main: Partition (%) of total precipitation per climatology for different levels of dataset agreement 
ggplot(biome_class_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = biome_class), stat = "identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'KG class')  +
  scale_fill_manual(values = colset_mid_qual[2:6]) +
  theme_light()

ggplot(prec_mask_biomes) +
  geom_boxplot(aes(y = prec_mean, fill = rel_dataset_agreement)) +
  facet_wrap(~biome_class, scales = 'free') +
  labs(fill = 'Dataset agreement')  +
  ylab('Mean monthly precipitation')  +
  scale_fill_manual(values = colset_mid_qual[c(1, 4, 3, 2, 5)]) +
  theme_light() +
  theme(strip.background = element_rect(fill = "black"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
