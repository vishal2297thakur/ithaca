source('source/blueprint.R')
source('source/graphics.R')

## Read data 
prec_mask <- readRDS(paste0(path_save_blueprint, "prec_masks.rds"))
prec_mask_month <- readRDS(paste0(path_save_blueprint, "prec_masks_month.rds"))

## Variables
needed_for_cumsum <- expand.grid(prec_mask[, unique(rel_dataset_agreement)], prec_mask[, unique(KG_class)])
colnames(needed_for_cumsum) <- c("rel_dataset_agreement", 'KG_class')

## Analysis
KG_class <- prec_mask[, .(sum_KG_class = sum(prec_mean)), 
                        .(KG_class, rel_dataset_agreement)]
KG_class <-  merge(KG_class, needed_for_cumsum, by = c('KG_class', "rel_dataset_agreement"), all.y = TRUE)
KG_class <- KG_class[order(KG_class, rel_dataset_agreement), ]

KG_class_cum <- KG_class[, .(cumsum_KG_class = cumsum(sum_KG_class), 
                                 rel_dataset_agreement), .(KG_class)]
KG_class_cum[, fraction_bias := cumsum_KG_class  / sum(cumsum_KG_class), rel_dataset_agreement]

## Quick validation
ggplot() +
  geom_raster(data = prec_mask, aes(lon, lat, fill = KG_class)) +
  geom_point(data = prec_mask[rel_dataset_agreement == 'high', .(lon, lat)],  aes(lon, lat)) +
  scale_fill_manual(values = colset_mid_qual[3:5]) +
  coord_sf(default_crs = sf::st_crs(3395)) + #Mercator
  labs(fill = 'Precipitation')  +
  theme_light()

ggplot(prec_mask) +
  geom_bar(aes(x = KG_class)) + 
  theme_light()

## Plot
# Main: Partition (%) of total precipitation per climatology for different levels of dataset agreement 
ggplot(KG_class_cum) +
  geom_bar(aes(x = rel_dataset_agreement, y = fraction_bias , fill = KG_class), stat="identity") +
  xlab('Cumulative dataset agreement')  +
  ylab('Precipitation fraction')  +
  labs(fill = 'KG class')  +
  scale_fill_manual(values = colset_mid_qual[3:5]) +
  theme_light()


