# Investigate the reasons of low dataset agreement
install.packages('UpSetR')
install.packages('randomForestSRC')
install.packages('kohonen')
install.packages('ggRandomForests')
install.packages('caret')

library(kohonen)
library(aweSOM)
library(randomForestSRC)
library(ggRandomForests)
library(UpSetR)

source('source/partition_prec.R')
source('source/geo_functions.R')
source('source/graphics.R')

# Data
prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_datasets.rds"))
prec_mask <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_masks.rds"))
ens_stats <- readRDS(paste0(PATH_SAVE_PARTITION_PREC,  "prec_ensemble_stats.rds"))

# Variables
masks_low_agreement <- prec_mask[rel_dataset_agreement == 'low']

datasets_low_agreement <- merge(masks_low_agreement[, .(lon, lat, prec_quant, KG_class_1, 
                                                        elev_class, land_cover_class, biome_class)], prec_datasets, by = c('lon', 'lat'))
datasets_low_agreement <- merge(ens_stats[, .(lon, lat, ens_mean_mean)], datasets_low_agreement, by = c('lon', 'lat'))

datasets_low_agreement[, mean_distance := round(ens_mean_mean - prec_mean, 0)]
datasets_low_agreement[ens_mean_mean > 0, rel_mean_distance := round((ens_mean_mean - prec_mean)/ens_mean_mean, 2)]

## Self-Organizing Map
sample_size <- nrow(masks_low_agreement)
grid_size <- ceiling(sample_size ^ (1/2.5))
som_grid <- somgrid(xdim = grid_size, ydim = grid_size, topo = 'hexagonal', toroidal = T)
data_for_som <- masks_low_agreement[, .(KG_class = KG_class_1, 
                                        elevation = as.character(elev_class), 
                                        land_use = land_use_short_class,
                                        biome = biome_short_class)]
data_for_som <- cbind(model.matrix(~ elevation - 1, data_for_som),
                      model.matrix(~ land_use - 1, data_for_som),
                      model.matrix(~ biome - 1, data_for_som),
                      model.matrix(~ KG_class - 1, data_for_som))

masks_low_agreement_som <- som(data_for_som, grid = somgrid(6, 6, "hexagonal")) 

superclust_pam <- cluster::pam(masks_low_agreement_som$codes[[1]], 6)
superclasses_pam <- superclust_pam$clustering

superclust_hclust <- hclust(dist(masks_low_agreement_som$codes[[1]]), "complete")
superclasses_hclust <- cutree(superclust_hclust, 10)

aweSOMplot(som = masks_low_agreement_som, type = "Circular", data = data_for_som, 
           superclass = superclasses_pam, 
           values = "median")

aweSOMplot(som = masks_low_agreement_som, type = "Hitmap", superclass = superclasses_hclust)
aweSOMplot(som = masks_low_agreement_som, type = "UMatrix", superclass = superclasses_pam)



# UpSet
upset(data.table(data_for_som), nsets = 24, nintersects = 40, point.size = 4, 
      main.bar.color = "lightskyblue3", sets.bar.color = "SteelBlue", 
      matrix.dot.alpha = 0, mainbar.y.label = "Grid cells",
      sets.x.label = "", order.by = "freq")



# Random Forest
data_for_rf <- prec_mask[, .(KG_class = KG_class_1, 
                             elevation = as.character(elev_class), 
                             land_use = land_use_short_class)]
data_for_rf <- model.matrix(~ KG_class + elevation + land_use - 1, data.frame(data_for_rf))
data_for_rf <- data.frame(cbind(rel_dataset_agreement = prec_mask$rel_dataset_agreement, data_for_rf))
dataset_agreement_rf <- rfsrc(rel_dataset_agreement ~ ., data = data_for_rf,
                              tree.err = TRUE, importance = T, ntree = 2000)

gg_vimp_plot <- gg_vimp(dataset_agreement_rf)
plot(gg_vimp_plot)
gg_md <- gg_minimal_depth(dataset_agreement_rf)
plot(gg_md, lbls = 1:10)
ggRandomForests:::plot.gg_minimal_vimp(gg_md)