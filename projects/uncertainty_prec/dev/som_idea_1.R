install.packages('kohonen')
library(kohonen)

source('source/partition_prec.R')

prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_datasets.rds"))
simple_stats_for_som <- prec_datasets[ , .(mean = mean(prec), sd = sd(prec)), .(lon, lat, dataset)]

simple_stats_for_som_short <- simple_stats_for_som[sample(1:nrow(simple_stats_for_som), 17000), ]

sample_size <- nrow(simple_stats_for_som_short)
grid_size <- ceiling(sample_size ^ (1/2.5))
som_grid <- somgrid(xdim = grid_size, ydim = grid_size, topo = 'hexagonal', toroidal = T)

data_for_som <- cbind(model.matrix(~ mean - 1, simple_stats_for_som_short),
                      model.matrix(~ sd - 1, simple_stats_for_som_short))

simple_stats_short_som <- som(data_for_som, grid = som_grid) 

superclust_pam <- cluster::pam(simple_stats_short_som$codes[[1]], 6)
superclasses_pam <- superclust_pam$clustering

superclust_hclust <- hclust(dist(simple_stats_short_som$codes[[1]]), "complete")
superclasses_hclust <- cutree(superclust_hclust, 10)

aweSOMplot(som = simple_stats_short_som, type = "Circular", data = data_for_som, 
           superclass = superclasses_pam, 
           values = "median")

aweSOMplot(som = masks_low_agreement_som, type = "Hitmap", superclass = superclasses_hclust)
aweSOMplot(som = masks_low_agreement_som, type = "UMatrix", superclass = superclasses_pam)