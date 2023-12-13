# Creates network plots
install.packages("ggnetwork")

library(readr)
library(igraph)
library(ggnetwork)

source('source/partition_prec.R')
source('source/graphics.R')

prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_datasets.rds"))
prec_datasets_mean <- prec_datasets[, .(prec = mean(prec, na.rm = TRUE)), .(dataset)]
family_tree <- read_csv(paste0(PATH_SAVE_PARTITION_PREC, 'dataset_families.csv'))[-1]
colnames(family_tree)[c(7, 10, 14, 15)] <- c('era5', 'gpm', 'doe', 'ncar')

prec_datasets_mean <- prec_datasets[, .(prec = mean(prec, na.rm = TRUE)), .(dataset)]
prec_datasets_mean[prec_datasets_mean == 'ncep-doe'] <- 'doe'
prec_datasets_mean[prec_datasets_mean == 'ncep-ncar'] <- 'ncar'
prec_datasets_mean[prec_datasets_mean == 'era5-land'] <- 'era5'
prec_datasets_mean[prec_datasets_mean == 'gpm-imerg'] <- 'gpm'

prec_annual <- prec_datasets[, .(prec = mean(prec, na.rm = TRUE)), .(dataset, year)]
prec_annual[prec_annual == 'ncep-doe'] <- 'doe'
prec_annual[prec_annual == 'ncep-ncar'] <- 'ncar'
prec_annual[prec_annual == 'era5-land'] <- 'era5'
prec_annual[prec_annual == 'gpm-imerg'] <- 'gpm'

rownames(family_tree) <- colnames(family_tree)
family <- as.factor(c("chirps", "ncep", "cpc", "cpc", "cru", "era5", "era5", "cpc", "cpc", "merged", 
                        "jra55", "cpc", "merged", "ncep", "ncep", "cpc", "precl"))
dataset_families <- data.table(name = colnames(family_tree), family = family)
network_cols <- colset_mid[c(4, 3, 5, 7, 9, 11, 6, 2)]

## Families
family_network <- graph_from_adjacency_matrix(data.matrix(family_tree), 
                                    mode = 'directed', 
                                    diag = FALSE)
family_network_dt <- merge(ggnetwork(family_network), dataset_families, by = 'name')
family_network_dt[family_network_dt == 'ncep-doe'] <- 'doe'
family_network_dt[family_network_dt == 'ncep-ncar'] <- 'ncar'
family_network_dt[family_network_dt == 'era5-land'] <- 'era5'
family_network_dt[family_network_dt == 'gpm-imerg'] <- 'gpm'

set.seed(1) 
fig_families <- ggplot(family_network_dt, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(6, "pt"), type = "closed"), col = "grey30") +
  scale_fill_manual(values = network_cols) +
  scale_color_manual(values = network_cols) +
  geom_nodes(aes(col = family), size = 6) +
  geom_nodes(aes(col = family), size = 8, alpha = 0.4) +
  geom_nodetext(aes(label = name)) +
  theme_blank() +
  theme(panel.border = element_rect(fill = NA, size = 0.3)) +
  guides(fill = "none", col = 'none') 

## Correlation
data_for_cor <- dcast(prec_annual, year ~ dataset, value.var = 'prec')
data_for_cor <- data_for_cor[, c(1:6, 8:15, 7, 16:18)]
cor_matrix <- cor(data_for_cor[, -1], use = 'pairwise.complete.obs') + 1
cor_matrix[cor_matrix < quantile(cor_matrix, 0.8)] <- 0

cor_network <- graph_from_adjacency_matrix(cor_matrix, 
                                    mode = 'undirected', 
                                    diag = FALSE)
cor_network_dt <- merge(ggnetwork(cor_network), dataset_families, by = 'name')
set.seed(1) 
fig_correlation <- ggplot(cor_network_dt, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(6, "pt"), type = "closed"), col = "grey30") +
  scale_fill_manual(values = network_cols) +
  scale_color_manual(values = network_cols) +
  geom_nodes(aes(col = family), size = 6) +
  geom_nodes(aes(col = family), size = 8, alpha = 0.4) +
  geom_nodetext(aes(label = name)) +
  theme_blank() +
  theme(panel.border = element_rect(fill = NA, size = 0.3)) +
  guides(fill = "none", col = 'none') 

## Distance
abs_distance <- abs(dist(prec_datasets_mean[!dataset %in% c('cmorph', 'persiann', 'chirps')]$prec))
names(abs_distance) <- prec_datasets_mean[!dataset %in% c('cmorph', 'persiann', 'chirps')]$dataset
abs_distance <- 1 + 1 / abs_distance
abs_distance[abs_distance < quantile(abs_distance, 0.7)] <- 0 

distance_network <- graph_from_adjacency_matrix(data.matrix(abs_distance), 
                                   mode = 'undirected', 
                                   diag = FALSE)
distance_network_dt <- merge(ggnetwork(distance_network), dataset_families, by = 'name')
network_cols <- colset_mid[c(3, 5, 7, 9, 11, 6, 2, 4)]
set.seed(1) 
fig_distance <- ggplot(distance_network_dt, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(6, "pt"), type = "closed"), col = "grey30") +
  scale_fill_manual(values = network_cols) +
  scale_color_manual(values = network_cols) +
  geom_nodes(aes(col = family), size = 6) +
  geom_nodes(aes(col = family), size = 8, alpha = 0.4) +
  geom_nodetext(aes(label = name)) +
  theme_blank() +
  theme(panel.border = element_rect(fill = NA, size = 0.3)) +
  guides(fill = "none", col = 'none') 

## Combination
cor_matrix_global <- cor_matrix[!rownames(cor_matrix) %in% c("chirps", "cmorph", "persiann"), ] 
cor_matrix_global <- cor_matrix_global[, !colnames(cor_matrix_global) %in% c("chirps", "cmorph", "persiann")] 
family_tree_global <- family_tree[!rownames(family_tree) %in% c("chirps", "cmorph", "persiann"), ] 
family_tree_global <- family_tree_global[, !colnames(family_tree_global) %in% c("chirps", "cmorph", "persiann")] 

dataset_agreement <- as.matrix(abs_distance) * cor_matrix_global * data.matrix(family_tree_global)
dataset_agreement[dataset_agreement < 0] <- 0 
dataset_agreement_network <- graph_from_adjacency_matrix(dataset_agreement, weighted = TRUE, 
                                                mode = 'directed', 
                                                diag = FALSE)
dataset_agreement_network_dt <- merge(ggnetwork(dataset_agreement_network, layout = layout.auto(distance_network)), 
                                      dataset_families, by = 'name')

set.seed(1) 
fig_combination <- ggplot(dataset_agreement_network_dt, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(6, "pt"), type = "closed"), col = "grey30") +
  scale_fill_manual(values = network_cols) +
  scale_color_manual(values = network_cols) +
  geom_nodes(aes(col = family), size = 6) +
  geom_nodes(aes(col = family), size = 8, alpha = 0.4) +
  geom_nodetext(aes(label = name)) +
  theme_blank() +
  theme(panel.border = element_rect(fill = NA, size = 0.3)) +
  guides(fill = "none", col = 'none') 

ggarrange(fig_families, fig_correlation, fig_distance, fig_combination, 
          ncol = 2, nrow = 2, labels = c('a', 'b', 'c', 'd'), hjust = -2.1, vjust = 2.5)

ggsave(paste0(PATH_SAVE_PARTITION_PREC_FIGURES, "dataset_relationships.png"), width = 12, height = 12)
