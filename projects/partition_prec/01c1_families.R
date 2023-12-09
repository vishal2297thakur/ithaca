# Creates network plots
library(readr)
library(igraph)

source('source/partition_prec.R')
source('source/graphics.R')

prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_datasets.rds"))
prec_datasets_mean <- prec_datasets[, .(prec = mean(prec, na.rm = TRUE)), .(dataset)]
family_tree <- read_csv(paste0(PATH_SAVE_PARTITION_PREC, 'dataset_families.csv'))[-1]

prec_datasets_mean <- prec_datasets[, .(prec = mean(prec, na.rm = TRUE)), .(dataset)]
prec_annual <- prec_datasets[, .(prec = mean(prec, na.rm = TRUE)), .(dataset, year)]

rownames(family_tree) <- colnames(family_tree)
families <- as.factor(c("chirps", "ncep", "cpc", "cpc", "cru", "era5", "era5", "cpc", "cpc", "merged", 
                        "jra55", "cpc", "merged", "ncep", "ncep", "cpc", "precl"))

## Families
family_network <- graph_from_adjacency_matrix(data.matrix(family_tree), 
                                    mode = 'directed', 
                                    diag = FALSE)
family_network <- set_vertex_attr(family_network, 'family', value = families)

network_cols <- colset_mid[c(4, 3, 5, 7, 9, 11, 6, 2)]
V(family_network)$color <- network_cols[as.numeric(V(family_network)$family)]

set.seed(1)
plot(family_network,
     edge.arrow.size = .2,
     vertex.label.color = "black",
     layout = layout.auto(family_network))

## Correlation
data_for_cor <- dcast(prec_annual, year ~ dataset, value.var = 'prec')
cor_matrix <- cor(data_for_cor[, -1], use = 'pairwise.complete.obs') + 1
cor_matrix[cor_matrix < quantile(cor_matrix, 0.8)] <- 0

cor_network <- graph_from_adjacency_matrix(cor_matrix, 
                                    mode = 'undirected', 
                                    diag = FALSE)
cor_network <- set_vertex_attr(cor_network, 'family', value = families)
V(cor_network)$color <- network_cols[as.numeric(V(cor_network)$family)]

set.seed(1)
plot(cor_network,
     edge.arrow.size = .2,
     vertex.label.color = "black",
     layout = layout.auto(cor_network))

## Distance
abs_distance <- abs(dist(prec_datasets_mean[!dataset %in% c('cmorph', 'persiann', 'chirps')]$prec))
names(abs_distance) <- prec_datasets_mean[!dataset %in% c('cmorph', 'persiann', 'chirps')]$dataset
abs_distance <- 1 + 1 / abs_distance
abs_distance[abs_distance < quantile(abs_distance, 0.7)] <- 0 

distance_network <- graph_from_adjacency_matrix(data.matrix(abs_distance), 
                                   mode = 'undirected', 
                                   diag = FALSE)

families <- as.factor(c("ncep", "cpc", "cru", "era5", "era5", "cpc", "cpc", "merged", "jra55", "cpc", "merged", "ncep", "ncep", "precl"))
distance_network <- set_vertex_attr(distance_network, 'family', value = families)
network_cols <- colset_mid[c(3, 5, 7, 9, 11, 6, 2, 4)]
V(distance_network)$color <- network_cols[as.numeric(V(distance_network)$family)]
E(distance_network)$curved <- FALSE
set.seed(1)
plot(distance_network,
     edge.arrow.size = .2,
     vertex.label.color = "black",
     layout = layout.auto(distance_network))

## Combination
cor_matrix_global <- cor_matrix[!rownames(cor_matrix) %in% c("chirps", "cmorph", "persiann"), ] 
cor_matrix_global <- cor_matrix_global[, !colnames(cor_matrix_global) %in% c("chirps", "cmorph", "persiann")] 
family_tree_global <- family_tree[!rownames(family_tree) %in% c("chirps", "cmorph", "persiann"), ] 
family_tree_global <- family_tree_global[, !colnames(family_tree_global) %in% c("chirps", "cmorph", "persiann")] 

dataset_agreement <- as.matrix(abs_distance) * cor_matrix_global 
dataset_agreement_network <- graph_from_adjacency_matrix(dataset_agreement, weighted = TRUE, 
                                                         mode = 'undirected', 
                                                         diag = FALSE)

families <- as.factor(c("ncep", "cpc", "cru", "era5", "era5", "cpc", "cpc", "merged", "jra55", "cpc", "merged", "ncep", "ncep", "precl"))
dataset_agreement_network <- set_vertex_attr(dataset_agreement_network, 'family', value = families)
network_cols <- colset_mid[c(3, 5, 7, 9, 11, 6, 2, 4)]
V(dataset_agreement_network)$color <- network_cols[as.numeric(V(dataset_agreement_network)$family)]

plot(dataset_agreement_network,
     edge.arrow.size = .2,
     vertex.label.color = "black",
     layout = layout.auto(distance_network))

dataset_agreement <- as.matrix(abs_distance) * cor_matrix_global * data.matrix(family_tree_global)
dataset_agreement[dataset_agreement < 0] <- 0 
dataset_agreement_network <- graph_from_adjacency_matrix(dataset_agreement, weighted = TRUE, 
                                                mode = 'directed', 
                                                diag = FALSE)

families <- as.factor(c("ncep", "cpc", "cru", "era5", "era5", "cpc", "cpc", "merged", "jra55", "cpc", "merged", "ncep", "ncep", "precl"))
dataset_agreement_network <- set_vertex_attr(dataset_agreement_network, 'family', value = families)
network_cols <- colset_mid[c(3, 5, 7, 9, 11, 6, 2, 4)]
V(dataset_agreement_network)$color <- network_cols[as.numeric(V(dataset_agreement_network)$family)]

plot(dataset_agreement_network,
     edge.arrow.size = .2,
     vertex.label.color = "black",
     layout = layout.auto(distance_network))
