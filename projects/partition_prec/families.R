install.packages('splitstackshape')

library(igraph)
require(splitstackshape)

prec_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_PREC, "prec_datasets.rds"))
prec_datasets_mean <- prec_datasets[, .(prec = mean(prec, na.rm = TRUE)), .(dataset)]

family_tree <- read_csv('../../shared/data_projects/ithaca/partition_prec/dataset_families.csv')[-1]
dataset_meta <- data.table(name = names(family_tree))
rownames(family_tree) <- colnames(family_tree)

#Families
family_network <- graph_from_adjacency_matrix(data.matrix(family_tree), 
                                    mode = 'directed', 
                                    diag = FALSE)
families <- as.factor(c("chirps", "ncep", "cpc", "cpc", "cru", "era5", "era5", "cpc", "cpc", "merged", 
                        "jra55", "cpc", "merged", "ncep", "ncep", "cpc", "precl"))
dataset_meta$family <- families
family_network <- set_vertex_attr(family_network, 'family', value = families)
colrs <- c("gray50", "tomato", "gold", "darkred", "steelblue", "lightgreen", "darkblue", "brown")
V(family_network)$color <- colrs[as.numeric(V(family_network)$family)]

set.seed(1)
plot(family_network,
     edge.arrow.size = .2,
     layout = layout.auto(family_network))


edge <- as_ids(E(family_network))
edge <- as.data.frame(edge)
split <- as.data.table(cSplit(edge, "edge", "|"))
split[, ':=' (dummy=1,
            key = paste(edge_1, edge_2, sep = "_"))]
split <- unique(split[, .(edge_1,edge_2,sum(dummy)), by = key])[, 2:4, with = FALSE][, 1:2]
split[, .N, edge_1]
split[, .N, edge_2]

#Correlation
cor_matrix <- cor(data_for_cor_temporal[, -1], use = 'pairwise.complete.obs') + 1
cor_matrix[cor_matrix < quantile(cor_matrix, 0.8)] <- 0

cor_network <- graph_from_adjacency_matrix(cor_matrix, 
                                    mode = 'undirected', 
                                    diag = FALSE)
cor_network <- set_vertex_attr(cor_network, 'family', value = families)
colrs <- c("gray50", "tomato", "gold", "darkred", "steelblue", "lightgreen", "darkblue", "brown")
V(cor_network)$color <- colrs[as.numeric(V(cor_network)$family)]

set.seed(1)
plot(cor_network,
     edge.arrow.size = .2,
     layout = layout.auto(cor_network))

optimal_lg = cluster_leading_eigen(cor_network)
set.seed(1)
plot(optimal_lg, cor_network)

optimal_lg = cluster_fluid_communities(cor_network)
plot(optimal_lg, cor_network)


edge <- as_ids(E(cor_network))
edge <- as.data.frame(edge)
split <- as.data.table(cSplit(edge, "edge", "|"))
split[, ':=' (dummy=1,
              key = paste(edge_1, edge_2, sep = "_"))]
split <- unique(split[, .(edge_1,edge_2,sum(dummy)), by = key])[, 2:4, with = FALSE][, 1:2]
split[, .N, edge_1]
split[, .N, edge_2]


# Distance
abs_distance <- abs(dist(prec_datasets_mean[!dataset %in% c('cmorph', 'persiann', 'chirps')]$prec))
names(abs_distance) <- prec_datasets_mean[!dataset %in% c('cmorph', 'persiann', 'chirps')]$dataset
abs_distance[abs_distance > quantile(abs_distance, 0.3)] <- 0 
#abs_distance[abs_distance > quantile(abs_distance, 0.2)] <- 0 

distance_network <- graph_from_adjacency_matrix(data.matrix(1 + 1/abs_distance), 
                                   mode = 'undirected', 
                                   diag = FALSE)

families <- as.factor(c("ncep", "cpc", "cru", "era5", "era5", "cpc", "cpc", "merged", "jra55", "cpc", "merged", "ncep", "ncep", "precl"))
distance_network <- set_vertex_attr(distance_network, 'family', value = families)
colrs <- c("tomato", "gold", "darkred",   "steelblue", "lightgreen", "darkblue", "brown")
V(distance_network)$color <- colrs[as.numeric(V(distance_network)$family)]

set.seed(1)
plot(distance_network,
     edge.arrow.size = .2,
     layout = layout.auto(distance_network))

edge <- as_ids(E(distance_network))
edge <- as.data.frame(edge)
split <- as.data.table(cSplit(edge, "edge", "|"))
split[, ':=' (dummy=1,
              key = paste(edge_1, edge_2, sep = "_"))]
split <- unique(split[, .(edge_1,edge_2,sum(dummy)), by = key])[, 2:4, with = FALSE][, 1:2]
split[, .N, edge_1]
split[, .N, edge_2]

# Combination
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
colrs <- c("tomato", "gold", "darkred",   "steelblue", "lightgreen", "darkblue", "brown")
V(dataset_agreement_network)$color <- colrs[as.numeric(V(dataset_agreement_network)$family)]

plot(dataset_agreement_network,
     edge.arrow.size = .2,
     layout = layout.auto(distance_network))

dataset_agreement <- as.matrix(abs_distance) * cor_matrix_global * data.matrix(family_tree_global)
dataset_agreement[dataset_agreement < 0] <- 0 
dataset_agreement_network <- graph_from_adjacency_matrix(dataset_agreement, weighted = TRUE, 
                                                mode = 'directed', 
                                                diag = FALSE)

families <- as.factor(c("ncep", "cpc", "cru", "era5", "era5", "cpc", "cpc", "merged", "jra55", "cpc", "merged", "ncep", "ncep", "precl"))
dataset_agreement_network <- set_vertex_attr(dataset_agreement_network, 'family', value = families)
colrs <- c("tomato", "gold", "darkred",   "steelblue", "lightgreen", "darkblue", "brown")
V(dataset_agreement_network)$color <- colrs[as.numeric(V(dataset_agreement_network)$family)]

plot(dataset_agreement_network,
     edge.arrow.size = .2,
     layout = layout.auto(distance_network))



shordistance_network_paths(distance_network, from = "ncep-doe",to = "cmap", weights = E(distance_network)$Distance)$vpath[[1]]
get_diameter(distance_network, weights = E(distance_network)$Distance)

optimal_lg = cluster_leading_eigen(distance_network)
set.seed(1)
plot(optimal_lg, distance_network)

dendrogram <- cluster_edge_betweenness(distance_network)
plot_dendrogram(dendrogram)

set_vertex_attr(distance_network, 'family', value = rep("A", 147))
get_vertex_att
V(distance_network)[Faction == 1]$shape <- "circle"
V(distance_network)[Faction == 2]$shape <- "square"
set.seed(1)
plot(dendrogram,karate)