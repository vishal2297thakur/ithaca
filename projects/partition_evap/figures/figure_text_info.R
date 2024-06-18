# Support text with numbers ----

source("source/partition_evap.R")

## Table S2 and S3 ----
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
levels(evap_mask$rel_dataset_agreement) <- c("High", "Above average", "Average",
                                             "Below average", "Low")
levels(evap_mask$evap_quant_dataset_agreement) <- c("High", "Above average", "Average",
                                                    "Below average", "Low")


distribution <- as.data.table(readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_index_gridwise.rds")))
distribution <-  distribution[!is.na(index),]
distribution[, summary(index)]

### Distribution agreement thresholds at quantile 0.1, 0.3, 0.7. 0.9
quant_dist_0_1 <- quantile(distribution$index, c(0.1))
quant_dist_0_3 <- quantile(distribution$index, c(0.3))
quant_dist_0_7 <- quantile(distribution$index, c(0.7))
quant_dist_0_9 <- quantile(distribution$index, c(0.9))

### Relative dataset agreement thresholds at quantile 0.1, 0.3, 0.7. 0.9
quant_quart_0_1 <- quantile(evap_mask$std_quant_range, c(0.1))
quant_quart_0_3 <- quantile(evap_mask$std_quant_range, c(0.3))
quant_quart_0_7 <- quantile(evap_mask$std_quant_range, c(0.7))
quant_quart_0_9 <- quantile(evap_mask$std_quant_range, c(0.9))

## Interannual variance ----
evap_annual_vol <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "global_annual_means.rds"))
evap_annual_vol[dataset %in% EVAP_DATASETS_REANAL, dataset_type := "Reanalysis"]
evap_annual_vol[dataset %in% EVAP_DATASETS_REMOTE, dataset_type := "Remote"]
evap_annual_vol[dataset %in% EVAP_DATASETS_HYDROL, dataset_type := "Hydrological model"]
evap_annual_vol[dataset %in% EVAP_DATASETS_ENSEMB, dataset_type := "Ensemble"]
evap_annual_vol[, .(min(evap_volume), max(evap_volume)), dataset]
evap_annual_vol[, .(min(evap_volume), max(evap_volume)), dataset_type]


## Maps of dataset agreement ----

distribution[index <= quant_dist_0_1, agreement_fac := ordered(1, labels = "low")] 
distribution[index > quant_dist_0_1 & index <= quant_dist_0_3, agreement_fac := ordered(3, labels = "below average")]
distribution[index > quant_dist_0_3 & index <= quant_dist_0_7, agreement_fac := ordered(4, labels = "average")]
distribution[index > quant_dist_0_7 & index <= quant_dist_0_9, agreement_fac := ordered(5, labels = "above average")]
distribution[index > quant_dist_0_9, agreement_fac := ordered(7, labels = "high")]

levels(distribution$agreement_fac) <- c( "Low", "Below average", "Average",
                                            "Above average","High")

### % of high quartile and high distribution agreement 
evap_datasets_grid_mean <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets_grid_mean.rds"))

agreement <- merge(evap_mask[, .(lon, lat, rel_dataset_agreement, std_quant_range)], distribution, by = c("lon", "lat")) 
agreement <- agreement[evap_datasets_grid_mean[,.(lon, lat, area)], on = .(lon, lat)]
agreement <- unique(agreement)

agreement_area <- agreement[, .(agreement_area = sum(area)), .(rel_dataset_agreement, agreement_fac)]
agreement_area[, fraction := agreement_area/sum(agreement_area)]

agreement_area[rel_dataset_agreement %in% c("High", "Above average") & agreement_fac %in% c("High", "Above average"), sum(fraction)]
agreement_area[rel_dataset_agreement %in% c("Low", "Below average") & agreement_fac %in% c("Low", "Below average"), sum(fraction)]

agreement_area[rel_dataset_agreement %in% c("Low", "Below average") & agreement_fac %in% c("High", "Above average"), sum(fraction)]
agreement_area[rel_dataset_agreement %in% c("High", "Above average") & agreement_fac %in% c("Low", "Below average"), sum(fraction)]

agreement_area <- agreement_area[complete.cases(agreement_area)]
agreement_area <- agreement_area[!is.na(rel_dataset_agreement)]

agreement_area[, rel_dataset_agreement:= as.character(rel_dataset_agreement)]
agreement_area[, agreement_fac:= as.character(agreement_fac)]

library(ggsankeyfier)

es_long <-
  pivot_stages_longer(
    ## the data.frame we wish to pivot:
    data        = agreement_area ,
    ## the columns that represent the stages:
    stages_from = c("rel_dataset_agreement", "agreement_fac"),
    ## the column that represents the size of the flows:
    values_from = "fraction"
  )

es_long <- as.data.table(es_long)

es_long[, stage := factor(stage, levels = levels(es_long$stage), labels = c("Quartile Agreement", "Distribution Agreement"))]

ggplot(es_long,
       aes(x = stage, y = fraction, group = node,
           connector = connector, edge_id = edge_id, col = node, fill  = node)) +
  geom_sankeyedge(v_space = "auto") +
  geom_sankeynode(v_space = "auto") +
  labs(fill = "Agreement", col = "Agreement", y = "Area fraction [-]", x = "")+
  theme_bw()

## dataset agreement across environments ----

load(paste0(PATH_SAVE_PARTITION_EVAP, "partition_evap.Rdata"))
land_cover_agreement[rel_dataset_agreement == "Low", ][order(land_cover_fraction)]
land_cover_agreement[rel_dataset_agreement == "High" | rel_dataset_agreement == "Above average", sum(land_cover_fraction), land_cover_short_class][order(V1)]
biome_agreement[rel_dataset_agreement == "High" | rel_dataset_agreement == "Above average", sum(biome_fraction), biome_short_class][order(V1)]
IPCC_agreement[rel_dataset_agreement == "High" | rel_dataset_agreement == "Above average", sum(IPCC_ref_region_fraction), IPCC_ref_region][order(V1)]
elevation_agreement[rel_dataset_agreement == "High" | rel_dataset_agreement == "Above average", sum(elev_fraction), elev_class][order(V1)]

load(paste0(PATH_SAVE_PARTITION_EVAP, "distribution_agreement_masks.Rdata"))
land_cover_agreement[agreement_fac == "Low", ][order(land_cover_fraction)]

land_cover_agreement[agreement_fac == "High" | agreement_fac == "Above average", sum(land_cover_fraction), land_cover_short_class][order(V1)]
biome_agreement[agreement_fac == "High" | agreement_fac == "Above average", sum(biome_fraction), biome_short_class][order(V1)]
IPCC_agreement[agreement_fac == "High" | agreement_fac == "Above average", sum(IPCC_ref_region_fraction), IPCC_ref_region][order(V1)]
elevation_agreement[agreement_fac == "High" | agreement_fac == "Above average", sum(elev_fraction), elev_class][order(V1)]


biome[, mean(fraction), biome_short_class][order(-V1)]

## Extreme years

## Datset perspective ----

## Dataset performance and distribution testing

biome <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "area_fraction_matching_products_biome.rds"))
biome[, mean(fraction), .(dataset.x, dataset.y)][order(-V1)]
biome[, mean(fraction), dataset.x][order(-V1)]

## Interannual variance over environments by datasets