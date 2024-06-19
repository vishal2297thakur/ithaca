# No global dataset so not applicable for evaporation


# Plots the datasets that are closests/furthest to the ensemble mean

source('source/partition_evap.R')
source('source/graphics.R')
source('source/geo_functions.R')

library(ggrepel)
library(tidyverse)

#Data
datasets_vol <- data.table(read.csv(paste0(PATH_SAVE_PARTITION_EVAP_TABLES, 
                                           "partition_IPCC_class_datasets_global.csv")))[, 2:47]

## Variables
datasets_vol_matrix <- t(as.matrix(datasets_vol[, 2:46])) 
colnames(datasets_vol_matrix) <- datasets_vol$Dataset
datasets_vol <- melt(datasets_vol)
colnames(datasets_vol) <- c('dataset', 'IPCC_ref_region', 'evap')

IPCC_means <- datasets_vol[, .(evap_mean = mean(evap)), IPCC_ref_region]
datasets_mean_ratio <- datasets_vol_matrix / IPCC_means$evap_mean

datasets_mean_ratio <- as_tibble(datasets_mean_ratio) %>%
  mutate(IPCC_ref_region = factor(rownames(datasets_mean_ratio)))  %>%
  relocate(IPCC_ref_region)

datasets_mean_ratio <- datasets_mean_ratio %>% pivot_longer(-IPCC_ref_region,
                                                            names_to="dataset",
                                                            values_to="evap") 

datasets_mean_ratio <- data.table(datasets_mean_ratio)

datasets_mean_ratio[, evap_diff := evap - 1]
datasets_mean_ratio[, abs_evap_diff := abs(evap_diff)]

## Analysis1
datasets_mean_ratio <- datasets_mean_ratio[order(-rank(IPCC_ref_region), abs_evap_diff)]
dummy_1 <- datasets_mean_ratio[datasets_mean_ratio[, .I[dataset == head(dataset, 2)], by = IPCC_ref_region]$V1]
dummy_1[, class := factor("mean")]
datasets_mean_ratio <- datasets_mean_ratio[order(-rank(IPCC_ref_region), evap_diff)]
dummy_2 <- datasets_mean_ratio[datasets_mean_ratio[, .I[dataset == head(dataset, 3)], by = IPCC_ref_region]$V1]
dummy_2[, class := factor("underestimate")]
datasets_mean_ratio <- datasets_mean_ratio[order(-rank(IPCC_ref_region), -evap_diff)]
dummy_3 <- datasets_mean_ratio[datasets_mean_ratio[, .I[dataset == head(dataset, 3)], by = IPCC_ref_region]$V1]
dummy_3[, class := factor("overestimate")]

## Figures1
to_plot <- rbind(dummy_1, dummy_2, dummy_3)[, c(1, 2, 3, 6)]

ggplot(to_plot, aes(IPCC_ref_region, evap)) +
  geom_hline(yintercept = 1, col = 'grey50') +
  geom_point() +
  geom_line(aes(group = IPCC_ref_region)) +
  scale_fill_manual(values = colset_RdBu_5[c(3, 4, 2)]) + 
  scale_color_manual(values = c('white', 'black', 'white')) + 
  geom_label_repel(aes(label = dataset, fill = class),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  scale_x_discrete(name = "IPCC_ref_region") +
  scale_y_continuous(name = "Ratio to the ensemble mean") +
  guides(fill = "none") +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12)) 

#ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "dataset_performance_IPCC_ref_region_ver1.png"), width = 15, height = 7)


#######################################################################
## Analysis2 and Figure2 
# Note: Run this part of the code for one IPCC_region at a time
#IPCC_region<- c("ARP", "CAF", "ESAF", "MDG", "NEAF", "SAH", "SEAF", "WAF", "WSAF") #Africa
#IPCC_region <-   c("EAS", "ECA", "ESB",  "RFE", "RAR",  "SAS", "SEA",  "TIB", "WCA", "WSB") #Asis
#IPCC_region <- c("CAU", "EAU", "NAU", "NZ", "PAC", "SAU") #Australasia 
#IPCC_region <- IPCC_region[-5] #Australasia
#IPCC_region <- c("EEU", "GIC","MED", "NEU", "WCE") # Europe
#IPCC_region <- c("CAR", "CNA", "ENA", "NCA","NEN", "NWN", "SCA", "WNA") #Namerica
IPCC_region <- c("NES","NSA","NWS","SAM","SES", "SSA","SWS") #Samerica

dummy_mat_subset <- datasets_mean_ratio[datasets_mean_ratio$IPCC_ref_region %in% IPCC_region,]

plotlist = list()

for (i in 1:(length(unique(dummy_mat_subset$IPCC_ref_region)))){
  datasets_mean_ratio_c1 <- dummy_mat_subset[dummy_mat_subset$IPCC_ref_region %in% 
                                               IPCC_region[i],]
  datasets_mean_ratio_c1$class <- NA
  dummy1 <- datasets_mean_ratio_c1[order(datasets_mean_ratio_c1$abs_evap_diff,decreasing = FALSE),]
  dummy1$class[1:2] <- "mean"
  dummy1$class[which(dummy1$evap_diff<0 & is.na(dummy1$class))] <- "underestimated"
  dummy1$class[which(is.na(dummy1$class))] <- "overestimated"
  
  dummy1$colour <- NA
  dummy1$colour[which(dummy1$class =="mean")] <- "firebrick"
  dummy1$colour[which(is.na(dummy1$colour))] <- "black"
    
  dummy1 <- dummy1[order(dummy1$dataset),]
  
  plotlist[[i]] <- ggplot(dummy1,aes(x = dataset, y =evap_diff, fill = class)) + 
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("firebrick","dodgerblue4","forestgreen"))+
    coord_flip()+ylab("Deviation from ensemble mean")+
    ylim(-0.8, 1)+
    ggtitle(unique(dummy_mat_subset$IPCC_ref_region)[i])+
    theme_bw()+
    theme(axis.title.y=element_blank(),axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          plot.title=element_text(size=11,hjust=0.5),
          axis.text.y = element_text(colour = dummy1$colour),
          legend.position = "none",
          plot.margin = margin(0,0.5,0,0.5, "cm"))
}

library(grid)
ggarrange(plotlist[[1]], plotlist[[2]],plotlist[[3]],
          plotlist[[4]],plotlist[[5]],plotlist[[6]],
          plotlist[[7]],
          nrow = 4, ncol = 2,labels = c("a","b","c","d","e","f", "g"))

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "dataset_performance_IPCC_Samerica.png"),
       width = 7, height = 9.6)

