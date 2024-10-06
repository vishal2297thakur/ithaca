source('source/partition_evap.R')
source('source/geo_functions.R')

evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))
evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
unique_landclass <- unique(evap_mask$KG_class_2)
unique_landclass <- na.omit(unique_landclass)

for (j in 1:length(EVAP_GLOBAL_DATASETS)){
  for (k in 2000:2019){
    
    dummy_mat2 <- evap_datasets[evap_datasets$dataset %in% EVAP_GLOBAL_DATASETS[j] &
                                  evap_datasets$year %in% k,]
    
    for (i in 1:length(unique_landclass)){
      dummy_mat1 <- evap_mask[evap_mask$KG_class_2 %in% unique_landclass[i],]
      dummy_mat1_subset <- data.frame(lon=dummy_mat1$lon,lat=dummy_mat1$lat)
      
      mean_val <- mean(merge(dummy_mat2,dummy_mat1_subset)$evap)
      
      if (j==1 && k==2000 && i==1){
        dummy_mat <- data.frame(year=k,dataset=EVAP_GLOBAL_DATASETS[j],
                                land_cover_class=unique_landclass[i],mean_evp=mean_val)
      }else{
        dummy_mat <- dummy_mat %>% 
          add_row(year=k,dataset=EVAP_GLOBAL_DATASETS[j],
                  land_cover_class=unique_landclass[i],mean_evp=mean_val)
      }
    }
  }
}


####################################################################################
dummy_agreement <- array(NA, dim=c(n_datasets_2000_2019,n_datasets_2000_2019,
                                   length(unique_landclass)))
for (k in 1:length(unique_landclass)){
  for (i in 1:n_datasets_2000_2019){
    for (j in 1:n_datasets_2000_2019){
      
      dummy_mat_1 <- dummy_mat[dummy_mat$dataset %in% EVAP_GLOBAL_DATASETS[i] &
                                 dummy_mat$land_cover_class %in% unique_landclass[k],]
      
      dummy_mat_2 <- dummy_mat[dummy_mat$dataset %in% EVAP_GLOBAL_DATASETS[j] &
                                 dummy_mat$land_cover_class %in% unique_landclass[k],]
      
      dummy_pval <- ks.test(dummy_mat_1$mean_evp, dummy_mat_2$mean_evp)$p.value
      
      if (dummy_pval>=0.05){
        dummy_agreement[i,j,k] <- dummy_pval
      }
      else {
        dummy_agreement[i,j,k] <- 0
      }
      
    }
  }
}


##############################################################
colnames(dummy_agreement) <- EVAP_GLOBAL_DATASETS
rownames(dummy_agreement) <- EVAP_GLOBAL_DATASETS
plotlist = list()

for (i in 1:1:length(unique_landclass)){
  dummy_agreement[upper.tri(dummy_agreement[,,i])] <- NA
  diag(dummy_agreement[,,i]) <- NA
  df <- melt(dummy_agreement[,,i])
  df$value[df$value > 0] <- 1 
  colnames(df) <- c("x", "y", "value")
  df <- na.omit(df)
  
  legend_title <- " "
  
  plotlist[[i]] <- ggplot(df, aes(x = x, y = y, fill = factor(value))) +
    geom_tile(color = "white",lwd = 0.8,linetype = 1) +
    scale_fill_manual(values = c("black","red"),labels = c("Not Matching","Matching"),name="")+
    coord_fixed()+
    ggtitle(unique_landclass[i])+
    theme(text = element_text(size = 10),
          axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
          plot.title = element_text(hjust = 0.5))+
    xlab("") +  ylab("")
  
}

library(grid)
ggarrange(plotlist[[1]], plotlist[[2]],plotlist[[3]],
          plotlist[[4]],plotlist[[5]],plotlist[[6]],
          plotlist[[7]],plotlist[[8]],plotlist[[9]],
          plotlist[[10]],plotlist[[11]],plotlist[[12]],
          plotlist[[13]],plotlist[[14]],
          common.legend = TRUE,
          legend = 'right', align = 'hv',
          nrow = 3, ncol = 5)

ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "heatmap_dist_match_kgclass.png"), 
       width = 16, height = 8)

