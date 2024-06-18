 source('source/partition_evap.R')
 source('source/geo_functions.R')
 
 evap_datasets <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_datasets.rds"))
 evap_mask <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_masks.rds"))
 unique_landcover <- unique(evap_mask$land_cover_short_class)
 
 for (j in 1:length(EVAP_GLOBAL_DATASETS)){
   for (k in 2000:2019){
     
     dummy_mat2 <- evap_datasets[evap_datasets$dataset %in% EVAP_GLOBAL_DATASETS[j] &
                                   evap_datasets$year %in% k,]
     
     for (i in 1:length(unique_landcover)){
       dummy_mat1 <- evap_mask[evap_mask$land_cover_short_class %in% unique_landcover[i],]
       dummy_mat1_subset <- data.frame(lon=dummy_mat1$lon,lat=dummy_mat1$lat)
       
       mean_val <- mean(merge(dummy_mat2,dummy_mat1_subset)$evap)
       
       if (j==1 && k==2000 && i==1){
         dummy_mat <- data.frame(year=k,dataset=EVAP_GLOBAL_DATASETS[j],
                                 land_cover_class=unique_landcover[i],mean_evp=mean_val)
       }else{
         dummy_mat <- dummy_mat %>% 
           add_row(year=k,dataset=EVAP_GLOBAL_DATASETS[j],
                   land_cover_class=unique_landcover[i],mean_evp=mean_val)
       }
     }
   }
 }
 
 #saveRDS(dummy_mat, paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_yearwise_landcover.rds"))
 dummy_mat <- readRDS(paste0(PATH_SAVE_PARTITION_EVAP, "evap_mean_yearwise_landcover.rds"))
 
 ####################################################################################
 dummy_agreement <- array(NA, dim=c(n_datasets_2000_2019,n_datasets_2000_2019,
                                    length(unique_landcover)))
 for (k in 1:length(unique_landcover)){
   for (i in 1:n_datasets_2000_2019){
     for (j in 1:n_datasets_2000_2019){
       
       dummy_mat_1 <- dummy_mat[dummy_mat$dataset %in% EVAP_GLOBAL_DATASETS[i] &
                                  dummy_mat$land_cover_class %in% unique_landcover[k],]
       
       dummy_mat_2 <- dummy_mat[dummy_mat$dataset %in% EVAP_GLOBAL_DATASETS[j] &
                                  dummy_mat$land_cover_class %in% unique_landcover[k],]
       
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
 
 for (i in 1:1:length(unique_landcover)){
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
     ggtitle(unique_landcover[i])+
     theme(text = element_text(size = 10),
           axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
           plot.title = element_text(hjust = 0.5))+
     xlab("") +  ylab("")
   
 }
 
 library(grid)
 ggarrange(plotlist[[1]], plotlist[[2]],plotlist[[3]],
           plotlist[[4]],plotlist[[5]],plotlist[[6]],
           plotlist[[7]],plotlist[[8]],plotlist[[9]],
           common.legend = TRUE,
           legend = 'right', align = 'hv',
           nrow = 2, ncol = 5)
 
 ggsave(paste0(PATH_SAVE_PARTITION_EVAP_FIGURES, "heatmap_dist_match_landcover.png"), 
        width = 16, height = 8)
 
 