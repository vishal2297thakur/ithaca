#############################################################
#Boxplot at annual scale 
#############################################################
#Required library
library(ggh4x)
library(fst)
library(data.table)
library(ggplot2)
library(tidyverse)
library(Kendall)
library(trend)
library(ggpubr)

FILE_PATH <- "~/shared/data_projects/ithaca/pet_europe/data/"
SAVE_PATH <- "~/shared/data_projects/ithaca/pet_europe/figures/"

#laoding data 
basin_classification <- read_fst(paste0(FILE_PATH, "basin_classification.fst"), as.data.table = TRUE)
monthly_dt <- read_fst(paste0(FILE_PATH,"aet_pre_pet_q_tws_twsc.fst"), as.data.table = TRUE)
monthly_dt[, YEAR := year(date)]

#Computing monthly to annual scale
yearly_dt <- monthly_dt[, .(value = sum(value, na.rm = TRUE)), by = .(basin, pet_method, variable, YEAR)]

#averaging the TWS by diving 12 
yearly_dt[variable == "tws", value := value/12]

#Slope agreement for period 1980 to 2019 
slope_dt <- yearly_dt[YEAR >= 1980 & YEAR <= 2019, .(sen_slope = sens.slope(value, conf.level = 0.95)[[1]]), by = .(basin, pet_method, variable)]

#Merging slope datatable with basin types
slope_dt <- merge(slope_dt, basin_classification, by = "basin")

# Outlier classification 

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

slope_dt[, basin_type := as.factor(basin_type)]
slope_dt[, outlier := is_outlier(sen_slope), by = .(pet_method, variable, basin_type)]

# Ordering pet methods based on temperature, radiation and combinational type
pet_method_order <- c("pet_th", "pet_br", "pet_bc", "pet_od", "pet_mb", "pet_hm", "pet_hs", "pet_jh", 
                      "pet_eop", "pet_pt", "pet_pm", "pet_co2")
slope_dt$pet_method <- factor(slope_dt$pet_method, levels = pet_method_order)

#changing name of pet methods and hydrological components  
levels(slope_dt$pet_method) <- c(pet_th = "TH", pet_br = "BR" , pet_bc = "BC", pet_od = "OD", 
                                 pet_mb = "MB", pet_hm = "HM", pet_hs = "HS", pet_jh = "JH", 
                                 pet_eop = "EOP", pet_pt = "PT", pet_pm = "PM", pet_co2 = "CO2"  )
levels(slope_dt$variable) <- c(twsc = "TWSC", pet = "PET", aet = "AET", q = "Q", tws = "TWS", pre = "PRE" )

##############################################
#Boxplots 
#############################################

p1 <- ggplot(data = slope_dt[variable == "PET" & outlier == "FALSE"]) +
  geom_boxplot(aes( fill = pet_method, y = sen_slope), na.rm = TRUE, outlier.shape = NA, width = 0.1) +
  labs(y = NULL, x = NULL) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(name='Method:', values = c(TH= "#E31A1C", BR = "dodgerblue2" , BC = "green4", OD = "#6A3D9A", 
                                              MB ="#FDBF6F", HM= "gold1", HS = "deeppink1", JH = "darkturquoise", 
                                              EOP ="green1", PT = "brown", PM = "blue1", CO2 = "yellow3"  )
  ) +
  facet_grid2(rows = vars(basin_type),cols = vars(variable), scales = "free", independent = "all", 
              labeller = labeller(basin_type = c(energy_limited = "Energy-Limited", mixed = "Mixed", water_Limited = "Water-limited")))+ 
  theme_bw()+
  theme(
    axis.text.y = element_text(size = 18, color = "black"),  
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    strip.text = element_text( size = 18, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    legend.title =  element_text( size = 18, color = "black"),
    legend.direction = "horizontal", 
    legend.text = element_text(size = 18, color = "black"),
    legend.spacing.y = unit(0.1, 'cm'),
    panel.grid = element_blank(), 
    panel.border = element_rect(color = "black", linewidth = 1),
    panel.spacing.x = unit(0.1, "cm"),
    panel.spacing.y = unit(0.1, "cm")
  ) +
  guides(fill = guide_legend(title.position = "left", nrow = 1, label.position = "top", keywidth = unit(2, "cm")))

p2 <- ggplot(data = slope_dt[variable == "AET" & outlier == "FALSE"]) +
  geom_boxplot(aes( fill = pet_method, y = sen_slope), na.rm = TRUE, outlier.shape = NA, position = position_dodge(width = 1)) + #
  labs(y = NULL, x = NULL) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(name='Method:',values = c(TH= "#E31A1C", BR = "dodgerblue2" , BC = "green4", OD = "#6A3D9A", 
                                              MB ="#FDBF6F", HM= "gold1", HS = "deeppink1", JH = "darkturquoise", 
                                              EOP ="green1", PT = "brown", PM = "blue1", CO2 = "yellow3"  )
  ) +
  facet_grid2(rows = vars(basin_type),cols = vars(variable), scales = "free", independent = "all", 
              labeller = labeller(basin_type = c(energy_limited = "Energy-Limited", mixed = "Mixed", water_limited = "Water-Limited"))) + 
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 18, color = "black"),  
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    strip.text = element_text( size = 18, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    legend.title =  element_text( size = 18, color = "black"),
    legend.direction = "horizontal", 
    legend.text = element_text(size = 18, color = "black"),
    legend.spacing.y = unit(0.1, 'cm'),
    panel.grid = element_blank(), 
    panel.border = element_rect(color = "black", linewidth = 1),
    panel.spacing.x = unit(0.1, "cm"),
    panel.spacing.y = unit(0.1, "cm")
  ) +
  guides(fill = guide_legend(title.position = "left", nrow = 1, label.position = "top", keywidth = unit(2, "cm")),)

p3 <- ggplot(data = slope_dt[variable == "Q" & outlier == "FALSE"]) +
  geom_boxplot(aes( fill = pet_method, y = sen_slope), na.rm = TRUE, outlier.shape = NA, position = position_dodge(width = 1)) + #
  labs(y = NULL, x = NULL) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(name='Method:',values = c(TH= "#E31A1C", BR = "dodgerblue2" , BC = "green4", OD = "#6A3D9A", 
                                              MB ="#FDBF6F", HM= "gold1", HS = "deeppink1", JH = "darkturquoise", 
                                              EOP ="green1", PT = "brown", PM = "blue1", CO2 = "yellow3"  )
  ) +
  facet_grid2(rows = vars(basin_type),cols = vars(variable), scales = "free", independent = "all", 
              labeller = labeller(basin_type = c(energy_limited = "Energy-Limited", mixed = "Mixed", water_limited = "Water-Limited"))) + 
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 18, color = "black"),  
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    strip.text = element_text( size = 18, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    legend.title =  element_text( size = 18, color = "black"),
    legend.direction = "horizontal", 
    legend.text = element_text(size = 18, color = "black"),
    legend.spacing.y = unit(0.1, 'cm'),
    panel.grid = element_blank(), 
    panel.border = element_rect(color = "black", linewidth = 1),
    panel.spacing.x = unit(0.1, "cm"),
    panel.spacing.y = unit(0.1, "cm")
  ) +
  guides(fill = guide_legend(title.position = "left", nrow = 1, label.position = "top", keywidth = unit(2, "cm")))


p4 <- ggplot(data = slope_dt[variable == "TWS" & outlier == "FALSE"]) +
  geom_boxplot(aes( fill = pet_method, y = sen_slope), na.rm = TRUE, outlier.shape = NA, position = position_dodge(width = 1)) +
  labs(y = NULL, x = NULL) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(name='Method:',values = c(TH= "#E31A1C", BR = "dodgerblue2" , BC = "green4", OD = "#6A3D9A", 
                                              MB ="#FDBF6F", HM= "gold1", HS = "deeppink1", JH = "darkturquoise", 
                                              EOP ="green1", PT = "brown", PM = "blue1", CO2 = "yellow3"  )
  ) +
  facet_grid2(rows = vars(basin_type),cols = vars(variable), scales = "free", independent = "all", 
              labeller = labeller(basin_type = c(energy_limited = "Energy-Limited", mixed = "Mixed", water_limited = "Water-Limited"))) + 
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 18, color = "black"),  
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    strip.text = element_text( size = 18, color = "black"),
    axis.text = element_text(size = 18, color = "black"),
    legend.title =  element_text( size = 18, color = "black"),
    legend.direction = "horizontal", 
    legend.text = element_text(size = 18, color = "black"),
    legend.spacing.y = unit(0.1, 'cm'),
    panel.grid = element_blank(), 
    panel.border = element_rect(color = "black", linewidth = 1),
    panel.spacing.x = unit(0.1, "cm"),
    panel.spacing.y = unit(0.1, "cm")
  ) +
  guides(fill = guide_legend(title.position = "left", nrow = 1, label.position = "top", keywidth = unit(2, "cm")))

#Arranging boxplot 
p7 <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
p8 <- annotate_figure(p7, left = text_grob("Slope (mm/year)", color = "black", rot = 90, size = 18))
p8
#Saving plot  
ggsave(file = paste0(SAVE_PATH, "boxplot.png"), p8 , width = 13, height = 13, units = "in", dpi = 300)


