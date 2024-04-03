# Calculate trend (lm, theil_sen, sigel) and significance for masks ----

source('source/evap_trend.R')

library("Kendall")
library(RobustLinearReg)

## Load data----
land_cover_class_global <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "land_cover_class_global.rds"))
biome_class_global <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "biome_class_global.rds"))
elev_class_global <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "elev_class_global.rds"))
evap_class_global <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "evap_class_global.rds"))
ipcc_class_global <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "ipcc_class_global.rds"))
KG_3_class_global <- readRDS(paste0(PATH_SAVE_EVAP_TREND, "KG_3_class_global.rds"))


land_cover_class_global <- land_cover_class_global[!(dataset == "etmonitor" & year == 2000), ]
biome_class_global <- biome_class_global[!(dataset == "etmonitor" & year == 2000), ]
elev_class_global <- elev_class_global[!(dataset == "etmonitor" & year == 2000), ]
evap_class_global <- evap_class_global[!(dataset == "etmonitor" & year == 2000), ]
ipcc_class_global <- ipcc_class_global[!(dataset == "etmonitor" & year == 2000), ]
KG_3_class_global <- KG_3_class_global[!(dataset == "etmonitor" & year == 2000), ]

land_cover_class_global  <- land_cover_class_global [complete.cases(land_cover_class_global)]
biome_class_global  <- biome_class_global [complete.cases(biome_class_global)]
elev_class_global  <- elev_class_global [complete.cases(elev_class_global)]
evap_class_global  <- evap_class_global [complete.cases(evap_class_global)]
ipcc_class_global  <- ipcc_class_global [complete.cases(ipcc_class_global)]
KG_3_class_global  <- KG_3_class_global [complete.cases(KG_3_class_global)]

## Analysis----
### Landcover ----
land_cover_class_global[, year := as.numeric(as.character(year)), ]

land_cover_trend <- land_cover_class_global[, .(lm_slope = lm(evap_mean~year)$coefficients[2], 
                                                            lm_p_value = summary(lm(evap_mean~year))$coefficients[8],
                                                            kendall_tau = Kendall(evap_mean, year)$tau,
                                                            kendall_p_value = Kendall(evap_mean, year)$sl,
                                                            theil_sen_slope = theil_sen_regression(evap_mean~year)$coefficients[2], 
                                                            theil_sen_p_value = summary(theil_sen_regression(evap_mean~year))$coefficients[8],
                                                            siegel_slope = siegel_regression(evap_mean~year)$coefficients[2], 
                                                            siegel_p_value = summary(siegel_regression(evap_mean~year))$coefficients[8]
                                                ), 
                                            .(dataset, land_cover_short_class)]

land_cover_trend[lm_p_value > 0.05,significant_lm:= FALSE] 
land_cover_trend[lm_p_value <= 0.05,significant_lm:= TRUE] 

land_cover_trend[kendall_p_value > 0.05,significant_kendall:= FALSE] 
land_cover_trend[kendall_p_value <= 0.05,significant_kendall:= TRUE] 

land_cover_trend[theil_sen_p_value > 0.05,significant_theil_sen:= FALSE] 
land_cover_trend[theil_sen_p_value <= 0.05,significant_theil_sen:= TRUE] 

land_cover_trend[siegel_p_value > 0.05,significant_siegel:= FALSE] 
land_cover_trend[siegel_p_value <= 0.05,significant_siegel:= TRUE] 

land_cover_trend[significant_theil_sen*theil_sen_slope > 0, trend_score := 1  ]
land_cover_trend[significant_theil_sen*theil_sen_slope < 0, trend_score := -1  ]
land_cover_trend[significant_theil_sen == FALSE, trend_score := 0  ]

land_cover_trend[trend_score == 1, trend_direction := "positive"]
land_cover_trend[trend_score == -1, trend_direction := "negative"]
land_cover_trend[trend_score == 0, trend_direction := "no trend"]

#### plot landcover trend direction ----
ggplot(land_cover_trend)+
  geom_bar(aes(x = dataset, y = trend_score, fill = trend_direction), stat = "identity")+
  geom_abline(intercept = 0, slope = 0, col = "black")+
  scale_fill_manual(values = c("negative" = "darkblue", "positive" = "darkred", "no trend" = "gray"))+
  labs(y = "", fill = "Trend direction")+
  facet_wrap(~land_cover_short_class, ncol = 1)+
  theme_bw()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)
  )
ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "land_cover_avereaged_trend_direction_by_product.png"), 
       width = 12, height = 16)


### Biome ----
biome_class_global[, year:= as.numeric(as.character(year))]

biome_trend <- biome_class_global[, .(lm_slope = lm(evap_mean~year)$coefficients[2], 
                                     lm_p_value = summary(lm(evap_mean~year))$coefficients[8],
                                     kendall_tau = Kendall(evap_mean, year)$tau,
                                     kendall_p_value = Kendall(evap_mean, year)$sl,
                                     theil_sen_slope = theil_sen_regression(evap_mean~year)$coefficients[2], 
                                     theil_sen_p_value = summary(theil_sen_regression(evap_mean~year))$coefficients[8],
                                     siegel_slope = siegel_regression(evap_mean~year)$coefficients[2], 
                                     siegel_p_value = summary(siegel_regression(evap_mean~year))$coefficients[8]
                                     ), 
                                  .(dataset, biome_class)]
biome_trend[lm_p_value > 0.05,significant_lm:= FALSE] 
biome_trend[lm_p_value <= 0.05,significant_lm:= TRUE] 

biome_trend[kendall_p_value > 0.05,significant_kendall:= FALSE] 
biome_trend[kendall_p_value <= 0.05,significant_kendall:= TRUE] 

biome_trend[theil_sen_p_value > 0.05,significant_theil_sen:= FALSE] 
biome_trend[theil_sen_p_value <= 0.05,significant_theil_sen:= TRUE] 

biome_trend[siegel_p_value > 0.05,significant_siegel:= FALSE] 
biome_trend[siegel_p_value <= 0.05,significant_siegel:= TRUE] 

biome_trend[significant_theil_sen*theil_sen_slope > 0, trend_score := 1  ]
biome_trend[significant_theil_sen*theil_sen_slope < 0, trend_score := -1  ]
biome_trend[significant_theil_sen == FALSE, trend_score := 0  ]

biome_trend[trend_score == 1, trend_direction := "positive"]
biome_trend[trend_score == -1, trend_direction := "negative"]
biome_trend[trend_score == 0, trend_direction := "no trend"]

#### plot biome trend direction ----
ggplot(biome_trend)+
  geom_bar(aes(x = dataset, y = trend_score, fill = trend_direction), stat = "identity")+
  geom_abline(intercept = 0, slope = 0, col = "black")+
  scale_fill_manual(values = c("negative" = "darkblue", "positive" = "darkred", "no trend" = "gray"))+
  labs(y = "", fill = "Trend direction")+
  facet_wrap(~biome_class, ncol = 1)+
  theme_bw()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)
  )

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "biome_averaged_trend_direction_by_product.png"), 
       width = 12, height = 16)


### Elevation ----
elev_class_global[, year := as.numeric(as.character(year))]

elev_class_global_trend <- elev_class_global[, .(lm_slope = lm(evap_mean~year)$coefficients[2], 
                                     lm_p_value = summary(lm(evap_mean~year))$coefficients[8],
                                     kendall_tau = Kendall(evap_mean, year)$tau,
                                     kendall_p_value = Kendall(evap_mean, year)$sl,
                                     theil_sen_slope = theil_sen_regression(evap_mean~year)$coefficients[2], 
                                     theil_sen_p_value = summary(theil_sen_regression(evap_mean~year))$coefficients[8],
                                     siegel_slope = siegel_regression(evap_mean~year)$coefficients[2], 
                                     siegel_p_value = summary(siegel_regression(evap_mean~year))$coefficients[8]
                                     ), 
                                     .(dataset, elev_class)]

elev_class_global_trend[lm_p_value > 0.05, significant_lm:= FALSE] 
elev_class_global_trend[lm_p_value <= 0.05, significant_lm:= TRUE] 

elev_class_global_trend[kendall_p_value > 0.05, significant_kendall:= FALSE] 
elev_class_global_trend[kendall_p_value <= 0.05, significant_kendall:= TRUE] 

elev_class_global_trend[theil_sen_p_value > 0.05, significant_theil_sen:= FALSE] 
elev_class_global_trend[theil_sen_p_value <= 0.05, significant_theil_sen:= TRUE] 

elev_class_global_trend[siegel_p_value > 0.05, significant_siegel:= FALSE] 
elev_class_global_trend[siegel_p_value <= 0.05, significant_siegel:= TRUE] 

elev_class_global_trend[significant_theil_sen*theil_sen_slope > 0, trend_score := 1]
elev_class_global_trend[significant_theil_sen*theil_sen_slope < 0, trend_score := -1]
elev_class_global_trend[significant_theil_sen == FALSE, trend_score := 0]

elev_class_global_trend[trend_score == 1, trend_direction := "positive"]
elev_class_global_trend[trend_score == -1, trend_direction := "negative"]
elev_class_global_trend[trend_score == 0, trend_direction := "no trend"]

#### Plot elevation trend direction ----
ggplot(elev_class_global_trend)+
  geom_bar(aes(x = dataset, y = trend_score, fill = trend_direction), stat = "identity")+
  geom_abline(intercept = 0, slope = 0, col = "black")+
  scale_fill_manual(values = c("negative" = "darkblue", "positive" = "darkred", "no trend" = "gray"))+
  labs(y = "", fill = "Trend direction")+
  facet_wrap(~elev_class, ncol = 1)+
  theme_bw()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)
  )

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "elevation_averaged_trend_direction_by_product.png"), 
       width = 12, height = 12)



### Evaporation quantiles ----
evap_class_global[ , year := as.numeric(as.character(year))]

evap_class_global_trend <- evap_class_global[, .(lm_slope = lm(evap_mean~year)$coefficients[2], 
                                     lm_p_value = summary(lm(evap_mean~year))$coefficients[8],
                                     kendall_tau = Kendall(evap_mean, year)$tau,
                                     kendall_p_value = Kendall(evap_mean, year)$sl,
                                     theil_sen_slope = theil_sen_regression(evap_mean~year)$coefficients[2], 
                                     theil_sen_p_value = summary(theil_sen_regression(evap_mean~year))$coefficients[8],
                                     siegel_slope = siegel_regression(evap_mean~year)$coefficients[2], 
                                     siegel_p_value = summary(siegel_regression(evap_mean~year))$coefficients[8]
                                     ), 
                                     .(dataset, evap_quant)]
evap_class_global_trend[lm_p_value > 0.05,significant_lm:= FALSE] 
evap_class_global_trend[lm_p_value <= 0.05,significant_lm:= TRUE] 

evap_class_global_trend[kendall_p_value > 0.05,significant_kendall:= FALSE] 
evap_class_global_trend[kendall_p_value <= 0.05,significant_kendall:= TRUE] 

evap_class_global_trend[theil_sen_p_value > 0.05,significant_theil_sen:= FALSE] 
evap_class_global_trend[theil_sen_p_value <= 0.05,significant_theil_sen:= TRUE] 

evap_class_global_trend[siegel_p_value > 0.05,significant_siegel:= FALSE] 
evap_class_global_trend[siegel_p_value <= 0.05,significant_siegel:= TRUE] 

evap_class_global_trend[significant_theil_sen*theil_sen_slope > 0, trend_score := 1]
evap_class_global_trend[significant_theil_sen*theil_sen_slope < 0, trend_score := -1]
evap_class_global_trend[significant_theil_sen == FALSE, trend_score := 0]

evap_class_global_trend[trend_score == 1, trend_direction := "positive"]
evap_class_global_trend[trend_score == -1, trend_direction := "negative"]
evap_class_global_trend[trend_score == 0, trend_direction := "no trend"]

#### Plot evaporation trend direction ----
ggplot(evap_class_global_trend)+
  geom_bar(aes(x = dataset, y = trend_score, fill = trend_direction), stat = "identity")+
  geom_abline(intercept = 0, slope = 0, col = "black")+
  scale_fill_manual(values = c("negative" = "darkblue", "positive" = "darkred", "no trend" = "gray"))+
  labs(y = "", fill = "Trend direction")+
  facet_wrap(~evap_quant, ncol = 1)+
  theme_bw()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)
  )

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "evap_quantile_averaged_trend_direction_by_product.png"), 
       width = 12, height = 12)


### IPCC reference region ----
ipcc_class_global[ , year := as.numeric(as.character(year))]

ipcc_class_global_trend <- ipcc_class_global[, .(lm_slope = lm(evap_mean~year)$coefficients[2], 
                                                 lm_p_value = summary(lm(evap_mean~year))$coefficients[8],
                                                 kendall_tau = Kendall(evap_mean, year)$tau,
                                                 kendall_p_value = Kendall(evap_mean, year)$sl,
                                                 theil_sen_slope = theil_sen_regression(evap_mean~year)$coefficients[2], 
                                                 theil_sen_p_value = summary(theil_sen_regression(evap_mean~year))$coefficients[8],
                                                 siegel_slope = siegel_regression(evap_mean~year)$coefficients[2], 
                                                 siegel_p_value = summary(siegel_regression(evap_mean~year))$coefficients[8]
), 
.(dataset, IPCC_ref_region)]
ipcc_class_global_trend[lm_p_value > 0.05,significant_lm:= FALSE] 
ipcc_class_global_trend[lm_p_value <= 0.05,significant_lm:= TRUE] 

ipcc_class_global_trend[kendall_p_value > 0.05,significant_kendall:= FALSE] 
ipcc_class_global_trend[kendall_p_value <= 0.05,significant_kendall:= TRUE] 

ipcc_class_global_trend[theil_sen_p_value > 0.05,significant_theil_sen:= FALSE] 
ipcc_class_global_trend[theil_sen_p_value <= 0.05,significant_theil_sen:= TRUE] 

ipcc_class_global_trend[siegel_p_value > 0.05,significant_siegel:= FALSE] 
ipcc_class_global_trend[siegel_p_value <= 0.05,significant_siegel:= TRUE] 

ipcc_class_global_trend[significant_theil_sen*theil_sen_slope > 0, trend_score := 1]
ipcc_class_global_trend[significant_theil_sen*theil_sen_slope < 0, trend_score := -1]
ipcc_class_global_trend[significant_theil_sen == FALSE, trend_score := 0]

ipcc_class_global_trend[trend_score == 1, trend_direction := "positive"]
ipcc_class_global_trend[trend_score == -1, trend_direction := "negative"]
ipcc_class_global_trend[trend_score == 0, trend_direction := "no trend"]

#### Plot IPCC trend direction ----
ggplot(ipcc_class_global_trend)+
  geom_bar(aes(x = dataset, y = trend_score, fill = trend_direction), stat = "identity")+
  geom_abline(intercept = 0, slope = 0, col = "black")+
  scale_fill_manual(values = c("negative" = "darkblue", "positive" = "darkred", "no trend" = "gray"))+
  labs(y = "", fill = "Trend direction")+
  facet_wrap(~IPCC_ref_region, ncol = 3)+
  theme_bw()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)
  )

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "ipcc_regions_averaged_trend_direction_by_product.png"), 
       width = 12, height = 12)

### Koeppen Geiger region ----

KG_3_class_global[ , year := as.numeric(as.character(year))]

KG_class_global_trend <- KG_3_class_global[, .(lm_slope = lm(evap_mean~year)$coefficients[2], 
                                                 lm_p_value = summary(lm(evap_mean~year))$coefficients[8],
                                                 kendall_tau = Kendall(evap_mean, year)$tau,
                                                 kendall_p_value = Kendall(evap_mean, year)$sl,
                                                 theil_sen_slope = theil_sen_regression(evap_mean~year)$coefficients[2], 
                                                 theil_sen_p_value = summary(theil_sen_regression(evap_mean~year))$coefficients[8],
                                                 siegel_slope = siegel_regression(evap_mean~year)$coefficients[2], 
                                                 siegel_p_value = summary(siegel_regression(evap_mean~year))$coefficients[8]
), 
.(dataset, KG_class_3)]
KG_class_global_trend[lm_p_value > 0.05,significant_lm:= FALSE] 
KG_class_global_trend[lm_p_value <= 0.05,significant_lm:= TRUE] 

KG_class_global_trend[kendall_p_value > 0.05,significant_kendall:= FALSE] 
KG_class_global_trend[kendall_p_value <= 0.05,significant_kendall:= TRUE] 

KG_class_global_trend[theil_sen_p_value > 0.05,significant_theil_sen:= FALSE] 
KG_class_global_trend[theil_sen_p_value <= 0.05,significant_theil_sen:= TRUE] 

KG_class_global_trend[siegel_p_value > 0.05,significant_siegel:= FALSE] 
KG_class_global_trend[siegel_p_value <= 0.05,significant_siegel:= TRUE] 

KG_class_global_trend[significant_theil_sen*theil_sen_slope > 0, trend_score := 1]
KG_class_global_trend[significant_theil_sen*theil_sen_slope < 0, trend_score := -1]
KG_class_global_trend[significant_theil_sen == FALSE, trend_score := 0]

KG_class_global_trend[trend_score == 1, trend_direction := "positive"]
KG_class_global_trend[trend_score == -1, trend_direction := "negative"]
KG_class_global_trend[trend_score == 0, trend_direction := "no trend"]

#### Plot KG trend direction ----
ggplot(KG_class_global_trend)+
  geom_bar(aes(x = dataset, y = trend_score, fill = trend_direction), stat = "identity")+
  geom_abline(intercept = 0, slope = 0, col = "black")+
  scale_fill_manual(values = c("negative" = "darkblue", "positive" = "darkred", "no trend" = "gray"))+
  labs(y = "", fill = "Trend direction")+
  facet_wrap(~KG_class_3, ncol = 2)+
  theme_bw()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 1, hjust=1)
  )

ggsave(paste0(PATH_SAVE_EVAP_TREND_FIGURES, "Koeppen-Geiger_averaged_trend_direction_by_product.png"), 
       width = 12, height = 12)
