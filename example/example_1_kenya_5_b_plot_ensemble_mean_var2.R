source('source/main.R')
source('source/example_kenya_source.R')
source('example/example_1_kenya_5_a_comparing_periods_var2.R')


ggplot(var2_mean_df_change) +
  geom_raster(aes(x = x, y = y, fill = percent_change_e)) + 
  coord_quickmap()+  labs(x = "Lon", y = "Lat", fill = "Change in e [%]")+
  scale_fill_gradient2(low ="red", mid = "white", high = "blue", midpoint = 0)+
  theme_bw()
