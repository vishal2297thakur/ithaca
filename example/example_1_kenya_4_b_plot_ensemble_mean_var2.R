source('source/main.R')
source('source/example_kenya_source.R')
source('example/example_1_kenya_4_a_ensemble_mean_var2.R')

ggplot(var2_mean_df[time == time1]) +
  geom_raster(aes(x = x, y = y, fill = value)) + 
  coord_quickmap()+  labs(x = "Lon", y = "Lat", fill = varname2)+
  scale_fill_gradient2(low ="red", mid = "white", high = "blue", midpoint = 0)+
  theme_bw()

ggplot(var2_mean_df[time == time2,]) +
  geom_raster(aes(x = x, y = y, fill = value)) + 
  coord_quickmap()+  labs(x = "Lon", y = "Lat", fill = varname2)+
  scale_fill_gradient2(low ="red", mid = "white", high = "blue", midpoint = 0)+
  theme_bw()


ggplot(var2_mean_v2_df[time == time1]) +
  geom_raster(aes(x = x, y = y, fill = value)) + 
  coord_quickmap()+  labs(x = "Lon", y = "Lat", fill = varname2)+
  scale_fill_gradient2(low ="red", mid = "white", high = "blue", midpoint = 0)+
  theme_bw()

ggplot(var2_mean_v2_df[time == time2,]) +
  geom_raster(aes(x = x, y = y, fill = value)) + 
  coord_quickmap()+  labs(x = "Lon", y = "Lat", fill = varname2)+
  scale_fill_gradient2(low ="red", mid = "white", high = "blue", midpoint = 0)+
  theme_bw()

