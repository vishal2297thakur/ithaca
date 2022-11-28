source('source/main.R')
source('source/example_kenya_source.R')
source('example/example_1_kenya_2_a_ensemble_mean_var1.R')

ggplot(var1_mean_df[time == time1]) +
  geom_raster(aes(x = x, y = y, fill = value)) + 
  coord_quickmap()+  labs(x = "Lon", y = "Lat", fill = varname1)+
  scale_fill_gradient2(low ="red", mid = "white", high = "blue", midpoint = 0)+
  theme_bw()

ggplot(var1_mean_df[time == time2,]) +
  geom_raster(aes(x = x, y = y, fill = value)) + 
  coord_quickmap()+  labs(x = "Lon", y = "Lat", fill = varname1)+
  scale_fill_gradient2(low ="red", mid = "white", high = "blue", midpoint = 0)+
  theme_bw()


ggplot(var1_mean_v2_df[time == time1]) +
  geom_raster(aes(x = x, y = y, fill = value)) + 
  coord_quickmap()+  labs(x = "Lon", y = "Lat", fill = varname1)+
  scale_fill_gradient2(low ="red", mid = "white", high = "blue", midpoint = 0)+
  theme_bw()

ggplot(var1_mean_v2_df[time == time2,]) +
  geom_raster(aes(x = x, y = y, fill = value)) + 
  coord_quickmap()+  labs(x = "Lon", y = "Lat", fill = varname1)+
  scale_fill_gradient2(low ="red", mid = "white", high = "blue", midpoint = 0)+
  theme_bw()
