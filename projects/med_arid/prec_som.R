library(somspace)
library(pRecipe)
library(ggplot2)

prec <- readRDS('../../shared/data_projects/ithaca/med_prec/pr_year_1980_2019.rds')

#Data pre-processing
names(prec)[6] <- c('dataset') #all variables must be lowercase 
prec[, period := NULL]
prec <- prec[!duplicated(prec)] 

dataset_names_40yr <- prec[year < 1990, unique(dataset)] 
prec_40yr <- prec[dataset %in% dataset_names_40yr]
prec_40yr_mean <- prec_40yr[, .(value = mean(pr_year)), .(year, lat, lon)]

#Self-Organizing Map (SOM)
dim_1 <- 3
dim_2 <- 3
n_nodes <- dim_1 * dim_2 
n_iterations <- 10^4

##Dataset mean
inp_som <- sominp(prec_40yr_mean)
prec_40yr_som <- somspa(inp_som, rlen = n_iterations, grid = somgrid(dim_1, dim_2, "hexagonal"))
plot(prec_40yr_som)
my_regions <- somregs(prec_40yr_som, nregions = 9) 
plot(my_regions, 2:9, 3, 3)

##ERA5
prec_era5 <- prec_40yr[dataset == dataset_names_40yr[1], .(year, lat, lon, value = pr_year)]
inp_som <- sominp(prec_era5)
era5_som <- somspa(inp_som, rlen = n_iterations, grid = somgrid(dim_1, dim_2, "hexagonal"))
plot(era5_som)

##MERRA2
prec_merra2 <- prec_40yr[dataset == dataset_names_40yr[2], .(year, lat, lon,value = pr_year)]
inp_som <- sominp(prec_merra2)
merra2_som <- somspa(inp_som, rlen = n_iterations, grid = somgrid(dim_1, dim_2, "hexagonal"))
plot(merra2_som)

#Slopes
prec_40yr_classes <- prec_40yr_som$input_dt[, .(lon, lat, date = time, value = variable, node)]
prec_40yr_slopes <- list()

for(node_count in 1:n_nodes) {
  prec_40yr_slopes[[node_count]] <- trend(prec_40yr_classes[node == node_count][, 1:4])
}
prec_40yr_slopes_dt <- rbindlist(prec_40yr_slopes, idcol = 'node')
prec_40yr_slopes_dt[, node := factor(node)]

#Plots
ggplot(prec_40yr_slopes_dt) +
  geom_tile(aes(x = lon, y = lat, fill = slope)) +
  scale_fill_gradient2(low = "navyblue", mid = "grey90", high = "darkred", midpoint = 0) +
  theme_light()

ggplot(prec_40yr_slopes_dt) +
  geom_tile(aes(x = lon, y = lat, fill = node)) +
  theme_light()

ggplot(prec_40yr_slopes_dt) +
  geom_density(aes(x = slope, col = node))+
  theme_light()


