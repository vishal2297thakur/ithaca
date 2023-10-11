
## somspace
library(somspace)
dummy <- tabular(ensemble_median)
dummy <- dummy[, c(3, 2, 1, 4)]
inp_som <- sominp(dummy)
my_som <- somspa(inp_som, rlen = 10000, grid = somgrid(5, 5, "hexagonal"))
my_regions <- somregs(my_som, nregions = 18) 
plot(my_som$som)
plot(my_regions, 3:18, nrow = 4, ncol =4)
cnet(my_regions, n = 6, thres = 0.7)

## csa
library(csa)



plot_ts(my_regions, 1:5)