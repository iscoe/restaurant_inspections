# Compute the number of restaurants within a given radius of each restaurant
# and see if this is predictive of number of violations. Seems to have very 
# little relationship. 

library(data.table) 
library(readr)
library(magrittr)
library(lubridate)
library(sp)
library(geosphere)


# Read data. 
dat <- read_csv("../data/merged.csv") %>% data.table()

# Compute density. 
dat_loc <- subset(dat, select = c("HSISID", "X", "Y")) %>% unique()
n <- nrow(dat_loc)
zeros <- rep(0, n)
dat_loc[ , c("density_half_km" , "density_1_km", "density_2_km", "density_3_km") := 0]
for (i in 1:n){
  print(paste("Proccesing", i, "of", n))
  curr_record <- dat_loc[i,]
  curr_loc <- c(curr_record$X, curr_record$Y)
  dat_loc$dist <- distGeo(curr_loc, as.matrix(dat_loc[ , list(X, Y)]))
  density_half <- dat_loc[ , length(which(dist < 500)) - 1]
  density_1 <- dat_loc[ , length(which(dist < 1000)) - 1]
  density_2 <- dat_loc[ , length(which(dist < 2000)) - 1]
  density_3 <- dat_loc[ , length(which(dist < 3000)) - 1]
  set(dat_loc, i = i, j = 4L, value = density_half)
  set(dat_loc, i = i, j = 5L, value = density_1)
  set(dat_loc, i = i, j = 6L, value = density_2)
  set(dat_loc, i = i, j = 7L, value = density_3)
}
dat_loc[ , dist := NULL]

# The density of resaturants do not appear to be predictive, so we do not 
# save those columns into `dat`, but we do visualize.
dd <- merge(dat_loc, dat, by = c("HSISID", "X", "Y"))
dd[ , mean(num_critical), by = density_half_km] %>% ggplot(aes(density_half_km, V1)) + 
  geom_point() + stat_smooth(span = 1)
dd[ , mean(num_critical), by = density_1_km] %>% ggplot(aes(density_1_km, V1)) + 
  geom_point() + stat_smooth(span = 1)
dd[ , mean(num_critical), by = density_2_km] %>% ggplot(aes(density_2_km, V1)) + 
  geom_point() + stat_smooth(span = 1)
dd[ , mean(num_critical), by = density_3_km] %>% ggplot(aes(density_3_km, V1)) +
  geom_point() + stat_smooth(span = 1)

