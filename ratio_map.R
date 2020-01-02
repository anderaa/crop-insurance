library(ggplot2)
library('RColorBrewer')

# load data 
cty_shape <- read.csv("county_shape.csv")
st_shape <- read.csv("state_shape.csv")
data <- read.csv('predictions_ratio_allfips.csv', stringsAsFactors = FALSE)

# organize loss data
cty_shape$Fips = as.character(cty_shape$Fips)
data$cntyfips <- as.character(data$cntyfips)
data$stfips <- as.character(data$stfips)
data$fips <- NA

for (i in 1:nrow(data)) {
  if (nchar(data$cntyfips[i]) == 1) {
    data$cntyfips[i] = paste('00', data$cntyfips[i], sep='')
  }
  if (nchar(data$cntyfips[i]) == 2) {
    data$cntyfips[i] = paste('0', data$cntyfips[i], sep='')
  }
  data$fips[i] = paste(data$stfips[i], data$cntyfips[i], sep='')
}

cty_shape['lost_corn'] <- NA
cty_shape['lost_wheat'] <- NA
cty_shape['lost_soybeans'] <- NA
cty_shape['lost_peanuts'] <- NA

for (i in 1:nrow(data)) {
  if (data$commoditycode[i] == 41) {
    cty_shape[cty_shape$Fips == data$fips[i], 'lost_corn'] = data$ratiohat[i]
  }
  if (data$commoditycode[i] == 11) {
    cty_shape[cty_shape$Fips == data$fips[i], 'lost_wheat'] = data$ratiohat[i]
  }
  if (data$commoditycode[i] == 81) {
    cty_shape[cty_shape$Fips == data$fips[i], 'lost_soybeans'] = data$ratiohat[i]
  }
  if (data$commoditycode[i] == 75) {
    cty_shape[cty_shape$Fips == data$fips[i], 'lost_peanuts'] = data$ratiohat[i]
  }
}




# map it
ggplot() + 
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle('Fraction of Peanut Acres Lost to Wildlife') + 
  scale_fill_gradientn('', colours = brewer.pal(9, 'YlOrRd')) +
  # county polygons 
  geom_polygon(data = cty_shape[ order(cty_shape$order), ],
               aes(long, lat, group = group, fill = lost_peanuts),
               col = "#c0c5ce", size=0.1) +
  # cords
  coord_map("bonne",  param = 40) + 
  # state polygons
  geom_polygon(data = st_shape[ order(st_shape$state_order), ],
               aes(state_long, state_lat, group = state_group),
               fill = NA,
               col = "black") 
