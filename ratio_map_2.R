
library('ggplot2')
library('RColorBrewer')
devtools::install_github('anderaa/quickerstats', force=TRUE, build_vignettes=TRUE, ref='development')

key = Sys.getenv('NASS_KEY')
library('quickerstats')
library('scales')

get_ratio_map_data <- function(crop_list, county_shape_path, state_shape_path, ratio_path) {
  # This function return state and county shape files needed to produce ratio maps
  # Args:
  #   crop_list: A list containing lists of crop and crop-code pairs for targeted crops.
  #   county_shape_path: Relative path to county shape file.
  #   state_shape_path: Relative path to state shape file.
  #   ratio_path: Relative path to data containing estimate ratios
  # Returns: A list containing county and state shape files
  
  # load data 
  county <- read.csv(county_shape_path)
  state <- read.csv(state_shape_path)
  data <- read.csv(ratio_path, stringsAsFactors = FALSE)
  
  # organize loss data
  county$Fips = as.character(county$Fips)
  data$cntyfips <- as.character(data$cntyfips)
  data$stfips <- as.character(data$stfips)
  data$fips <- NA
  
  # fix problem with leading zeros in fips strings
  for (i in 1:nrow(data)) {
    if (nchar(data$cntyfips[i]) == 1) {
      data$cntyfips[i] = paste('00', data$cntyfips[i], sep='')
    }
    if (nchar(data$cntyfips[i]) == 2) {
      data$cntyfips[i] = paste('0', data$cntyfips[i], sep='')
    }
    data$fips[i] = paste(data$stfips[i], data$cntyfips[i], sep='')
  }
  
  # loop through crops and add to dataframe of data for map
  for (i in 1:length(crop_list)) {
    county[crop_list[[i]][[1]]] <- NA
    for (j in 1:nrow(data)) {
      if (data$commoditycode[j] == crop_list[[i]][[2]]) {
        county[county$Fips == data$fips[j], crop_list[[i]][[1]]] = data$ratiohat[j]
      }
    }
  }
  
  return(list(county, state))
}


plot_fraction_lost <- function(crop) {
  # plot fraction of acres lost for every county in US
  
  crop <- tolower(crop)
  county_data[crop]
  ggplot() + 
    theme_void() +
    theme(legend.position="bottom") +
    theme(legend.text=element_text(size=12)) +
    theme(legend.key.width=unit(2,"cm")) +
    theme(axis.title=element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    #ggtitle(paste('Percent of', tools::toTitleCase(crop),  'Acres Lost to Wildlife')) + 
    scale_fill_gradientn('', colours = brewer.pal(9, 'YlOrRd'), 
                         limits=c(0, 0.10),
                         breaks=c(0, 0.05, 0.10),
                         oob=squish,
                         labels=c('0%', '5%', '>10%')) +
    
    #scale_fill_gradientn('', colours = brewer.pal(9, 'YlOrRd')) +
    # county polygons 
    geom_polygon(data = county_data[order(county_data$order), ],
                 aes_string('long', 'lat', group = 'group', fill = crop),
                 col = "#c0c5ce", size=0.1) +
    # cords
    coord_map("bonne",  param = 40) + 
    # state polygons
    geom_polygon(data = state_data[ order(state_data$state_order), ],
                 aes(state_long, state_lat, group = state_group),
                 fill = NA,
                 col = "black")
}


# assemble args for ratio data function
county_shape_path <- 'data/county_shape.csv'
state_shape_path <- 'data/state_shape.csv'
ratio_path <- 'data/predictions_ratio_allfips_1_cov.csv'
crop_list <- list(list('corn', 41), list('wheat', 11), list('soybeans', 81), list('cotton', 21))

# get ratio data for mapping
map_data <- get_ratio_map_data(crop_list, county_shape_path, state_shape_path, ratio_path)
county_data <- map_data[[1]]
state_data <- map_data[[2]]


# add zero to fips codes to ease merging with nass data
for (i in 1:nrow(county_data)) {
  if (nchar(county_data$Fips[i]) == 4) {
    county_data$Fips[i] = paste('0', county_data$Fips[i], sep='')
  }
}


merge_sales <- function(map_data, data_item, col_name) {
  # This function add revenue columns to the county mapping data
  crop_data <- get_county_data(key, 2017, data_item)
  crop_data <- within(crop_data, fips <- paste(state_fips_code, 
                                               county_code,sep=''))
  crop_data['fips']
  crop_data['Value']
  map_data[col_name] <- 0
  for (i in 1:nrow(crop_data)) {
    fips <- as.character(crop_data[i, 'fips'])
    value <- crop_data[i, 'Value']
    if (value == '(D)') {
      value = NA
    } else {
      value = gsub(',', '', value)
      value = as.numeric(value)
    }
    map_data[map_data$Fips == fips, col_name] <- value
  }
  return(map_data)
}  

# merge sales data into mapping data
county_data <- merge_sales(county_data, "CORN - SALES, MEASURED IN $", 'corn_revenue')
county_data <- merge_sales(county_data, "WHEAT - SALES, MEASURED IN $", 'wheat_revenue')
county_data <- merge_sales(county_data, "SOYBEANS - SALES, MEASURED IN $", 'soybeans_revenue')
county_data <- merge_sales(county_data, "COTTON, LINT & SEED - SALES, MEASURED IN $", 'cotton_revenue')

# get maps
corn <- plot_fraction_lost('corn')
ggsave('corn_fraction.svg', plot=corn, device='svg', path=NULL,
       scale=1, width=10, height=7, dpi=320, units='in')
soybeans <- plot_fraction_lost('soybeans')
ggsave('soybeans_fraction.svg', plot=soybeans, device='svg', path=NULL,
       scale=1, width=10, height=7, dpi=320, units='in')
wheat <- plot_fraction_lost('wheat')
ggsave('wheat_fraction.svg', plot=wheat, device='svg', path=NULL,
       scale=1, width=10, height=7, dpi=320, units='in')
cotton <- plot_fraction_lost('cotton')
ggsave('cotton_fraction.svg', plot=cotton, device='svg', path=NULL,
       scale=1, width=10, height=7, dpi=320, units='in')


plot_sales <- function(crop) {
  crop <- tolower(crop)
  col_name <- paste(crop, '_revenue', sep='')
  ggplot() + 
    theme_void() +
    theme(legend.position="bottom") +
    theme(legend.key.width=unit(2,"cm")) +
    theme(legend.text=element_text(size=12)) +
    theme(axis.title=element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    scale_fill_gradientn('', colours = brewer.pal(9, 'YlOrRd'), 
                         limits=c(0, 100000000),
                         breaks=c(0, 50000000, 100000000),
                         oob=squish,
                         labels=c('$0', '$50mil', '>$100mil')) +
    # county polygons 
    geom_polygon(data = county_data[order(county_data$order), ],
                 aes_string('long', 'lat', group = 'group', fill = col_name),
                 col = "#c0c5ce", size=0.1) +
    # cords
    coord_map("bonne",  param = 40) + 
    # state polygons
    geom_polygon(data = state_data[ order(state_data$state_order), ],
                 aes(state_long, state_lat, group = state_group),
                 fill = NA,
                 col = "black") 
}


# make plots of sales data
corn_sales_plot <- plot_sales('corn')
ggsave('corn_sales_plot.svg', plot=corn_sales_plot, device='svg', path=NULL,
       scale=1, width=10, height=7, dpi=320, units='in')

soybean_sales_plot <- plot_sales('soybeans')
ggsave('soybean_sales_plot.svg', plot=soybean_sales_plot, device='svg', path=NULL,
       scale=1, width=10, height=7, dpi=320, units='in')

wheat_sales_plot <- plot_sales('wheat')
ggsave('wheat_sales_plot.svg', plot=wheat_sales_plot, device='svg', path=NULL,
       scale=1, width=10, height=7, dpi=320, units='in')

cotton_sales_plot <- plot_sales('cotton')
ggsave('cotton_sales_plot.svg', plot=cotton_sales_plot, device='svg', path=NULL,
       scale=1, width=10, height=7, dpi=320, units='in')


# calculate dollar losses
county_data$corn_loss <- (county_data$corn_revenue / (1 - county_data$corn)) - county_data$corn_revenue
county_data$wheat_loss <- (county_data$wheat_revenue / (1 - county_data$wheat)) - county_data$wheat_revenue
county_data$soybeans_loss <- (county_data$soybeans_revenue / (1 - county_data$soybeans)) - county_data$soybeans_revenue
county_data$cotton_loss <- (county_data$cotton_revenue / (1 - county_data$cotton)) - county_data$cotton_revenue


plot_loss_dollars <- function(crop) {
  crop <- tolower(crop)
  col_name <- paste(crop, '_loss', sep='')
  ggplot() + 
    theme_void() +
    theme(legend.position="bottom") +
    theme(legend.key.width=unit(2,"cm")) +
    theme(legend.text=element_text(size=12)) +
    theme(axis.title=element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    scale_fill_gradientn('', colours = brewer.pal(9, 'YlOrRd'), 
                         limits=c(0, 500000),
                         breaks=c(0, 250000, 500000),
                         oob=squish,
                         labels=c('$0', '$250k', '>$500k')) +
    
    # scale_fill_gradientn('', colours = brewer.pal(9, 'YlOrRd')) +
    # county polygons 
    geom_polygon(data = county_data[order(county_data$order), ],
                 aes_string('long', 'lat', group = 'group', fill = col_name),
                 col = "#c0c5ce", size=0.1) +
    # cords
    coord_map("bonne",  param = 40) + 
    # state polygons
    geom_polygon(data = state_data[ order(state_data$state_order), ],
                 aes(state_long, state_lat, group = state_group),
                 fill = NA,
                 col = "black") 
}


corn_loss_plot <- plot_loss_dollars('corn')
ggsave('corn_county_loss.svg', plot=corn_loss_plot, device='svg', path=NULL,
       scale=1, width=10, height=7, dpi=320, units='in')
soybean_loss_plot <- plot_loss_dollars('soybeans')
ggsave('soybean_county_loss.svg', plot=soybean_loss_plot, device='svg', path=NULL,
       scale=1, width=10, height=7, dpi=320, units='in')
wheat_loss_plot <- plot_loss_dollars('wheat')
ggsave('wheat_county_loss.svg', plot=wheat_loss_plot, device='svg', path=NULL,
       scale=1, width=10, height=7, dpi=320, units='in')
cotton_loss_plot <- plot_loss_dollars('cotton')
ggsave('cotton_county_loss.svg', plot=cotton_loss_plot, device='svg', path=NULL,
       scale=1, width=10, height=7, dpi=320, units='in')


# get state sum data
summary_data <- county_data
summary_data <- summary_data[!duplicated(summary_data$Fips), ]
state_fips <- unique(summary_data$state_fips)
for (state in state_fips) {
  
  corn_loss <- sum(summary_data[summary_data$state_fips == state, 'corn_loss'], na.rm=T)
  state_data[state_data$state_fips == state, 'corn_loss'] <- corn_loss
  
  soybeans_loss <- sum(summary_data[summary_data$state_fips == state, 'soybeans_loss'], na.rm=T)
  state_data[state_data$state_fips == state, 'soybeans_loss'] <- soybeans_loss
  
  wheat_loss <- sum(summary_data[summary_data$state_fips == state, 'wheat_loss'], na.rm=T)
  state_data[state_data$state_fips == state, 'wheat_loss'] <- wheat_loss
  
  cotton_loss <- sum(summary_data[summary_data$state_fips == state, 'cotton_loss'], na.rm=T)
  state_data[state_data$state_fips == state, 'cotton_loss'] <- cotton_loss
  
}

plot_state_loss <- function(series) {
  ggplot() + 
    theme_void() +
    theme(legend.position="bottom") +
    theme(legend.key.width=unit(2,"cm")) +
    theme(legend.text=element_text(size=12)) +
    theme(axis.title=element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    scale_fill_gradientn('', colours = brewer.pal(9, 'YlOrRd'), 
                         limits=c(0, 40000000),
                         breaks=c(0,20000000, 40000000),
                         oob=squish,
                         labels=c('$0', '20mil', '>$40mil')) +
    
    # scale_fill_gradientn('', colours = brewer.pal(9, 'YlOrRd')) +
    # county polygons 
    geom_polygon(data = state_data[order(state_data$state_order), ],
                 aes_string('state_long', 'state_lat', group = 'state_group', 
                            fill = series),
                 col = "#c0c5ce", size=0.1) +
    # cords
    coord_map("bonne",  param = 40) 
}

state_corn_plot <- plot_state_loss('corn_loss')
ggsave('state_corn_plot.svg', plot=state_corn_plot, device='svg', path=NULL,
       scale=1, width=10, height=7, dpi=320, units='in')
state_soybeans_plot <- plot_state_loss('soybeans_loss')
ggsave('state_soybeans_plot.svg', plot=state_soybeans_plot, device='svg', path=NULL,
       scale=1, width=10, height=7, dpi=320, units='in')
state_wheat_plot <- plot_state_loss('wheat_loss')
ggsave('state_wheat_plot.svg', plot=state_wheat_plot, device='svg', path=NULL,
       scale=1, width=10, height=7, dpi=320, units='in')
state_cotton_plot <- plot_state_loss('cotton_loss')
ggsave('state_cotton_plot.svg', plot=state_cotton_plot, device='svg', path=NULL,
       scale=1, width=10, height=7, dpi=320, units='in')


state_summary_data <- state_data[!duplicated(state_data$state_fips), ]
dim(state_summary_data)
write.csv(state_summary_data, 'state_summary.csv')

sum(state_summary_data['corn_loss'], na.rm=T)
sum(state_summary_data['soybeans_loss'], na.rm=T)
sum(state_summary_data['wheat_loss'], na.rm=T)
sum(state_summary_data['cotton_loss'], na.rm=T)

# cotton_data <- get_county_data(key, 2017, 'COTTON - PRODUCTION, MEASURED IN BALES')
# corn_data <- get_county_data(key, 2017, 'CORN, GRAIN - PRODUCTION, MEASURED IN BU')
# soybean_data <- get_county_data(key, 2017, 'SOYBEANS - PRODUCTION, MEASURED IN BU')
# wheat_data <- get_county_data(key, 2017, 'WHEAT - PRODUCTION, MEASURED IN BU')

