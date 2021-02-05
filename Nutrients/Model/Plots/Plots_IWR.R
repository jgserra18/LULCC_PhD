source('./Main/Global_functions.R')

require('dplyr')
require('ggplot2')
require('ggrepel')

yrs = paste0('X',seq(1995,2017))


# continuous IWR for each crop 1995-2017 -----------------------------

compute_mainland_IWR = function(main_param = 'Total', IWW = 'dynamic') {
  #' @param main_param is the main param or Total
  #' @param IWW either static (DGADR) or dynamic (GlobWat)
  #' @description 
  #' 
  sub_mod = ifelse(IWW=='dynamic','GlobWat','DGADR')
  
  IWR = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Irrigation', subfolder = 'Irrigation_requirements', subfolderX2 = sub_mod, subfolderX3 = 'Total', subfolderX4 = 'Total', pattern = main_param)
  IWR = sapply(yrs, function(x) sum(IWR[, x], na.rm=T)/1e9)
  IWR = data.frame(yrs = seq(1995,2017), IWR = IWR, main_param = main_param)
  
  return(IWR)
}

compute_all_mainland_IWR_dataset = function( IWW = 'dynamic') {
  #' @param IWW either static (DGADR) or dynamic (GlobWat)
  #' @describeIn computes mainland IWR for all main crops
  #' unit: km3 yr-1
  
  irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
  main_params = unique(irrig_id$Main_crop)
  
  store = data.frame(yrs=NULL, IWR = NULL, main_param = NULL)
  
  for (main_param in main_params) {
    print(main_param)
    IWR = compute_mainland_IWR(main_param, IWW)
    store = rbind(store, IWR)
  }
  return(store)
}

aggregate_mainland_IWR_categories = function( IWW = 'dynamic') {
  #' @param IWW either static (DGADR) or dynamic (GlobWat)
  #' @description reduces the main crops from compute_all_mainland_IWR_dataset
  
  IWR_main_params = compute_all_mainland_IWR_dataset(IWW)
  fruits = c('Dried_nuts','Fresh_fruits','Citrus')
  others = c('Industry_crops','Pulses')
  
  # sum all fruits into one cat
  fruits_df = IWR_main_params %>%
    filter(main_param %in% fruits) %>%
    group_by(yrs) %>%
    summarize(IWR = sum(IWR))
  fruits_df[, 'main_param'] = 'Fruits'
  fruits_df = as.data.frame(fruits_df)
  
  # sum others into one cat
  others_df = IWR_main_params %>%
    filter(main_param %in% others) %>%
    group_by(yrs) %>%
    summarize(IWR = sum(IWR))
  others_df[, 'main_param'] = 'Others'
  others_df = as.data.frame(others_df)
  
  
  # remove fruits from IWR_main_params
  IWR_main_params = subset(IWR_main_params, !main_param %in%fruits & !main_param %in% others)
  # add fruits_df to IWR_main_params
  IWR_main_params = rbind(fruits_df, others_df, IWR_main_params)
  
  IWR_main_params = IWR_main_params %>%
    group_by(main_param)
  
  return(IWR_main_params)
}

View(subset(df, main_param == 'Total'))
df = aggregate_mainland_IWR_categories()
df$main_param = gsub('Olive_grove','Olive grove', df$main_param)
df = rbind(df, df %>%
  group_by(yrs) %>%
  summarise(IWR = sum(IWR)) %>%
  mutate(main_param = 'Total')) %>%
  arrange(yrs, IWR)


last_yr = subset(df, yrs == 2017)

p1 = ggplot(df, aes(x=yrs, y=IWR,  colour=main_param)) + 
  geom_line(size=1.1,alpha=1, show.legend = F) + 
  geom_point(data=last_yr, aes(x=yrs, y=IWR, colour=main_param),size = 3, show.legend = F) + 
  theme_test() + 
  scale_colour_manual(values = c('Others'='azure4',
                                 'Olive grove' = 'black',
                                 'Vineyard' = 'darkslateblue',
                                 'Potato' = 'burlywood4',
                                 'Horticulture' = 'chartreuse',
                                 'Fruits' = 'red1',
                                 'Pastures' = 'orange1',
                                 'Forage' = 'forestgreen',
                                 'Cereals' = 'purple',
                                 'Total' = 'blue1')) +  
  #scale_colour_viridis_d(option = 'D') + 
  xlab('') + ylab(expression(Irrigation~water~withdrawals~(km^{3}~yr^{-1}))) + 
  labs(fill='Main crops') +
  scale_y_continuous(expand=c(0,0.05), limits=c(0,4.2)) + 
  scale_x_continuous(breaks=c(1995,2000,2005,2010,2015)) + 
  theme(
        text = element_text(family='serif', size=14),
        axis.title = element_text(size=14),
        axis.text = element_text(size = 13)) 

p1 = p1 + geom_text_repel(data=last_yr, aes(x=2017,y=IWR, label=main_param, colour=main_param),
                     size=4, 
                     family='serif',
                     show.legend = F,
                     hjust=-0.05,
                     vjust=1,  
                     max.time = 15,
                     max.iter = 1e8,
                     direction='y',force = 12,
                     max.overlaps = Inf,
                     box.padding = -0.1,
                     nudge_x = 1,
                     nudge_y = 0.8,
                     point.padding = 0, 
                     xlim = c(-Inf,+Inf), 
                     ylim = c(-Inf,+Inf)) +
  scale_colour_manual(values = c('Others'='azure4',
                                 'Olive grove' = 'black',
                                 'Vineyard' = 'darkslateblue',
                                 'Potato' = 'burlywood4',
                                 'Horticulture' = 'chartreuse',
                                 'Fruits' = 'red1',
                                 'Pastures' = 'orange1',
                                 'Forage' = 'forestgreen',
                                 'Cereals' = 'purple',
                                 'Total' = 'blue1')) +  
  coord_cartesian(clip = "off")  + 
  theme_classic()  + 
  theme(
    text = element_text(family='serif', size=14),
    axis.title = element_text(size=14),
    axis.text = element_text(size = 13),
    plot.margin = margin(t = 0.1, r = 4.3, b = 0.1, l = 0.1, "cm"),
    panel.grid.major = element_line(colour = 'black', size=0.1, linetype='dotted'),
    axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))
p1  
  
ggsave(p1, filename = './Nutrients/Output/Plots/Irrigation/IWR_1995_2017.jpeg',dpi=600, height = 4.2, width = 5.5)
ggsave(p1, filename = './Nutrients/Output/Plots/Irrigation/IWR_1995_2017.tiff',dpi=600, height = 4.2, width = 5.5, compress='lzw')


# compute average IWR rates (m3 ha-1 yr-1) per municipality 1995-2017 ---------

compute_avg_IWR_rates_muni = function() {
  #' @description Computes the avg IWR rate (mm yr-1) for each muni over the period 1995-2017
  
  IWR_vol = read.csv('./Nutrients/Output/Irrigation/Irrigation_requirements/GlobWat/Total/Total/Total.csv')
  TIA = read.csv('./Nutrients/Activity_data/Correct_data_Municipality/Irrigation/Correct_irrigated_areas_method/Total/Total/Total.csv')
  
  IWR_rates = IWR_vol[, c('Muni_ID','ID','Muni',yrs)]
  IWR_rates[, yrs] = sapply(yrs, function(x) round(IWR_vol[,x]/TIA[,x], 1))
  IWR_rates = data_cleaning(IWR_rates)
  
  store = get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store$Avg_IWR_rates = round(rowMeans(IWR_rates[,yrs])/10, 1) # mm yr-1
  names(store)[1] = 'Admin_id'
  
  muni = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Municipality')
  muni = merge(muni, store[, c('Admin_id','Avg_IWR_rates')])
  
  return(muni)
  rm(list=c('IWR_vol','TIA','store'))
}

library(rworldmap)
library(rworldxtra)
library(tmap)

# prepare GIS plot 
muni = compute_avg_IWR_rates_muni()


world <- getMap(resolution='high')
sb <- subset(world, ADMIN== 'Spain' | ADMIN=='Portugal')
pt <- subset(world, ADMIN=='Portugal')

data = raster('C:\\Users\\serrm\\Downloads\\GEBCO_2020_04_Dec_2020_9fbffc72b8e3\\GEBCO_2020_04_Dec_2020_9fbffc72b8e3\\gebco_2020_n43.453125000000014_s35.718750000000014_w-13.4384765625_e-5.4404296875.tif')
bath = reclassify(data, rcl=c(0, +Inf, NA))
elev = reclassify(data, rcl=c(-Inf, 0, NA))

downscale = F



p1 = tm_shape(bath, projection = "+proj=longlat",raster.downsample = downscale) + 
  tm_raster(legend.show = F, contrast=c(0.3,1),n=20,palette = 'Blues') + 
  tm_shape(elev,  projection = "+proj=longlat",raster.downsample = downscale) +
  tm_raster(palette = 'Greys', alpha = 0.65, n=20, contrast=c(0.3,1), legend.show = F) +
  tm_shape(muni) + tm_borders(col='gray', lwd = 0.5) + 
  tm_polygons(col='Avg_IWR_rates', 
              breaks=c(0,200,400,600,800,+Inf), 
              labels = c('0 - 200','200 - 400','400 - 600','600 - 800','> 800'), 
              palette='YlGnBu', title = 'IWR (mm/yr)') + 
  tm_shape(sb, projection = "+proj=longlat") + tm_borders(col='black',lwd=0.5) +
  tm_graticules(projection = '+proj=longlat',x = c(-11,-6), y=c(36.5,42)) +
  tm_layout(legend.position = c(0.1,0.45),
            legend.outside = F,
            legend.bg.color = 'white',
            legend.frame = T,
            frame = T,
            # legend.height = 0.45,
            legend.width = 0.3,
            legend.text.size = 0.7,
            legend.title.size = 1,
            bg.color = 'gray',
            fontfamily = 'serif',
            outer.margins = 0.01)  +
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.85, 
               position=c(0.05, 0.02), breaks = c(0, 50, 100, 200))
p1
tmap_save(p1, filename = './Nutrients/Output/Plots/Irrigation/Avg_IWR_1995_2017.jpeg', dpi=1000,height = 4, width = 3)

st = stack('./Water/Output/Crop_irrigation_requirements/Annual/Rice.grd')
st[st==0] = NA
cellStats(st,'mean')

require('tmap')
tm_shape(st) + 
  tm_raster(style='kmeans', palette = 'viridis')
View(df)
df = colMeans(IWR_rates[,yrs], na.rm = T)
cor.test(x=seq(1995,2017),y=df, method = 'spearman')
