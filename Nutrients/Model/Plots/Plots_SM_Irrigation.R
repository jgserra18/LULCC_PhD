source('./Main/Data_operations.R')

require('tmap')
require('ggplot2')
require('raster')

# TIA plots ------------------------------------------------

# modelled vs statistical 2009
source('./Nutrients/Model/Irrigation/Support/3_Populate_irrigated_areas.R')

modelled = compute_total_irrigated_areas()[, c('Muni_ID','X2009')]
statistics = compute_total_X2009_irrigated_areas()

df = data.frame(mod=modelled$X2009, stat=statistics$X2009)
ggplot(df, aes(mod, X2009)) + 
  geom_point(size=2) + 
  geom_smooth(method='lm', colour='red1', se = T) + 
  geom_abline(slope = 1, intercept = 0, colour='black') + 
  scale_y_continuous(limits=c(0, 15000), breaks=seq(0,15000,2500)) + 
  scale_x_continuous(limits=c(0, 15000), breaks=seq(0,15000,2500)) + 
  xlab('Modelled TIA 2009 (ha)') + ylab('Statistical TIA 2009 (ha)') + labs(colour='') + 
  theme_classic() + 
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        text = element_text(family='serif', size=14),
        axis.title = element_text(size=15.5),
        axis.text = element_text(size = 14),
        legend.text = element_text(size=14),
        panel.grid.major = element_line(colour = 'black', size=0.3, linetype='dotted'),
        axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))
summary(lm(X2009~mod, df))$r.squared
ggsave(filename = './Nutrients/Output/Plots/Irrigation/Supp_materials/ModelledVSStatistical_TIA.tiff',compress='lzw')  


# TIA vs other sources

FAO_data = data.frame(yrs=c(2005,2007,2010, 2013, 2016),
                      tia = c(454, 583.7, 466, 479.8, 474.1),
                      src = 'FAOSTAT')
aquastat = data.frame(yrs=2007, tia = 421.5, src = 'AQUASTAT')

# corrected TIA (vs statistical data total crop areas)
tia = read.csv('./Nutrients/Activity_data/Correct_data_Municipality/Irrigation/Correct_irrigated_areas_method/Total/Total/Total.csv')
tia = compute_mainland_total(tia)

modelled_corr = data.frame(yrs=seq(1995,2017),
                      tia = as.numeric(tia[, paste0('X',seq(1995,2017))])/1000,
                      src = 'Modelled')
modelled_corr
# incorrected TIA (ie, not corrected to data total crop areas)
tia = read.csv('./Nutrients/Activity_data/Correct_data_Municipality/Irrigation/Incorrect_irrigated_areas_method//Total/Total/Total.csv')
tia = compute_mainland_total(tia)
modelled_inc = data.frame(yrs=seq(1995,2017),
                      tia = as.numeric(tia[, paste0('X',seq(1995,2017))])/1000,
                      src = 'Modelled')

require('ggrepel')
all = rbind(modelled_corr,aquastat, FAO_data)
p_corr = ggplot() + 
  geom_line(data = subset(all, src == 'Modelled'), aes(x=yrs, y=tia, color=src), size = 1.5) + 
  geom_point(data = subset(all, src != 'Modelled'), aes(x=yrs, y=tia, color=src), size = 3) + 
  scale_y_continuous(limits=c(0,650)) + 
  xlab('') + ylab('Total irrigated areas (1000 ha)') + labs(colour='') + 
  facet_grid(.~'Corrected data') + 
  theme_classic() + 
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        text = element_text(family='serif', size=14),
        axis.title = element_text(size=15.5),
        axis.text = element_text(size = 14),
        legend.text = element_text(size=14),
        panel.grid.major = element_line(colour = 'black', size=0.1, linetype='dotted'),
        axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))
p_corr
all = rbind(modelled_inc,aquastat, FAO_data)
p_inc = ggplot() + 
  geom_line(data = subset(all, src == 'Modelled'), aes(x=yrs, y=tia, color=src), size = 1.5) + 
  geom_point(data = subset(all, src != 'Modelled'), aes(x=yrs, y=tia, color=src), size = 3) + 
  scale_y_continuous(limits=c(0,650)) + 
  xlab('') + ylab('Total irrigated areas (1000 ha)') + labs(colour='') + 
  facet_grid(.~'Data not corrected') + 
  theme_classic() + 
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        text = element_text(family='serif', size=14),
        axis.title = element_text(size=15.5),
        axis.text = element_text(size = 14),
        legend.text = element_text(size=14),
        panel.grid.major = element_line(colour = 'black', size=0.1, linetype='dotted'),
        axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))
p_inc

both = gridExtra::grid.arrange(p_corr, p_inc, ncol=1)

ggsave(plot = both, filename = './Nutrients/Output/Plots/Irrigation/Supp_materials/TIA_srcs.tiff',compress='lzw')  


FAO_data$modelled_corr= modelled_corr[which(modelled_corr$yrs %in% c(2005,2007,2010, 2013, 2016)),'tia']
FAO_data$diff = 1- FAO_data$modelled_corr/FAO_data$tia
mean(FAO_data$diff)
sd(FAO_data$diff)

FAO_data$modelled_inc= modelled_inc[which(modelled_inc$yrs %in% c(2005,2007,2010, 2013, 2016)),'tia']
FAO_data$diff = 1- FAO_data$modelled_inc/FAO_data$tia
mean(FAO_data$diff)*100
sd(FAO_data$diff)*100


# TIA vs eurostat
require('dplyr')

tia =read.csv('./Nutrients/Activity_data/Correct_data_Municipality/Irrigation/Correct_irrigated_areas_method/Total/Total/Total.csv')
tia = general_sumIF_NUTS2(admin = 'NUTS2', tia)
tia = tia[, c('nuts2_ID','X2005','X2007','X2010','X2013')]
tia[6, c('X2005','X2007','X2010','X2013')] = colSums(tia[, c('X2005','X2007','X2010','X2013')])
tia[6, 1] = 'PT'
tia = reshape2::melt(tia, c('nuts2_ID'))
tia = tia %>% arrange(nuts2_ID)

tia_euro = read.csv('./Nutrients/Activity_data/General_params/Irrigation/Comparison_source/Eurostat_TIA.csv')
tia_euro = reshape2::melt(tia_euro, c('nuts2_ID'))
tia_euro = tia_euro %>% arrange(nuts2_ID)
names(tia_euro)[3] = 'eurostat'
tia_euro$modelled = tia$value
tia_euro$dif = 1-tia_euro$modelled/tia_euro$eurostat

mean(tia_euro[which(tia_euro$nuts2_ID==18), 'dif'])
sd(tia_euro[which(tia_euro$nuts2_ID==18), 'dif'])
View(tia_euro)

ggplot(tia_euro, aes(x=eurostat/1e3, y=modelled/1e3)) + 
  geom_point(aes(colour=nuts2_ID),size=3.5, fill='black') + 
  scale_fill_brewer(palette='Set1') + 
  geom_abline(slope = 1, intercept = 0, colour='black') + 
  geom_smooth(method='lm',colour='red1', se=F) + 
  scale_y_continuous(limits=c(0,500), expand=c(0,0)) + 
  scale_x_continuous( limits=c(0,500), expand=c(0,0)) + 
  xlab('Eurostat TIA (1000 ha)') + ylab('Modelled TIA (1000 ha)') + 
  labs(colour='NUTS2 ID') + 
  theme(
    text = element_text(family='serif', size=14),
    axis.title = element_text(size=15.5, face = 'bold'),
    axis.text = element_text(size = 15),
    axis.ticks.length = unit(0.1,'cm'),
    panel.grid.major  = element_line(colour = 'black', size=0.5, linetype='dotted'),
    axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))

ggsave(filename = './Nutrients/Output/Plots/Irrigation/Supp_materials/EUROSTAT_VS_TIA.tiff',compress='lzw')
summary(lm(modelled~eurostat, tia_euro))$r.squared




iww =read.csv('./Nutrients/Output/Irrigation/Irrigation_requirements/GlobWat/Total/Total/Total.csv')
iww = compute_temporal_sumIF_admin_df(admin = 'NUTS2', iww)
iww = iww[, c('Admin_id','X2010')]
iww[6, c('X2010')] = sum(iww$X2010)
iww[6, 1] = 'PT'
iww$eurostat = c(797722890, 161272460, 868334330, 179464350, 1405506770, 3437365770)
iww$dif = 1-iww$X2010/iww$eurostat

ggplot(iww, aes(x=eurostat,y=X2010)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_smooth(method='lm')

# IWW -------------------------
IWW = read.csv('./Nutrients/Output/Irrigation/Irrigation_requirements/GlobWat/Total/Total/Total.csv')
IWW = compute_temporal_sumIF_admin_df(admin = 'NUTS2', merge_df = IWW, yrs = paste0('X',seq(1995,2017)))
IWW_2010 = IWW[,'X2010']

#' @source https://ec.europa.eu/eurostat/databrowser/view/EF_POIRRIG__custom_431022/default/table?lang=en || 
#' @source EF_POIRRIG__custom_431022

nuts2_df = data.frame(nuts=c(11,16,17,18,15, 'PT'),
                      modelled = c(IWW_2010, sum(IWW_2010)),
                      eurostat = c(797722890, 868334330, 179464350, 1405506770, 161272460, 3437365770))
summary(lm(eurostat~modelled,nuts2_df))$r.squared
nuts2_df$dif = 1-nuts2_df$modelled/nuts2_df$eurostat
sd(nuts2_df$dif)

#' @source AQUASTAT
aquastat_df = data.frame(yrs=c(1997,2002, 2007, 2012, 2017),
                         fao=c(6.35, 6.56, 6.56, 3.76, 3.76),
                         modelled = as.numeric(sapply(c('X1997','X2002','X2007','X2012','X2017'), 
                                                      function(x) sum(IWW[x])/1e9)))
aquastat_df$dif = 1-aquastat_df$modelled/aquastat_df$fao

ggplot() + 
  geom_point(data=nuts2_df, aes(x=modelled/1e9,y=eurostat/1e9, color=nuts, shape=nuts), size=5) + 
  geom_smooth(data = nuts2_df, mapping = aes(x=modelled/1e9,y=eurostat/1e9), method='lm',se = F) + 
  geom_abline(slope = 1, intercept = 0, colour='black') + 
  labs(colour='Administrative\nregions', shape='Administrative\nregions') + 
  xlab('Modelled irrigation withdrawals (km3)') + 
  ylab('Eurostat irrigation withdrawals (km3)') + 
  theme_classic() + 
  theme(
    text = element_text(family='serif', size=14),
    axis.title = element_text(size=14),
    axis.text = element_text(size = 13),
    panel.grid.major = element_line(colour = 'black', size=0.1, linetype='dotted'),
    axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))


# crop irrigation requirements (DGADR) regions) -------------------------------------------------------------------------------

source('./Nutrients/Model/Irrigation/1_Crop_irrigation_requirements.R')

region_template = create_crop_irrigation_requirements('Cereals','Wheat')
names(region_template)[1] = 'Admin_id'
muni = get_activity_data(module = 'LULCC', folder = 'Admin',pattern = 'Munici')
muni = merge(muni, region_template[, c('Admin_id','Volume_regions')], 'Admin_id')

p = tm_shape(muni) +
  tm_polygons(col='Volume_regions', title='Regions', labels=c('Inland North','Coastal North','South'), palette=c('yellow','blue1','orange'))
tmap_save(tm = p, filename = './Nutrients/Output/Plots/Irrigation/Supp_materials/CropIrrig_regions.jpeg', dpi=600, height = 5, width = 3.5)


HID_aei = stack('c:/Users/serrm/Downloads/HID_aei_ha.nc')

tm_shape(HID_aei[[1]])


# monitoring stations nitrate --------------------------------------------------------------------------------------------------

source('./Nutrients/Model/Irrigation/Preprocessing/1_Preprocessing_2017.R')  
source('./Nutrients/Model/Irrigation/Preprocessing/2_Preprocessing_1987_2016.R') 

library(rworldmap)
library(rworldxtra)
library(tmap)

# prepare plot params
downscale=F
world <- getMap(resolution='high')
sb <- subset(world, ADMIN== 'Spain' | ADMIN=='Portugal')
pt <- subset(world, ADMIN=='Spain')

data = raster('C:\\Users\\serrm\\Downloads\\GEBCO_2020_04_Dec_2020_9fbffc72b8e3\\GEBCO_2020_04_Dec_2020_9fbffc72b8e3\\gebco_2020_n43.453125000000014_s35.718750000000014_w-13.4384765625_e-5.4404296875.tif')
bath = reclassify(data, rcl=c(0, +Inf, NA))
elev = reclassify(data, rcl=c(-Inf, 0, NA))

# prepare stations

yrs = as.character(seq(1995,2017))
all_stations  = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'Irrigation', subfolderX3 = 'GW_monitoring_station',  subfolderX4 = '1987-2016', pattern = 'GW')
store_plots = list()
ctr = 1
yr = '2016'
for (yr in yrs) {
  print(yr)
  if (yr == '2017') {
    stations_shp = spatialize_stations_X2017(stations_source = 'GW')[[2]]
  }
  else {
    all_stations  = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'Irrigation', subfolderX3 = 'GW_monitoring_station',  subfolderX4 = '1987-2016', pattern = 'GW')
    stations_shp = spatialize_annual_mean_NO3(yr, all_stations)[[2]]
  }
  
  store_plots[[ctr]] = tm_shape(bath, projection = "+proj=longlat",raster.downsample = downscale) + 
    tm_raster(legend.show = F, contrast=c(0.3,1),n=20,palette = 'Blues') + 
    tm_shape(elev,  projection = "+proj=longlat",raster.downsample = downscale) +
    tm_raster(palette = 'Greys', alpha = 0.65, n=20, contrast=c(0.3,1), legend.show = F) +
    tm_shape(stations_shp, projection = '+proj=longlat') + 
    tm_dots(col='Avg_NO3', 
            title = 'Nitrate concentration\n(mg/L)',
            breaks=c(0,10,25,50,100, +Inf), 
            labels = c('< 10','10-25','25-50','50-100','>100'),
            palette=c('blue1','green1','yellow1','red1', 'purple'),size=0.1) +
    tm_shape(sb, projection = "+proj=longlat") + tm_borders(col='black',lwd=0.5) +
    tm_graticules(projection = '+proj=longlat',x = c(-11,-6), y=c(36.5,42)) +
    tm_layout(legend.position = c(0.1,0.45),
              legend.outside = F,
              legend.bg.color = 'white',
              legend.frame = T,
              frame = T,
              legend.width = 0.3,
              legend.text.size = 0.7,
              legend.title.size = 0.9,
              bg.color = 'gray',
              fontfamily = 'serif',
              outer.margins = 0.01,
              panel.labels = paste0(yr,', ', paste0('N = ', nrow(stations_shp))),
              panel.label.size = 0.9)  +
    tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.85, 
                 position=c(0.05, 0.02), breaks = c(0, 50, 100, 200))
  
  ctr = ctr + 1
}
i=1
start = i
end = i + 5
start = end + 1
end = ifelse(start+5>23, 23, start + 5)

# sequential plots (2x3)
for (i in c(1,7,13,20)) {
     
  start = i
  end = ifelse(i+6>23,23,i+6)
  print(start)
  print(end)
  all_plot = tmap_arrange(lapply(start:end, function(x) store_plots[[x]]), ncol = 2)
  tmap_save(all_plot, paste0('./Nutrients/Output/Plots/Irrigation/Supp_materials/GW_stations_',start,'.jpeg'),
            dpi=600, height = ifelse(end==23, 17, 17), width = 8.7)
}

# testing vs training sets of the RF algorithm ------------------------------------------------------------------

# get R2 per year
pred_no3_gw_R2 =list.files(path = './Nutrients/Output/Irrigation/Nitrate_modelling/GW/Spatial_prediction/', pattern='.tif')
pred_no3_gw_R2 = gsub('RF_GW_','', pred_no3_gw_R2)
yrs = as.character(seq(1995,2017))

for (i in seq_along(pred_no3_gw_R2)) { pred_no3_gw_R2[i] = gsub(paste0(yrs[i],'_'), '', pred_no3_gw_R2[i])}
pred_no3_gw_R2 = gsub('.tif','', pred_no3_gw_R2)

r2_df = data.frame(yrs=yrs, r2 = pred_no3_gw_R2)
r2_df = sapply(1:2, function(x) as.numeric(r2_df[,x]))
r2_df = as.data.frame(r2_df)
names(r2_df) = c('yrs','r2')
# plot
library(ggrepel)
ggplot(r2_df, aes(yrs,r2)) + 
  geom_vline(xintercept = 2003, color='blue1', size = 1) + 
  geom_point(colour='red1',size=2.5) + 
  geom_line(colour='red1',size=1.2) + 
  geom_label_repel(aes(label=round(r2,2)), 
                   label.padding = 0.25, 
                   force = 11, direction = 'both') + 
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1)) + 
  geom_text(aes(x=1997, y = 0.1), label='Period 1', colour='blue1', size = 7.5, family='serif') + 
  geom_text(aes(x=2012, y = 0.1), label='Period 2', colour='forestgreen', size = 7.5, family='serif') + 
  theme_classic()  + 
  xlab('') + ylab(expression(R^{2})) + 
  theme(
    text = element_text(family='serif', size=14),
    axis.title = element_text(size=14),
    axis.text = element_text(size = 13),
    panel.grid.major = element_line(colour = 'black', size=0.1, linetype='dotted'),
    axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=1, linetype='solid')) 

ggsave(filename = './Nutrients/Output/Plots/Irrigation/Supp_materials/GW_RF_R2.tiff', compress='lzw')





# FRAC irrig source ------------------------------------------------------------------------------------------------------------
# after running monitoring stations
source('./Nutrients/Model/Irrigation/3_Irrigation_source.R')

gw = get_irrigation_source_AR()
gw$FRAC_surface = gw$FRAC_dam+gw$FRAC_river
agr = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Agrarian')
agr = merge(agr,gw,'Admin_id')
agr$FRAC_groundwater
p = tm_shape(bath, projection = "+proj=longlat",raster.downsample = downscale) + 
  tm_raster(legend.show = F, contrast=c(0.3,1),n=20,palette = 'Blues') + 
  tm_shape(elev,  projection = "+proj=longlat",raster.downsample = downscale) +
  tm_raster(palette = 'Greys', alpha = 0.65, n=20, contrast=c(0.3,1), legend.show = F) +
  tm_shape(agr, projection = "+proj=longlat") + 
  tm_polygons(col='FRAC_groundwater', breaks=seq(60,85,5), palette='Reds', title = 'Groundwater\nsource (%)') + 
  tm_shape(sb, projection = "+proj=longlat") + tm_borders(col='black',lwd=0.5) +
  tm_graticules(projection = '+proj=longlat',x = c(-11,-6), y=c(36.5,42)) +
  tm_layout(legend.position = c(0.1,0.45),
            legend.outside = F,
            legend.bg.color = 'white',
            legend.frame = T,
            frame = T,
            legend.width = 0.3,
            legend.text.size = 0.7,
            legend.title.size = 0.9,
            bg.color = 'gray',
            fontfamily = 'serif',
            outer.margins = 0.01) +
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.85, 
               position=c(0.05, 0.02), breaks = c(0, 50, 100, 200))
tmap_save(tm = p, filename = './Nutrients/Output/Plots/Irrigation/Supp_materials/FRAC_GW.jpeg', dpi=600, height = 5, width = 3.5)

# RF model mode exp vars -------------------------------------------------------------------------------------------------------
require('dplyr')
top25_vars = lapply(list.files(path = './Nutrients/Output/Irrigation/Nitrate_modelling/GW/Spatial_prediction/', pattern = '.csv',full.names = T), read.csv)
names(top25_vars) = paste0('X',seq(2004,2017))

yrs = as.character(seq(2004,2017))
df = data.frame(imp=NULL, var=NULL, yr = NULL)


for (i in seq_along(top25_vars)) {
  var = top25_vars[[i]] %>%
    arrange(-imp) 
  var = var[1:25, ]
  var$yr = yrs[i]
  df = rbind(df, var)
}

df = df[-which(df$var=='Slope_fine'), ]
df[which(grepl('Total_leaching', df$var)==T), 'var'] = 'Leaching'
df = df[-which(grepl('RF_GW', df$var)==T), ] 

counts = df %>%
  arrange(imp) %>%
  count(var) %>%
  top_n(15) 

counts$avg = sapply(1:15, function(x) mean(df[which(df$var==counts[x, 1]), 'imp']))
counts$sd = sapply(1:15, function(x) sd(df[which(df$var==counts[x, 1]), 'imp']))
counts$names = paste0(counts$var, ', n=', counts$n)

library(forcats)
ggplot(counts, aes(x=fct_reorder(names, avg), y=avg)) + 
  geom_segment(aes(xend=fct_reorder(names, avg), yend=0)) + 
  geom_point( size=4, color="orange") +
  coord_flip() + 
  theme_bw() + 
  xlab('Top 15 explanatory variables') + 
  ylab('Average IncNodePurity') + 
  scale_x_discrete() + 
  scale_y_continuous(expand=c(0,0), limits=c(0,21000)) + 
  theme(
    text = element_text(family='serif', size=14),
    axis.title = element_text(size=15.5),
    axis.text = element_text(size = 14),
    legend.text = element_text(size=14),
    panel.grid.major = element_line(colour = 'black', size=0.1, linetype='dotted'))
View(counts)
ggsave(filename = './Nutrients/Output/Plots/Irrigation/Supp_materials/Top15Vars.tiff', dpi=600, compress='lzw')

  
  
  
# plot N_irrig with heurestic SA -------------------------------------------------------------------------------------------------------

N_irrig = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Avg_municipality_ABS.csv')
N_irrig = compute_mainland_total(N_irrig)[, -1]
N_irrig = data.table::transpose(N_irrig)
N_irrig = cbind(N_irrig, yrs = seq(1995,2017))
names(N_irrig)[1] = 'baseline_Nirrig'

# select heuristic SA vars and compute mainland

get_heuristicSA_vars_mainland = function (var_name, col_name, param_modifier = 0.25) {
  #' @param var_name no3_gw, no3_sw, IWR, All
  #' @param param_modifier mod integer for SA
  
  param_modifier = as.character(1 + param_modifier)
  # first search for upper/lower according to param modifier
  # then search for param_modifier
  SA_var = list.files(path = './Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Heuristic_SA/', pattern = param_modifier, full.names = T)
  SA_var = read.csv(SA_var[which(grepl(var_name,SA_var)==T)])
  
  SA_var = compute_mainland_total(SA_var) # mainland computation and preprocess file
  SA_var = data.table::transpose(SA_var[-1])
  SA_var = cbind(SA_var, yrs = seq(1995,2017))
  names(SA_var)[1] = col_name
  SA_var
}

# plot heuristic SA for a given var
populate_N_irrig_heuristicSA_vars = function(N_irrig_df) {
  
  vars = c('no3_gw','no3_sw','IWR_gw','IWR_sw','All')
  for (var in vars) {
    
    upper_var = get_heuristicSA_vars_mainland(var, paste0('upper_',var), param_modifier = 0.25)
    lower_var = get_heuristicSA_vars_mainland(var, paste0('lower_',var), param_modifier = -0.25)
    N_irrig_df[, paste0('upper_',var)] =upper_var[,1]
    N_irrig_df[, paste0('lower_',var)] = lower_var[,1]
  }
  N_irrig_df
}

N_irrigd = populate_N_irrig_heuristicSA_vars(N_irrig)
head(N_irrigd)

SM_XX = ggplot() + 
  geom_ribbon(data=N_irrigd, aes(x=yrs, ymin=lower_All /1e6,ymax=upper_All  /1e6, fill="All"), color=NA, alpha=.3) +
  geom_ribbon(data=N_irrigd, aes(x=yrs, ymin=lower_IWR_gw /1e6,ymax=upper_IWR_gw /1e6, fill='IWW'),color=NA, alpha=.3) + 
  geom_ribbon(data=N_irrigd, aes(x=yrs, ymin=lower_no3_gw /1e6,ymax=upper_no3_gw /1e6, fill='NO3_GW'),color=NA, alpha=.3) + 
  geom_ribbon(data=N_irrigd, aes(x=yrs, ymin=lower_no3_sw /1e6,ymax=upper_no3_sw /1e6, fill='NO3_SW'),color=NA, alpha=.3) +
  geom_ribbon(data=N_irrigd, aes(x=yrs, ymin=lower_IWR_sw /1e6,ymax=upper_IWR_sw /1e6, fill='IWW'),color=NA, alpha=.3) + 
  scale_fill_manual(values = c("All"="red", "IWR_gw"="orange1", 'IWR_sw' = 'yellow1', "NO3_GW"="green","NO3_SW"="blue"),
                    labels = c('All','Irrigation water withdrawals (GW)','Irrigation water','Groundwater nitrate','Surface water nitrate')) + 
  geom_line(data=N_irrigd, aes(x=yrs, y=baseline_Nirrig/1e6, colour='baseline'),size=1.5) + 
  scale_colour_manual(values = c("baseline"="black"), labels = c('Baseline')) + 
  scale_y_continuous(limits=c(0,30), breaks=seq(0,30,5)) + 
  xlab('Years') + ylab(expression(N[irrig]~(Gg~N~yr^{-1})))+ 
  guides(fill=guide_legend(ncol=2,
                           title.position = 'top',
                           title = 'Heuristic SA'),
         colour=guide_legend(title = '')) + 
  theme_classic() + 
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        text = element_text(family='serif', size=14),
        axis.title = element_text(size=15.5),
        axis.text = element_text(size = 14),
        legend.text = element_text(size=14),
        panel.grid.major = element_line(colour = 'black', size=0.1, linetype='dotted'),
        axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))
SM_XX
ggsave(filename = './Nutrients/Output/Plots/Irrigation/Supp_materials/SM_heuristicSA_params.tiff',compress='lzw')  

# CALENDAR ---------
calendar = read.csv('./Water/Activity_data/Other_params/Tabled/Irrigation_calendars.csv')
calendar = calendar[, c('Main_crop','Crop',paste0('M',seq(1,12)))]
names(calendar) = c('Main_crop','Crop', 'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sept','Oct','Nov','Dec')
calendar = reshape2::melt(calendar, c('Main_crop','Crop'))

head(calendar)
ggplot(calendar, aes(x=variable, y=Crop, group=Main_crop, fill=value, label=value)) + 
  geom_tile() +
  #scale_fill_distiller(palette = 'Spectral', breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2)) + 
  scale_fill_viridis_c(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2)) + 
  geom_text() + 
  xlab('Month') + ylab('Crops') +
  labs(fill=expression(k[c])) + 
  theme_bw() + 
  theme(
        text = element_text(family='serif', size=14),
        axis.title = element_text(size=15.5),
        axis.text = element_text(size = 14),
        legend.text = element_text(size=14),
        panel.grid.major = element_line(colour = 'black', size=0.1, linetype='dotted'),
        axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))
ggsave(filename = './Nutrients/Output/Plots/Irrigation/Supp_materials/CropCalendar_cropKc.tiff',compress='lzw')  
