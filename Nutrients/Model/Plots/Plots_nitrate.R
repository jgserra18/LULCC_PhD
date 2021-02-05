
source('./Main/Global_functions.R')

require(tmap)
require(raster)
library(rworldmap)
library(rworldxtra)


# PAPER PLOTS ----------------------------------------------------------------------------------------

# prepare main plot data

world <- getMap(resolution='high')
sb <- subset(world, ADMIN== 'Spain' | ADMIN=='Portugal')
pt <- subset(world, ADMIN=='Portugal')


data = raster('C:\\Users\\serrm\\Downloads\\GEBCO_2020_04_Dec_2020_9fbffc72b8e3\\GEBCO_2020_04_Dec_2020_9fbffc72b8e3\\gebco_2020_n43.453125000000014_s35.718750000000014_w-13.4384765625_e-5.4404296875.tif')
bath = reclassify(data, rcl=c(0, +Inf, NA))
elev = reclassify(data, rcl=c(-Inf, 0, NA))

downscale = F


# GW
no3_gw = list.files(path = './Nutrients/Output/Irrigation/Nitrate_modelling/GW/Spatial_prediction/', full.names = T)
no3_gw = brick(lapply(no3_gw, raster))
#d = cv(no3_gw)
no3_gw_trend = (no3_gw[[nlayers(no3_gw)]]-no3_gw[[5]])/(2017-2000)
no3_gw_avg = calc(no3_gw, fun=mean) # average for 1995-2017
tm_shape(no3_gw_trend) + tm_raster(breaks=c(-Inf,-1,-0.5,0,0.5,1,+Inf), palette = c('blue1','green1','yellow1','orange1','red1'))

p1 =  tm_shape(bath, projection = "+proj=longlat",raster.downsample = downscale) + 
  tm_raster(legend.show = F, contrast=c(0.3,1),n=20,palette = 'Blues') + 
  tm_shape(elev,  projection = "+proj=longlat",raster.downsample = downscale) +
  tm_raster(palette = 'Greys', alpha = 0.65, n=20, contrast=c(0.3,1), legend.show = F) +
  tm_shape(no3_gw_avg, projection = "+proj=longlat", raster.downsample = downscale) + 
  tm_raster(breaks=c(0,10,25,50,+Inf), 
            palette = c('blue1','green1','yellow1','red1'), 
            labels = c('< 10','10 - 25','25 - 50','> 50'),
            title  = 'Nitrate\nconcentration\n(mg/L)'
            ) + 
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
            outer.margins = 0.01,
            panel.label.size = 0.8,
            panel.labels = 'Groundwater')  +
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.85, 
               position=c(0.05, 0.02), breaks = c(0, 50, 100, 200)) 


# SW

no3_sw = list.files(path = './Nutrients/Output/Irrigation/Nitrate_modelling/SW/Spatial_prediction/', full.names = T)
no3_sw = brick(lapply(no3_sw, raster))
no3_sw = no3_sw[[9:nlayers(no3_sw)]]
no3_sw_trend = (no3_sw[[nlayers(no3_sw)]]-no3_sw[[1]])/(2017-1995) * 50/11.3
no3_sw_avg = calc(no3_sw, fun=mean)
no3_sw_avg = no3_sw_avg * 50/11.3
tm_shape(no3_sw_trend) + tm_raster(style='cont',palette = '-Spectral')


box = c(xmin=2635890, ymin=1729732 , xmax = 2977390 , ymax = 2298232)

hydro_shed = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Watershed')


p2 =  tm_shape(bath, projection = "+proj=longlat",raster.downsample = downscale) + 
  tm_raster(legend.show = F, contrast=c(0.3,1),n=20,palette = 'Blues') + 
  tm_shape(elev,  projection = "+proj=longlat",raster.downsample = downscale) +
  tm_raster(palette = 'Greys', alpha = 0.65, n=20, contrast=c(0.3,1), legend.show = F) +
  tm_shape(no3_sw_avg, projection = "+proj=longlat", raster.downsample = downscale) + 
  tm_raster(breaks=c(0,10,20,30,40,50),
            palette = c('blue1','green1','yellow1','red1'), 
            labels = c('< 10','10 - 20','20 - 30','30 - 40', '40 - 50'),
            title  = 'Nitrate\nconcentration\n(mg/L)'
  ) +
  tm_shape(hydro_shed, projection = "+proj=longlat") + tm_borders(col='black',lwd=0.5) +
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
            outer.margins = 0.01,
            panel.label.size = 0.8,
            panel.labels = 'Surface water')  +
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.85, 
               position=c(0.05, 0.02), breaks = c(0, 50, 100, 200)) 

mplot <- tmap_arrange(p1,p2, ncol=2, nrow=1)
tmap_save(mplot, filename = './Nutrients/Output/Plots/Irrigation/GW_SW_NO3.jpeg', dpi=1000,height = 4, width = 6)




# EXPORT PLOTS ---------------------------------------------------------------------------------------


#  surface water ------------------------------------------------------------------
source('./Nutrients/Model/Irrigation/Preprocessing/3_Preprocessing_SW_1987_2017.R')
source('./Nutrients/Model/Irrigation/4_Nitrate_in_surfacewater.R')

hydro_shed = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Hydroshed')
watershed_SW = compile_SW_nitrate_Watershed()
yrs = paste0(seq(1995,2017))

for (y in yrs) {
  
  SW_NO3_yr = spatial_interpolation_over_watershed(y, watershed_SW)
  
  plot = tm_shape(SW_NO3_yr[[1]]) + tm_raster(breaks =  c(0,2,4,6,8,10,11.3,+Inf), palette = c('blue1','green1','yellow1','orange1','red1'), alpha=0.5, title ='Watershed') + 
    tm_shape(SW_NO3_yr[[2]]) + tm_dots(col=paste0('X', y), size = 0.5, breaks =  c(0,2,4,6,8,10,11.3,+Inf), palette = c('blue1','green1','yellow1','orange1','red1'), title = 'Station') + 
    tm_shape(hydro_shed) + tm_borders(col='black',lwd=1) 
  tmap_save(tm = plot, filename = paste0('./Nutrients/Output/Plots/Irrigation/Nitrate_SW/SW_', y, '.pdf'), dpi=300)
}


# groundwater ----------------------------------------------------------------------------------------------

source('./Nutrients/Model/Irrigation/Preprocessing/2_Preprocessing_1987_2016.R')
source('./Nutrients/Model/Irrigation/Preprocessing/1_Preprocessing_2017.R')


no3_gw = list.files(path = './Nutrients/Output/Irrigation/Nitrate_modelling/GW/Spatial_prediction/', full.names = T)
no3_gw = stack(lapply(no3_gw, raster))
ctr = 1995

for (file in 1:nlayers(no3_gw)) {
  ctr = ctr + 1  
  gw_now = no3_gw[[file]]
  
  plot = tm_shape(gw_now) + tm_raster(breaks =  c(0,10,25,50,+Inf), palette = c('blue1','green1','yellow1','red1'),  title ='mg NO3 L-1')
  tmap_save(tm = plot, filename = paste0('./Nutrients/Output/Plots/Irrigation/Nitrate_GW//GW_', ctr, '.pdf'), dpi=300)
}


require(ggplot2)
library(cowplot)

d = no3_gw[[5]]
d = raster('G:\\O meu disco\\IrrigatioN_EU\\Output\\NO3_concentrations/RF_GW_NO3.tif')
d = as.data.frame(d, xy=T, na.rm=T)
names(d)[3] = 'no3'
d$cuts = cut(d$no3, breaks=c(0,10,25,50,+Inf))
d = na.omit(d)


p = ggplot(d, aes(x=x, y=y, fill=cuts)) + geom_raster(na.rm=T)  + 
  scale_fill_manual(values=c('blue1','green1','yellow1', 'red1'), drop=F, 
                    labels = c('< 10','10 - 25','25 - 50','> 50'),
                    guide = guide_legend(
                      direction = "horizontal",
                      title.position = 'top',
                      # I shift the labels around, the should be placed 
                      # exactly at the right end of each legend key
                      title.hjust = 0.5,
                      label.hjust = 1,
                      nrow = 1,
                      byrow = T,
                      # also the guide needs to be reversed
                      reverse = F,
                      label.position = "bottom"
                    )) + 
  theme(legend.position="bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.title=element_blank() 
  )
p

require('ggExtra')

zonal_y <- aggregate(d[,3], list(d$y), mean)
names(zonal_y) = c('y','no3')
zonal_y$cuts =  cut(zonal_y$no3,breaks=c(0,10,25,50,+Inf))

ggzonal_y <- ggplot(zonal_y, aes(x=no3, y=y, colour=cuts,fill=cuts)) + 
  geom_point() + 
  theme(legend.position="none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
  ) + theme_nothing() + scale_colour_manual(values=c('blue1','green1','yellow1','red1'))
ggzonal_y



zonal_x <- aggregate(d[,3], list(d$x), mean)
names(zonal_x) = c('x','no3')
zonal_x$cuts =  cut(zonal_x$no3,breaks=c(0,10,25,50,+Inf))

  

ggzonal_x <- ggplot(zonal_x, aes(x=x, y=no3, colour=cuts,fill=cuts)) + 
  geom_point() + 
  theme(legend.position="none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
  ) + theme_nothing() + 
  scale_colour_manual(values=c('blue1','green1','yellow1','red1'))

ggzonal_x

grobs <- ggplotGrob(p1 + theme(legend.position="bottom"))$grobs
legend_b <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
prow <- plot_grid(plotlist = list(p1, ggzonal_y,align = c('h','v'), rel_widths = c(5, 1))
plot_grid(prow