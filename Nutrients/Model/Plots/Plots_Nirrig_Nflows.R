source('./Nutrients/Model/Nutrient_balance/1_Prepare_parameters.R')

require('dplyr')
require('ggplot2')

yrs = paste0('X',seq(1995,2017))

# inputs --

dep_crop = compute_total_atmN_muni(reference_area = 'Cropland')
dep_crop = dep_crop[, c('Muni_ID','ID','Muni',yrs)]
dep_grass = compute_total_atmN_muni(reference_area = 'Grassland')
dep_grass = dep_grass[, c('Muni_ID','ID','Muni',yrs)]
dep_total =dep_crop
dep_total[, yrs] = sapply(yrs, function(x) round(dep_grass[,x]+dep_crop[,x],1))


bnf = read.csv('./Nutrients/Output/BNF/N/Total/Total_sum.csv')
bnf = bnf[, c('Muni_ID','ID','Muni',yrs)]

gross_man = read.csv('./Nutrients/Output/Gross_manure/N/Total_Nexcretion/Total/Total_sum.csv')
gross_man = gross_man[, c('Muni_ID','ID','Muni',yrs)]

fert = read.csv('./Nutrients/Output/Fertilisation/N/Inorganic_fertiliser/Method 1/Without_ManSurplus/Total/Adjusted/Adjusted_fert_mainland.csv')
fert = fert[, c('Muni_ID','ID','Muni',yrs)]

N_irrig = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Avg_municipality_ABS.csv')
N_irrig = N_irrig[, c('Muni_ID','ID','Muni',yrs)]

# outputs ---
N_crop= read.csv('./Nutrients/Output/Crop_offtake/N/Total/Total_sum.csv') # crops + fodder
N_crop = N_crop[, c('Muni_ID','ID','Muni',yrs)]
#N_crop[, yrs] = sapply(yrs, function(x) N_crop[,x]*-1) # turn negative for the plot


compute_all_N_flows_mainland = function() {
  #' @describeIn computes mainland N flows f
  #' unit: Gg N yr-1 (ktonnes N)
  
  store_params = list(dep_total,bnf, gross_man, fert, N_irrig, N_crop)
  names(store_params) = c('Atmospheric deposition','BNF','Gross manure','Inorganic fertiliser','Irrigation','N harvested')
  
  store = data.frame(yrs=NULL, N_flow = NULL, param = NULL)

  for (i in 1:length(store_params)) {
    
    nflow_file = store_params[[i]]
    nflow_file = sapply(yrs, function(x) sum(nflow_file[, x], na.rm=T)/1e6)
    
    nflow_df = data.frame(yrs = seq(1995,2017), N_flow = nflow_file, param = names(store_params)[i])
    store = rbind(store, nflow_df)
  }
  return(store)
}

all_flows= compute_all_N_flows_mainland()

# mainland N_irrig (1995-2017) plot -----------

irrig_df = subset(all_flows, param =='Irrigation')
lm(N_flow~yrs, irrig_df)
p1 = ggplot(irrig_df, aes(x=yrs, y=N_flow)) + 
  geom_line(colour='greenyellow',size=1.5,alpha=1) + 
  geom_smooth(method = 'lm', se=T, formula=y~x, fill='azure4', colour='black') +
  annotate(geom='text',x=2010,y=10, label='y = -0.05x + 113.14', color='black', family='serif', size = 4) + 
  theme_bw() + 
  scale_fill_viridis_d() + 
  xlab('') + ylab(expression(N[Irrig]~(Gg~N~yr^{-1}))) + 
  labs(fill='N flows') +
  scale_y_continuous(expand=c(0,0), limits=c(0,21), breaks=seq(0,20,5)) + 
  scale_x_continuous(expand=c(0,0), breaks=c(1995,2000,2005,2010,2015)) + 
  theme(legend.position = 'none',
    text = element_text(family='serif', size=14),
    axis.title = element_text(size=15.5),
    axis.text = element_text(size = 13)) 
p1  

# with SA range

upper25 = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Heuristic_SA/Mod1.25_All_municipality.csv')
upper25 = colSums(upper25[, yrs])/1e6

lower25 = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Heuristic_SA/Mod0.75_All_municipality.csv')
lower25 = colSums(lower25[, yrs])/1e6

irrig_df$N_flow_upper25 = upper25
irrig_df$N_flow_lower25 = lower25
irrig_df

p1_support = ggplot(irrig_df, aes(x=yrs,y=N_flow)) + 
  geom_line(colour='greenyellow',size=1.5,alpha=1) + 
  annotate(geom='text',x=2010,y=10, label='y = -0.05x + 113.14', color='black', family='serif', size = 4) + 
  geom_ribbon(aes(ymin=N_flow_lower25, ymax=N_flow_upper25), colour=NA,fill='blue',alpha=0.25)+ 
  geom_line(colour='blue',size=1.5,alpha=1) + 
  geom_smooth(method = 'lm', se=F, formula=y~x, fill='azure4', colour='black') + 
  scale_fill_viridis_d() + 
  xlab('') + ylab(expression(N[Irrig]~(Gg~N~yr^{-1}))) + 
  labs(fill='N flows') +
  scale_y_continuous(expand=c(0,0), limits=c(0,30.5), breaks=seq(0,30,5)) + 
  scale_x_continuous(breaks=c(1995,2000,2005,2010,2015)) + 
  theme_classic() + 
  theme(legend.position = 'none',
        text = element_text(family='serif', size=14),
        axis.title = element_text(size=15.5),
        axis.text = element_text(size = 13),
        panel.grid.major = element_line(colour = 'black', size=0.1, linetype='dotted'),
        axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))
p1_support




# quantile plot ----------------------------------

N_irrig = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Avg_municipality_UAA.csv')  
store = data.frame(yr=NULL,min=NULL,f25=NULL,f50=NULL,f75=NULL,f100=NULL, avg = NULL, sd=NULL)

# for qhantile plot
ctr = 1
for (yr in yrs) {
  sb = N_irrig[, c('Muni_ID','Muni','ID', yr)]
  store[ctr, 'yr'] = yr
  store[ctr,c('min','f25','f50','f75','f100')] = quantile(sb[, yr])
  store[ctr, 'avg'] = mean(sb[,yr])
  store[ctr,'sd'] = sd(sb[,yr])
  ctr = ctr + 1  
  
}
store$yr = gsub('X','',store$yr)
store$yr = as.numeric(store$yr)
store_m = reshape2::melt(store, 'yr')

N_irrig = reshape2::melt(N_irrig, c('Muni_ID','Muni','ID')) # for boxplot
N_irrig$variable = gsub('X', '',N_irrig$variable)


p3 = ggplot() + 
  geom_boxplot(data=N_irrig, aes(x=as.numeric(variable), y=value, group=variable),color='black',fill='blue',alpha=.7) + 
  #geom_line(data=subset(store_m, variable=='f100'), aes(x=yr, y=value), color='blue1',alpha=.7, show.legend = F) + 
  xlab('') + ylab(expression(N[Irrig]~(kg~N~ha^{-1}~yr^{-1}))) + 
  labs(colour='') +
  scale_y_continuous(limits=c(0,105), breaks=seq(0,100,10), expand=c(0,0)) + 
  scale_x_continuous(breaks=c(1995,2000,2005,2010,2015)) + 
  theme_classic()+ 
  theme(
    text = element_text(family='serif', size=14),
    axis.title = element_text(size=15.5),
    axis.text = element_text(size = 13),
    panel.grid.major = element_line(colour = 'black', size=0.1, linetype='dotted'),
    axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))
p3


# N flows plot ---------------------------

UAA = read.csv('./Nutrients/Output/Reference_areas/UAA/UAA.csv')
UAA = UAA[,c('Muni_ID','ID','Muni',paste0('X',seq(1995,2017)))]
UAA = reshape2::melt(UAA, c('Muni_ID','ID','Muni'))

UAA = UAA %>%
  group_by(variable) %>%
  summarise(UAA=sum(value))

all_flows[which(all_flows$param=='N harvested'), 'N_flow'] = all_flows[which(all_flows$param=='N harvested'), 'N_flow'] * -1
GNB = all_flows %>%
  group_by(yrs) %>%
  summarise(N_flow = sum(N_flow))
GNB$param = 'GNB'
cor.test(GNB$yrs, GNB$N_flow, method='spearman')

all_flows = rbind(all_flows,GNB)
last_yr = subset(all_flows, yrs == 2017)
last_yr[1,3] = 'Deposition'
all_flows[which(all_flows$param=='Atmospheric deposition'), 'param'] = 'Deposition'


p2= ggplot() + 
  geom_line(data= all_flows, aes(x=yrs, y=N_flow,  colour=param), size=1.8, show.legend = F) + 
  geom_point(data=last_yr, aes(x=yrs,y=N_flow, colour=param), size=4, show.legend = F) + 
  scale_colour_manual(values = c('Deposition'='azure4',
                                 'BNF' = 'orange',
                                 'GNB' = 'black',
                                 'Gross manure'='burlywood4',
                                 'Inorganic fertiliser' = 'red1',
                                 'Irrigation' = 'blue',
                                 'N harvested' = 'chartreuse4')) + 
  xlab('') + ylab(expression(N~flows~(Gg~N~yr^{-1}))) + 
  scale_y_continuous(expand=c(0,0), limits=c(0,230), breaks=seq(0,225,50)) + 
  scale_x_continuous(breaks=c(1995,2000,2005,2010,2015), limits=c(1995,2017)) + 
  theme(
    text = element_text(family='serif', size=14),
    axis.title = element_text(size=15.5),
    axis.text = element_text(size = 13))
p2 = p2 + 
  geom_text_repel(data=last_yr, aes(x=2017,y=N_flow, label=param, colour=param),
                  size=4, 
                  family='serif',
                  show.legend = F,
                  hjust=-0.05,
                  vjust=0.5,  
                  max.time = 15,
                  max.iter = 1e8,
                  direction='y',force = 15,
                  max.overlaps = Inf,
                  box.padding = -0.1,
                  nudge_x = 1,
                  point.padding = 0, 
                  xlim = c(-Inf,+Inf), 
                  ylim = c(-Inf,+Inf)) + 
  scale_colour_manual(values = c('Deposition'='azure4',
                                 'BNF' = 'orange',
                                 'GNB' = 'black',
                                 'Gross manure'='burlywood4',
                                 'Inorganic fertiliser' = 'red1',
                                 'Irrigation' = 'blue',
                                 'N harvested' = 'chartreuse4')) + 
  coord_cartesian(clip = "off")  + 
  theme_classic()  + 
  theme(
    text = element_text(family='serif', size=14),
    axis.title = element_text(size=15.5),
    axis.text = element_text(size = 13),
    plot.margin = margin(t = 0.1, r = 4.3, b = 0.1, l = 0.1, "cm"),
    panel.grid.major = element_line(colour = 'black', size=0.1, linetype='dotted'),
    axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
    axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))
p2

require('ggpubr')
all = ggarrange(ggarrange(p1_support,p3, ncol=2, nrow=1, labels=c('A','B'), font.label = list(family='serif')), 
                p2, nrow=2, labels=c('','C'), font.label = list(family='serif'), heights = c(1,1.5))
all
ggsave(all, filename = './Nutrients/Output/Plots/Irrigation/N_flows_1995_2017_v3.jpeg',dpi=600, height = 7.8, width = 7.6)
ggsave(all, filename = './Nutrients/Output/Plots/Irrigation/N_flows_1995_2017_v3.tiff',dpi=600, height = 7.8, width = 7.6, compress='lzw')


p2

# GISplot 1995,2005,2015 UAA ----------

library(rworldmap)
library(rworldxtra)
library(tmap)

downscale=F
world <- getMap(resolution='high')
sb <- subset(world, ADMIN== 'Spain' | ADMIN=='Portugal')
pt <- subset(world, ADMIN=='Spain')

data = raster('C:\\Users\\serrm\\Downloads\\GEBCO_2020_04_Dec_2020_9fbffc72b8e3\\GEBCO_2020_04_Dec_2020_9fbffc72b8e3\\gebco_2020_n43.453125000000014_s35.718750000000014_w-13.4384765625_e-5.4404296875.tif')
bath = reclassify(data, rcl=c(0, +Inf, NA))
elev = reclassify(data, rcl=c(-Inf, 0, NA))


N_irrig = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Avg_municipality_UAA.csv')
N_irrig = N_irrig[, c('Muni_ID','ID','Muni','X1995','X2000','X2005','X2010','X2015','X2017')]
names(N_irrig)[1] = 'Admin_id'

muni = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Municipality')
muni = merge(muni, N_irrig[, c('Admin_id','X1995','X2000','X2005','X2010','X2015','X2017')])
muni$annual_var = round((muni$X2017-muni$X1995)/(2017-1995), 2)

p1 = tm_shape(bath, projection = "+proj=longlat",raster.downsample = downscale) + 
  tm_raster(legend.show = F, contrast=c(0.3,1),n=20,palette = 'Blues') + 
  tm_shape(elev,  projection = "+proj=longlat",raster.downsample = downscale) +
  tm_raster(palette = 'Greys', alpha = 0.65, n=20, contrast=c(0.3,1), legend.show = F) +
  tm_shape(muni) + tm_borders(col='black', lwd = 0.5) + 
  tm_polygons(col=c('X1995','X2005','X2015'),
              breaks=c(0,1,5,10,25,50,100), 
              labels = c('< 1','1 - 5','5 - 10','10 - 25','25 - 50','50 - 100'),
              palette='YlOrBr', title = 'Irrigation N\n(kg N/ha/yr)') + 
  tm_facets(nrow=1) + 
  tm_shape(pt, projection = "+proj=longlat") + tm_borders(col='black',lwd=0.5) +
  tm_graticules(projection = '+proj=longlat',x = c(-11,-6), y=c(36.5,42)) +
  tm_layout(legend.position = c(0.1,0.45),
            legend.outside = F,
            legend.bg.color = 'white',
            legend.frame = T,
            frame = T,
            # legend.height = 0.45,
            legend.width = 0.3,
            legend.text.size = 0.7,
            legend.title.size = 0.9,
            bg.color = 'gray',
            fontfamily = 'serif',
            outer.margins = 0.01,
            panel.labels = c('1995','2005','2015'),
            panel.label.size = 0.9)  +
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.85, 
               position=c(0.05, 0.02), breaks = c(0, 50, 100, 200))

p2 = tm_shape(bath, projection = "+proj=longlat",raster.downsample = downscale) + 
  tm_raster(legend.show = F, contrast=c(0.3,1),n=20,palette = 'Blues') + 
  tm_shape(elev,  projection = "+proj=longlat",raster.downsample = downscale) +
  tm_raster(palette = 'Greys', alpha = 0.65, n=20, contrast=c(0.3,1), legend.show = F) +
  tm_shape(muni) + tm_borders(col='black', lwd = 0.5) + 
  tm_polygons(col=c('annual_var'),
              breaks=c(-Inf,-0.5,0,0.5,+Inf), 
              labels = c('< -0.5','-0.5 - 0','0 - 0.5','> 0.5'),
              palette=c('blue1','green1','yellow1','red1'),
              title = 'Annual\nvariation\n(kg N/ha/yr)') + 
  tm_shape(pt, projection = "+proj=longlat") + tm_borders(col='black',lwd=0.5) +
  tm_graticules(projection = '+proj=longlat',x = c(-11,-6), y=c(36.5,42)) +
  tm_layout(legend.position = c(0.1,0.45),
            legend.outside = F,
            legend.bg.color = 'white',
            legend.frame = T,
            frame = T,
            # legend.height = 0.45,
            legend.width = 0.3,
            legend.text.size = 0.7,
            legend.title.size = 0.9,
            bg.color = 'gray',
            fontfamily = 'serif',
            outer.margins = 0.01,
            panel.labels = 'Annual variation',
            panel.label.size = 0.9)  +
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.85, 
               position=c(0.05, 0.02), breaks = c(0, 50, 100, 200))

all = tmap_arrange(p1,p2, widths=c(0.75,0.25), outer.margins = 0.02)
all = tmap_arrange(p1,p2, widths=c(0.75,0.25), outer.margins = 0.02)
tmap_save(all, filename = './Nutrients/Output/Plots/Irrigation/Nirrig_1995_2005_2015_annual_var_1x4.jpeg', dpi=1000,height = 4, width = 12.2)



# TIA !!! ------------

N_irrig1 = read.csv('./Nutrients/Output/Irrigation/Irrigation_N/Total/Average_municipality/Avg_municipality_TIA.csv')
N_irrig1 = N_irrig1[, c('Muni_ID','ID','Muni','X1995','X2000','X2005','X2010','X2015','X2017')]
names(N_irrig1)[1] = 'Admin_id'


muni = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Municipality')
muni = merge(muni, N_irrig1[, c('Admin_id','X1995','X2000','X2005','X2010','X2015','X2017')])
muni$annual_var = round((muni$X2017-muni$X1995)/(2017-1995), 2)
downscale=T
p1 = tm_shape(bath, projection = "+proj=longlat",raster.downsample = downscale) + 
  tm_raster(legend.show = F, contrast=c(0.3,1),n=20,palette = 'Blues') + 
  tm_shape(elev,  projection = "+proj=longlat",raster.downsample = downscale) +
  tm_raster(palette = 'Greys', alpha = 0.65, n=20, contrast=c(0.3,1), legend.show = F) +
  tm_shape(muni) + tm_borders(col=NA) + 
  tm_polygons(col=c('X1995','X2005','X2015'),
              breaks=c(0,10,25,50,75,+Inf), 
              palette='YlOrBr', title = 'Irrigation N\n(kg N/ha/yr)') + 
  tm_facets(nrow=1) + 
  tm_shape(pt, projection = "+proj=longlat") + tm_borders(col=NA) +
  tm_graticules(projection = '+proj=longlat',x = c(-11,-6), y=c(36.5,42)) +
  tm_layout(legend.position = c(0.1,0.45),
            legend.outside = F,
            legend.bg.color = 'white',
            legend.frame = T,
            frame = T,
            # legend.height = 0.45,
            legend.width = 0.3,
            legend.text.size = 0.7,
            legend.title.size = 0.9,
            bg.color = 'gray',
            fontfamily = 'serif',
            outer.margins = 0.01,
            panel.labels = c('1995','2005','2015'),
            panel.label.size = 0.9)  +
  tm_scale_bar(color.dark = 'black', text.color = 'black', text.size = 0.85, 
               position=c(0.05, 0.02), breaks = c(0, 50, 100, 200))
p1
l = list.files(path = './Nutrients/Output/Irrigation/Nitrate_modelling/GW/Spatial_prediction/', pattern='.tif',full.names = T)
l = lapply(l, raster)
l = stack(l) 
plot(l[[11]]>50)
