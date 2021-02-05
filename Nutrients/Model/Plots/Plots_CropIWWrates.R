source('./Nutrients/Model/Irrigation/1_Crop_irrigation_requirements.R')
source('./Nutrients/Model/Irrigation/1_CropIrrigation_GlobWat.R')

View(irrig_id)
irrig_id = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Irrigation', subfolderX2 = 'Portugal_statistics', pattern = 'Crop_ids')
tot_irrig <- data.frame(Admin_id=NULL, ID = NULL,  Muni = NULL, 
                        Volume_regions = NULL,
                        year = NULL, IWW_rate = NULL, 
                        method = NULL, main_param = NULL, param = NULL)
template = create_crop_irrigation_requirements('Cereals','Maize')[, c('Muni_ID','Volume_regions')]
i=1
for (i in 1:nrow(irrig_id)) {
  print(i)
  main_param = irrig_id[i, 'Main_crop']
  param = irrig_id[i, 'Crop']
  param = ifelse(param == 'Other_industry','Tomato',param)
  
  irrig_methods = get_crop_irrigMethods(main_param, param)
  method = 'gun'
  for (method in irrig_methods) {
    IWW = get_activity_data(module = 'Water', mainfolder = 'Output', folder = 'Crop_irrigation_withdrawals', subfolder = 'Avg_municipality', subfolderX2 = main_param, subfolderX3 = param, pattern = method)
    
    colnames(IWW)[1] = 'Muni_ID'
    IWW = plyr::join(IWW, template, 'Muni_ID')
    
    IWW = reshape2::melt(IWW, c('Muni_ID','ID','Muni', 'Volume_regions'))
    names(IWW)[c(4,5)] = c('year','IWW_rate')
    
    IWW$method = method
    IWW$main_param = main_param
    IWW$param = param
    tot_irrig = rbind(tot_irrig, IWW)
  }
}

tot_irrig = tot_irrig[which(tot_irrig$value>0), ]

# get dgadr numbers
for (i in 1:nrow(tot_irrig)) {

  main_param = tot_irrig[i,'main_param']
  param = tot_irrig[i,'param']
  method = tot_irrig[i,'method']
  id = tot_irrig[i,'Muni_ID']
  
  dgadr = create_crop_irrigation_requirements(main_param, param)[, c('Muni_ID',method)]

  tot_irrig[i,'dgadr'] = dgadr[which(dgadr$Muni_ID==id), 2]
}

data.table::fwrite(tot_irrig, './Nutrients/Output/Plots/Irrigation/Supp_materials/Crop_IWW/CropIWW_dataset.csv')

View(tot_irrig)
tot_irrig = data.table::fread('./Nutrients/Output/Plots/Irrigation/Supp_materials/Crop_IWW/CropIWW_dataset.csv')
tot_irrig = tot_irrig[-which(tot_irrig$dgadr==0), ]
tot_irrig$dif = 1-tot_irrig$value/tot_irrig$dgadr
mean(tot_irrig$dif)*100
sd(tot_irrig$dif)*100
View(tot_irrig)

require('ggplot2')
methds = c('drip', 'furrows','gun', 'microsprink', 'pivot', 'sprinkler')
store_plots = lapply(methds, function(x) {
  
  ggplot() + 
    geom_boxplot(data = subset(tot_irrig, method==x), aes(x=param, y=value, color=year)) + 
    geom_point(data = subset(tot_irrig, method==x), aes(x=param, y=dgadr,group=year), 
               colour='black',shape=3,size=3, position = position_dodge(width=0.75)) + 
    coord_flip() +
    theme_classic()  + 
    labs(colour='Region') + 
    labs(title=paste0(x, ' 1995-2017')) + 
    xlab('') + ylab(expression(Crop~irrigation~withdrawals~(m^{3}~ha^{-1}~yr^{-1}))) + 
    scale_y_continuous(limits=c(0,18000), breaks=seq(0,17500,2500)) + 
    theme(
      text = element_text(family='serif', size=14),
      axis.title = element_text(size=15.5, face = 'bold'),
      axis.text = element_text(size = 13),
      panel.grid.major.x  = element_line(colour = 'black', size=0.3, linetype='dotted'),
      axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
      axis.line.y = element_line(colour = 'black', size=1, linetype='solid'))
})
names(store_plots) = methds
dir.create(path = './Nutrients/Output/Plots/Irrigation/Supp_materials/Crop_IWW')

lapply(1:length(store_plots), function(x)
  ggsave(plot = store_plots[[x]], 
         filename = paste0('./Nutrients/Output/Plots/Irrigation/Supp_materials/Crop_IWW/', names(store_plots)[[x]],'.tiff'),
         compress='lzw', height = 9.3, width = 9.3))





