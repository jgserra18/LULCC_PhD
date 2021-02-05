source('./Water/Model/Scraping/1_Monthly_climaticParams.R')

msk = get_activity_data(module = 'LULCC', folder = 'Admin', pattern = 'Municipality')


TerraClim_annualRunoff = function(var='q', year) {
  #' @description preprocesses TerraClim monthly runoff data for a given year into annual data
  #' @unit mm yr-1
  
  start = paste0(year,'-01-01')
  end = paste0(year, '-12-01')
 
  q = getTerraClim(pt,var, start, end)
  q = sum(q[[1]])
  q = projectRaster(q, 
                    crs=CRS('+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs '))
  q = mask(crop(q, extent(msk)), msk)
  
  return(q)
}

find_year = function(year) {
  #' @description finds the year index for the annual modelled runoff 1995-2017
  #yrs = seq(1995,2017)
  return(which(seq(1995,2017)==year))
}

annualRunoff_Comparison = function(year) {
  #' @description stacks into a dataframe TerraClim and the modelled runoff for a year
  
  print(paste0('Stacking Year : ', year, '......'))
  qclim = TerraClim_annualRunoff('q',year)
  qmod = stack('./Water/Output/GlobWat/Annual/Runoff.grd', bands=find_year(year))
  
  df = stack(qclim, qmod)
  df = as.data.frame(df, na.rm=T)
  names(df) = c('TerraClim','Modelled')
  df$year = as.character(year)
  df$R2 = summary(lm(TerraClim~Modelled, df))$r.squared
  
  return(df)
  rm(list=c('qclim','qmod'))
}


stack_annualRunoff_Comparison = function() {
  
  yrs = seq(1995,2017)
  df = data.frame(TerraClim=NULL, Modelled=NULL, year = NULL, R2=NULL)
  store = lapply(yrs, function(x) annualRunoff_Comparison(x))
  store = data.table::rbindlist(l = store)
  
  dir.create(path = './Water/Output/DataComparison/Runoff', recursive = T)
  data.table::fwrite(x = store, file = './Water/Output/DataComparison/Runoff/Dataset.csv')
  gc()
}

dataset = data.table::fread('./Water/Output/DataComparison/Runoff/Dataset.csv')
head(dataset)
library('ggplot2')


ggplot(dataset, aes(x=Modelled,y=TerraClim)) + 
  geom_hex(bins=25, color='azure4') +
  scale_fill_fermenter(palette = 'YlGnBu', direction = 1, breaks=c(0, 50,100, 500, 1000, 2500,5000,7500, 10000)) + 
  geom_smooth(method='lm',color='blue1', size = 1.5) + 
  geom_abline(intercept = 0, slope = 1, color='black', size=1.5) + 
  xlab('Modelled runoff 1995-2017 (mm per year)') + 
  ylab('TerraClimate runoff 1995-2017 (mm per year)') + 
  theme_classic() + 
  theme(
        text = element_text(family='serif', size=14),
        axis.title = element_text(size=15.5),
        axis.text = element_text(size = 14),
        legend.text = element_text(size=14),
        axis.ticks.length = unit(0.2, 'cm'),
        panel.grid.major = element_line(colour = 'black', size=0.4, linetype='dotted'),
        axis.line.x = element_line(colour = 'black', size=1, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=1, linetype='solid')
        )

mean(abs(dataset$Modelled-dataset$TerraClim))
sd(abs(dataset$Modelled-dataset$TerraClim))

summary(lm(TerraClim~Modelled, dataset))$r.squared
ggsave(filename = './Nutrients/Output/Plots/Irrigation/Supp_materials/Runoff_comparison.tiff',compress='lzw',dpi=600)


