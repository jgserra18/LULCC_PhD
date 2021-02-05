source('./Nutrients/Model/Nutrient_balance/4_Spatially_explicit_balances.R')


compute_nutrient_total_leaching = function(reference_area, nutrient = 'N') {
  # leaching = Soil_balance * frac_leaching * adj_factor
  # unit: kg N-P yr-1
  
  ref = ifelse(reference_area == 'Cropland','Arable_land','Grassland')
  yrs = paste0('X',seq(1987,2017))

  for (yr in yrs) {
    
    ssnb = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Nutrient_balances', subfolder ='Soil_balance', subfolderX2 = nutrient, subfolderX3 = reference_area, subfolderX4 = 'Spatially_explicit', pattern = yr )
    #ssnb[ssnb==0] = NA # delimite based on reference_area
    ssnb[ssnb<0] = 0 # no leaching if negative
    
    adj_factor = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Reference_areas', subfolder = 'Adjusment_factor', subfolderX2 = ref, pattern = yr)
    FRAC_leaching = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'MITERRA_fractions', subfolder = 'FRAC_leaching', subfolderX2 = 'Leaching_fraction', pattern = gsub('X','',yr))
   
    # compute nutrient leaching
    nut_leaching = ssnb  * FRAC_leaching #* adj_factor

    export_file(module = 'Nutrients', 
                folder = 'Leaching', 
                subfolder = 'Total_leaching', 
                subfolderX2 = nutrient, 
                subfolderX3 = reference_area, 
                subfolderX4 = 'Spatially_explicit',
                file = nut_leaching, 
                filename = paste0('Total_leaching_', yr))
  }
}
compute_nutrient_total_leaching('Grassland')
compute_nutrient_total_leaching('Cropland')


compute_total_UAA_leaching = function(nutrient = 'N') {
  
  yrs = paste0('X',seq(1987,2017))
  clc = get_activity_data(module = 'LULCC', folder = 'CLC', subfolder = 'Native', pattern = '2000')
  for (yr in yrs) {
    
    grass = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Leaching', subfolder = 'Total_leaching', subfolderX2 = nutrient, subfolderX3 = 'Grassland', subfolderX4 = 'Spatially_explicit', pattern = yr)
    arable = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Leaching', subfolder = 'Total_leaching', subfolderX2 = nutrient, subfolderX3 = 'Cropland', subfolderX4 = 'Spatially_explicit', pattern = yr)
    
    total = mosaic(grass, arable, fun=sum)
    total = mask(crop(total, extent(clc)), clc)
    
    export_file(module = 'Nutrients', 
                folder = 'Leaching', 
                subfolder = 'Total_leaching', 
                subfolderX2 = nutrient, 
                subfolderX3 = 'UAA', 
                subfolderX4 = 'Spatially_explicit',
                file = total, 
                filename = paste0('Total_leaching_', yr))
  }
}
compute_total_UAA_leaching('N')

all = list.files(path = './Nutrients/Output/Leaching/Total_leaching/N/UAA/Spatially_explicit/', full.names = T)
all = lapply(all, raster)

df  = data.frame(yrs=seq(1987,2017), leaching = sapply(all, function(x) cellStats(x,'sum')/1e6))
library('ggplot2')
ggplot(df, aes(yrs,leaching)) + geom_line(colour='blue',size=1.5)  + scale_y_continuous(limits=c(0,60))

