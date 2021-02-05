source('./Main/Global_functions.R')
source('./Main/General_GIS.R')

library(doParallel)



# export animal N excretion for each param and year ------------------------------------------------------

stack_rasterize_annual_animal_population = function() {
  
  cl <- makePSOCKcluster(3)
  registerDoParallel(cl)
  
  yrs = paste0('X', seq(1987,2017))
  standard_params <- get_standard_params_list(main_param = 'Animals')
  main_params = unique(standard_params[, 2])
  store = list()
  
  for (yr in yrs) {
    
    for (animal in main_params) {

      
      N_exc = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Gross_manure', subfolder = 'N', subfolderX2 = 'Total_Nexcretion', subfolderX3 = 'Total', pattern = animal)
      N_exc = N_exc[, c('Muni_ID', yr)]
      names(N_exc)[1] = 'Admin_id'
      N_exc = fasterize_admin_statistical(module = 'LULCC', admin_res = 'Municipality', file = N_exc, name_id = 'Admin_id', name_field = yr, spatial_res = 500)
      
      export_file(module = 'Nutrients', folder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Irrigation', subfolderX3 = 'Other_exploratory_parameters', subfolderX4 = yr, subfolderX5 = 'N_excretion',
                  file = N_exc, filename = animal)
    }
  }
  stopCluster(cl)
  rm(list=c('N_exc'))
}


# export crop N fertiliser N and crop N offtake ------------------------------

source('./Nutrients/Model/Crop_production/get_fodder_crops_residues.R')


stack_rasterize_main_crop_N_offtake = function() {
  
  cl <- makePSOCKcluster(3)
  registerDoParallel(cl)
  
  yrs = paste0('X', seq(1987,2017))
  standard_params <- get_standard_params_list(main_param = 'Crops')
  main_params = unique(standard_params[, 'Main_crop'])

  for (yr in yrs) {
    
    for (main_param in main_params) {
      
        crop_offtake = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Crop_offtake', subfolder = 'N', subfolderX2 = 'Total', pattern = main_param)
        crop_offtake = crop_offtake[, c('Muni_ID', yr)]
        names(crop_offtake)[1] = 'Admin_id'
        crop_offtake = fasterize_admin_statistical(module = 'LULCC', admin_res = 'Municipality', file = crop_offtake, name_id = 'Admin_id', name_field = yr, spatial_res = 500)
        export_file(module = 'Nutrients', folder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Irrigation', subfolderX3 = 'Other_exploratory_parameters', subfolderX4 = yr, subfolderX5 = 'N_offtake',
                    file = crop_offtake, filename = main_param)
        rm(list=c('crop_offtake'))
      }
    }
  stopCluster(cl)
}


stack_rasterize_total_fertiliser_N = function() {
  
  cl <- makePSOCKcluster(3)
  registerDoParallel(cl)
  
  yrs = paste0('X', seq(1987,2017))
  fert = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Fertilisation', subfolder = 'N', subfolderX2 = 'Inorganic_fertiliser', subfolderX3 = 'Method 1', subfolderX4 = 'Without_ManSurplus', subfolderX5 = 'Total',subfolderX6 = 'Adjusted', pattern = 'Adjusted')
  
  for (yr in yrs) {
    
    yr_fert = fert[, c('Muni_ID', yr)]
    names(yr_fert)[1] = 'Admin_id'
    yr_fert = fasterize_admin_statistical(module = 'LULCC', admin_res = 'Municipality', file = yr_fert, name_id = 'Admin_id', name_field = yr, spatial_res = 500)
    export_file(module = 'Nutrients', folder = 'Activity_data', subfolder = 'General_params', subfolderX2 = 'Irrigation', subfolderX3 = 'Other_exploratory_parameters', subfolderX4 = yr, subfolderX5 = 'N_fertiliser',
                file = yr_fert, filename = yr)
  }
  stopCluster(cl)
}

