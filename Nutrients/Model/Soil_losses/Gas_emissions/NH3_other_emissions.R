


## GRAZING NH3 EMISSIONS  ------------------------------------------------------------------------------------

source('./Nutrients/Model/MMS/Grazing/Grazing_Nemissions.R')

loop_grazing_NH3_emissions()




## MANURE CROP APPLICATION NH3 EMISSIONS ---------------------------------------------------------------------





















## CROP RESIDUES BURNT ON-SITE NH3 EMISSIONS -----------------------------------------------------------------

source('./Nutrients/Model/Crop_production/Compute_crop_residues.R')

set_NH3_EF_CropResiduesBurnt = function() {
  # rom EMEP 2019
  # unit: kg N-NH3 kg DM-1
  
  EF_NH3 = 0.0024 * 17/17
  
  return(EF_NH3)
}



compute_crop_residue_burnt_NH3 = function(main_param, param, nutrient = 'N') {
  # computes crop residue burnt NH3 emissions according to EMEP 2019
  # unit: kg N-NH3 yr-1
  
  ef = set_NH3_EF_CropResiduesBurnt()
  res_burnt_N = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Crop_residues', subfolder = 'Burnt', subfolderX2 = 'N', subfolderX3 = main_param, pattern = param)
  
  if (main_param == 'Cereals') {
    FRAC_res_N = get_residues_nutrient_content('N', param, 'AG')
  }
  else {
    PC_Nres = get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'Crops', subfolderX3 = 'Residues',pattern = 'PermaCrops_Ncontent')
    FRAC_res_N = find_crop_variable(df = PC_Nres, param_col = 'crop', param = param, var = 'N_res')
  }
  
  # compute biomass burnt
  # unit: kg DM 
  yrs = paste0('X',seq(1987,2017))
  res_burnt_DM = res_burnt_N
  res_burnt_DM[, yrs] = sapply(yrs, function(x) round(res_burnt_N[, x] / FRAC_res_N, 1))
  
  # compute NH3 emissions
  res_burnt_NH3 = res_burnt_DM
  res_burnt_NH3[, yrs] = sapply(yrs, function(x) round(res_burnt_DM[, x] * ef, 1))
  
  return(res_burnt_NH3)
  rm(list=c('ef','res_burnt_N','FRAC_res_N','yrs','res_burnt_DM'))
}



compute_all_crop_residues_burnt_NH3 <- function() {
  
  main_crops <- c('Cereals','Vineyard', 'Olive_grove','Citrus','Dried_fruits','Fresh_fruits')
  standard_params <- get_standard_params_list(main_param = 'Crops')
  
  
  for (i in main_crops) {
    
    crops <- standard_params[which(standard_params$Main_crop==i), 'Crop']
    
    for (j in crops) {
      
      crop_res_burnt_NH3 = compute_crop_residue_burnt_NH3(main_param = i, param = j)
      export_file(module = 'Nutrients', 
                  file = crop_res_burnt_NH3, 
                  filename = j, 
                  folder = 'Gas_N_emissions', 
                  subfolder = 'NH3', 
                  subfolderX2 = 'Crop_residues_burnt',
                  subfolderX3 = i)
    }
  }
}


## BIOSOLID FIELD APPLICATION NH3 EMISSIONS -----------------------------------------------------------------------

source('./Nutrients/Model/Fertilization/2_Biosolids.R')
source('./Nutrients/Model/Fertilization/4_BiosolidsAllocation.R')

set_NH3_biosolid = function() {
  # EMEP 2019
  # unit kg N-Nh3 kg N applied -1
  
  EF_NH3_biosolid = 0.13 * 17/14 
  return(EF_NH3_biosolid)
}


compute_crop_biosolid_app_NH3 = function(main_param, param, manure_method = 'Method I', nutrient = 'N') {
  # unit: kg N-NH3 yr-1
  
  ef = set_NH3_biosolid()
  crop_biosolid_app = get_activity_data(module = 'Nutrients', 
                                        mainfolder = 'Output', 
                                        folder = 'Fertilisation', 
                                        subfolder = 'N', 
                                        subfolderX2 = 'Biosolids_application', 
                                        subfolderX3 = manure_method, 
                                        subfolderX4 = main_param, 
                                        pattern = param)
  
  yrs = paste0('X',seq(1987,2017))
  crop_biosolid_app_NH3 = crop_biosolid_app
  crop_biosolid_app_NH3[, yrs] = sapply(yrs, function(x) round(crop_biosolid_app[, x] / ef, 1))
  
  return(crop_biosolid_app_NH3)
  rm(list=c('ef','crop_biosolid_app'))
}


loop_crop_biosolid_app_NH3 = function(manure_method = 'Method I', nutrient = 'N') {
  
  biosolid_crops = get_sludge_distribution_crops()
  
  for (i in 1:nrow(biosolid_crops)) {
    
    main_param = biosolid_crops[i, 'Main_crop']
    param = biosolid_crops[i, 'Crop']
    
    crop_biosolid_app_NH3 = compute_crop_biosolid_app_NH3(main_param, param, manure_method)
    export_file(module = 'Nutrients', 
                file = crop_biosolid_app_NH3, 
                filename = param, 
                folder = 'Gas_N_emissions', 
                subfolder = 'NH3', 
                subfolderX2 = 'Biosolid_application',
                subfolderX3 = manure_method, 
                subfolderX4 = main_param)
  }
}
