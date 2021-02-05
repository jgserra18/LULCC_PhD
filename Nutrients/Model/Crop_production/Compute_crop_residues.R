source('./Main/Global_functions.R')
source('./Nutrients/Model/Crop_production/Compute_crop_nutrient_offtake.R')
source('./Main/Data_operations.R')



include_belowground_biomass = FALSE


## BURNT AND REMAINING AREAS------------------------------------------------------------------------ 


compute_crop_burnt_areas <- function(main_param, param) {
  # unit: ha burnt yr-1
  
  if (main_param == 'Pastures' | main_param == 'Industry_crops' | main_param == 'Potato'| main_param == 'Forage') {
    next
  }
  else {
    areas <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
    
    # find FRAC_burnt for param
    FRAC_burnt <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Residues', pattern = 'Residues_FRACburnt')
    FRAC_burnt <- general_linear_extrapolation_3years(file_df = FRAC_burnt, existing_years = c('X1990','X1999','X2009'))
    FRAC_burnt <-  FRAC_burnt[which(FRAC_burnt[, 'crop']==param), ] 
    
    # find Cf for param
    cf <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Residues', pattern = 'Residues_Cf')
    cf <- find_crop_variable(df = cf, param_col = 'Crop', param = param, var = 'Cf')
    
    # calculation --- 
    yrs <- paste0('X', seq(1987,2017))
    areas[, yrs] <- sapply(yrs, function(x) round ( areas[,x] * FRAC_burnt[, x] * cf, 1))
    
    return(areas)
    rm(list=c('FRAC_burnt','cf'))
  }
}


compute_remain_crop_areas <- function(main_param, param) {
  # unit: ha unburnt yr-1
  
  
  if (main_param == 'Pastures' | main_param == 'Industry_crops' | main_param == 'Pulses' | main_param == 'Potato' | main_param == 'Forage') {
    
    areas <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
    return(areas)
  }
  else {
    areas <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
    burnt_areas <- compute_crop_burnt_areas(main_param, param)
    
    # calculation --- 
    yrs <- paste0('X', seq(1987,2017))
    areas[, yrs] <- sapply(yrs, function(x) round ( areas[,x] - burnt_areas[, x], 1))
    
    return(areas)
    rm(list=c('burnt_areas','yrs'))
  }
}



## DM YIELDS ---------------------------------------

compute_crop_DM_yield <- function(main_param, param) {
  # pasture yields are static over time and are based on Velthof et al 2009
  # unit: kg DM ha-1 yr-1
  
  if (main_param != 'Pastures') {
    
    yields <- get_spatially_disaggagregated_yields(main_param, param)
    
    if (main_param != 'Horticulture') {
      FRAC_DM <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Residues', pattern = 'Residues_params')
      FRAC_DM <- find_crop_variable(df = FRAC_DM, param_col = 'Crop', param = param, var = 'DM_frac')
    }
    else {
      FRAC_DM <- 1
    }
    
    # calculation --- 
    yrs <- paste0('X', seq(1987,2017))
    yields[, yrs] <- sapply(yrs, function(x) round ( yields[,x] * FRAC_DM, 1))
    
    return(yields)
    rm(list='FRAC_DM')
  }
  else {
    
    FRAC_DM = 0.8
    yields = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Grass_yield', pattern = 'Pastures_yield')
    
    # calculation --- 
    yrs <- paste0('X', seq(1987,2017))
    yields[, yrs] <- sapply(yrs, function(x) round ( FRAC_DM * yields[,x], 1))
    
    return(yields)
  }
}


## ABOVEGROUND RESIDUES -----------------------------------------------------------------------------------------------

compute_AG_biomass <- function(main_param, param) {
  # computes abovground residue biomass based on IPCC (2006) Table 11.2 parameters
  # AG_DM = ( Yield * DM / 1000 ) * slope + intercept
  # unit: kg DM ha yr-1
  
  residues_params <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Residues', pattern = 'Residues_params')
  slope <- find_crop_variable(df = residues_params, param_col = 'Crop', param = param, var = 'slope')
  intercept <- find_crop_variable(df = residues_params, param_col = 'Crop', param = param, var = 'intercept')
  
  yields <- compute_crop_DM_yield(main_param, param)
  
  # calculation --- 
  yrs <- paste0('X', seq(1987,2017))
  yields[, yrs] <- sapply(yrs, function(x) round ( yields[,x]/1000 * slope + intercept, 1))
  
  return(yields)
  rm(list=c('residues_params','slope','intercept'))
}


compute_ratio_R_AG_yield <- function(main_param, param) {
  # ratio of AB residues biomass to harvsted crop yield
  # R_AG = AG_DM * 1000 / (yield * DM)
  # unit: dimensionless
  
  yields <- compute_crop_DM_yield(main_param, param)
  
  AG_DM <- compute_AG_biomass(main_param, param)
  
  # calculation --- 
  yrs <- paste0('X', seq(1987,2017))
  AG_DM[, yrs] <- sapply(yrs, function(x) round ( AG_DM[,x] * 1000 / yields[,x], 1))
  AG_DM <- data_cleaning(AG_DM)
  
  return(AG_DM)
  rm(list=c('yields'))
}






## BELOWGROUND RESIDUES -----------------------------------------------------------------------------------------------


compute_ratio_total_AG_biomass_yield <- function(main_param, param) {
  # computes the ratio of total aboveground biomass to harvested crop yield
  # TAG_DM = ( AG_DM * 1000 + (Yield * DM) ) / (Yield * DM)
  # unit: dimensionless
  
  yields <- compute_crop_DM_yield(main_param, param)
  
  AG_DM <- compute_AG_biomass(main_param, param)
  
  # calculation --- 
  yrs <- paste0('X', seq(1987,2017))
  
  if (param == 'Annual_mixtures' | param =='other_forage') {
    AG_DM[, yrs] <- sapply(yrs, function(x) AG_DM[,x] = 0.3)
  }
  else {
    
    AG_DM[, yrs] <- sapply(yrs, function(x) round ( (AG_DM[,x] * 1000 + yields[,x] ) / yields[, x], 1))
    AG_DM <- data_cleaning(AG_DM)
  }
  
  return(AG_DM)
  rm(list=c('yields'))
}


compute_ratio_R_BG_yield <- function(main_param, param) {
  # computes the ratio of belowground residues to harvested crop yield
  # R_BG = R_BG_BIO * TAG_DM
  # unit: dimensionless
  
  TAG_DM <- compute_ratio_total_AG_biomass_yield(main_param, param)
  
  residues_params <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Residues', pattern = 'Residues_params')
  R_BG_BIO <- find_crop_variable(df = residues_params, param_col = 'Crop', param = param, var = 'R_BG_BIO')
  
  # calculation --- 
  yrs <- paste0('X', seq(1987,2017))
  TAG_DM[, yrs] <- sapply(yrs, function(x) round (TAG_DM[,x] * R_BG_BIO, 1))
  
  return(TAG_DM)
  rm(list='R_BG_BIO')
}


## N AND P IN CROP RESIDUES-----------------------------------------------------------------------------------------------

# Total N in crop residues, irrespective of whether it is left, removed or burnt 
# is divided into different two different sections (IPCC 2006 EQ 11.6)
# One section regards the aboveground biomass, the other the belowground biomass


get_residues_nutrient_content <- function(nutrient, param, var) {
  # var is either AG or BG
  # unit: kg nutrient tonnes DM -1 yr-1
  
  pattern_name <- paste0('Residues_', nutrient,'content')
  residues_params <- get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = nutrient, subfolderX2 = 'Crops',  subfolderX3 = 'Residues', pattern = pattern_name)

  if (nutrient == 'P') {
    
    var <- paste0('P2O5_', var)
    nutrient_var <- find_crop_variable(df = residues_params, param_col = 'crop', param = param, var = var)
    nutrient_var <- nutrient_var * 0.4364 #  / 1000 # convert from kg P ton DM-1 to kg P kg DM-1
  }
  else {
    
    nutrient_var <- find_crop_variable(df = residues_params, param_col = 'crop', param = param, var = paste0(nutrient, '_', var))
  }
  
  return(nutrient_var)
  rm(list=c('pattern_name','residues_params'))
}


compute_BG_residues_section <- function(main_param, param, nutrient) {
  # computes the belowground section of IPCC (2006) 11.6
  # unit: kg N ha kg dm-1 yr-1
  
  R_BG <- compute_ratio_R_BG_yield(main_param, param)
  areas <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
  
  BG_param <- get_residues_nutrient_content(nutrient, param, 'BG') 
  
  # calculation --- 
  yrs <- paste0('X', seq(1987,2017))
  areas[, yrs] <- sapply(yrs, function(x) round( areas[, x] * BG_param * R_BG[,x], 1))
  
  return(areas)
  rm(list=c('R_BG', 'crop_area', 'BG_param'))
}


select_residue_management_fracs <- function(param, residue_practice) {
  # selects for a given crop, the FRAC of the residue left or removed from the field
  # residue_practice is either "removed" or "left"
  # unit: % of crop residue N
  
  residues_params <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Residues', pattern = 'Residues_params')
  FRAC_removed <-  find_crop_variable(df = residues_params, param_col = 'Crop', param = param, var = 'FRAC_removed')
  
  ifelse(residue_practice=='Removed',
         residue_FRAC <- FRAC_removed,
         residue_FRAC <- 1 - FRAC_removed)
  return(residue_FRAC)
}



compute_AG_residues_section <- function(main_param, param, residue_practice, nutrient) {
  # computes the aboveground section of IPCC (2006) 11.6
  # already includs
  # unit: kg N ha kg dm-1 ha-1yr-1
  
  R_AG <- compute_ratio_R_AG_yield(main_param, param)
  residue_area <- compute_remain_crop_areas(main_param, param)
  AG_param <- get_residues_nutrient_content(nutrient, param, 'AG')
  FRAC_residue <- select_residue_management_fracs(param, residue_practice)
  
  # calculation --- 
  yrs <- paste0('X', seq(1987,2017))
  AG_residues  = residue_area
  AG_residues[, yrs] <- sapply(yrs, function(x) round(residue_area[,x] * AG_param * R_AG[, x] * FRAC_residue, 3))
  
  return(AG_residues)
  rm(list=c('R_AG', 'FRAC_residue', 'AG_param'))
}



compute_vegetables_residues_left <- function(main_param = 'Horticulture', param, nutrient, residue_practice = 'Left') {
  # source: Cameira et al 2019
  # source: Fink et al 1999 Fink, M., Feller, C., Scharpf, H.-C., Weier, U., Maync, A., Ziegler, J., . Strohmeyer, K. (1999). Nitrogen, phosphorus, potassium and magnesium contents of field vegetables - 
  # Recent data for fertiliser recommendations and nutrient balances. Journal of Plant Nutrition and Soil Science, 162(1), 71-73. doi:10.1002/(sici)1522-2624(199901)162:1<71::aid-jpln71>3.0.co;2-0 
  
  areas <- get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
  residues_vegN <- areas
  
  if (nutrient == 'N') {
    nutrient_res =  30 # kg N ha-1 yr-1)
    
    # calculations -----------------------------------------------------------------
    yrs <- paste0('X', seq(1987,2017))
    
    residues_vegN[, yrs] <- sapply(yrs, function(x) round(areas[, x] * nutrient_res, 1))
  }
  else {
   
    nutrient_res = 0.40 # kg P tonnes DM-1
    yield = compute_crop_DM_yield(main_param, param)
    
    # calculations -----------------------------------------------------------------
    yrs <- paste0('X', seq(1987,2017))
    
    residues_vegN[, yrs] <- sapply(yrs, function(x) round(areas[, x] * nutrient_res * yield[, x]/1000, 1))
    
  }
  return(residues_vegN)
}


general_funct_crop_residues_nutrient <- function(main_param, param, residue_practice, nutrient) {
  # general function that is able to compute crop residues N, either left or removed from the field
  # general function to be later implemented according to residue practice
  # unit: kg N yr-1
  
  # load params ------------------------------------------------------------------
  crop_yield <- compute_crop_DM_yield(main_param, param)
  
  residues_params <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Residues',pattern = 'Residues_FRACrenew')
  FRAC_renew <- find_crop_variable(df = residues_params, param_col = 'crop', param = param, var = 'FRAC_renew')
  
  if (include_belowground_biomass == TRUE) {
    
    AG_section <- compute_AG_residues_section(main_param, param, residue_practice, nutrient)
    BG_section <- compute_BG_residues_section(main_param, param, nutrient)
    
    # calculations -----------------------------------------------------------------
    yrs <- paste0('X', seq(1987,2017))
    
    residues <- crop_yield
    residues[, yrs] <- sapply(yrs, function(x)
      round(crop_yield[,x] * FRAC_renew * (AG_section[, x] + BG_section[, x]), 1))
    residues <- data_cleaning(residues)
  }
  else {
    
    AG_section <- compute_AG_residues_section(main_param, param, residue_practice, nutrient)
    # calculations -----------------------------------------------------------------
    yrs <- paste0('X', seq(1987,2017))
    
    residues <- crop_yield
    residues[, yrs] <- sapply(yrs, function(x)
      round(crop_yield[,x] * FRAC_renew * (AG_section[, x]), 1))
    residues <- data_cleaning(residues)
  }
  

  return(residues)
  rm(list=c('crop_yield', 'FRAC_renew', 'AG_section', 'BG_section'))
}


compute_crop_residues_left <- function(main_param, param, nutrient, residue_practice = 'Left') {
  
  
  if (main_param == 'Horticulture') {
    
    CR_left <- compute_vegetables_residues_left(main_param, param, nutrient)
  }
  else {
    
    CR_left <- general_funct_crop_residues_nutrient(main_param, param, residue_practice, nutrient)
  }
  export_file(module = 'Nutrients', 
              file = CR_left, 
              filename = param, 
              folder = 'Crop_residues', 
              subfolder = residue_practice, 
              subfolderX2 = nutrient,
              subfolderX3 = main_param)
}




compute_crop_residues_removed <- function(main_param, param, nutrient, residue_practice = 'Removed') {
  
  CR_removed <- general_funct_crop_residues_nutrient(main_param, param, residue_practice, nutrient)
  export_file(module = 'Nutrients', 
              file = CR_removed, 
              filename = param, 
              folder = 'Crop_residues', 
              subfolder = residue_practice, 
              subfolderX2 = nutrient,
              subfolderX3 = main_param)
}


compute_crop_residues_nutrient <- function(nutrient) {
  # crop residues N/P left and removed from the field computation function 
  # unit: kg N/P yr-1
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
 # main_crops <- c('Cereals', 'Industry_crops', 'Potato', 'Pulses', 'Pastures')
  main_crops <- c('Cereals', 'Industry_crops', 'Potato', 'Pulses', 'Horticulture', 'Forage', 'Pastures')
 # main_crops <- c('Forage')
  for (i in main_crops) {
    
    crops <- standard_params[which(standard_params$Main_crop==i), 'Crop']
    
    for (j in crops) {

      compute_crop_residues_left(main_param = i, param = j, nutrient = nutrient)
      compute_crop_residues_removed(main_param = i, param = j, nutrient = nutrient)
    }
  }
}


loop_crop_residues_left_removed <- function() {
  
  nutrients <- c('N','P')
  sapply(nutrients, function(x) compute_crop_residues_nutrient(x))
}



## CROP RESIDUES BURNT IN SITU-------------------------------------------------------

compute_PC_trees <- function(main_param, param) {
  # calculates the no of trees of a permanent crop at the municipality level
  # EXCEPTION ----> VINEYARDS !!!!!!!!!!!!!!!
  # NO_trees = Burnt_area * Tree_density
  # unit: No_trees yr-1
  
  if (main_param == 'Vineyards') {
    print('Vineyards method is different.')
    next 
  }
  else {
    
    crop_area <- compute_crop_burnt_areas(main_param, param)
    
    # get tree densities from the AR to the municipality scale
    tree_densities <- get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Residues',pattern = 'PermaCrops_tree_densities')
    disagg_df <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Spatial_disaggregation')
    tree_densities <- plyr::join(x = disagg_df, y = tree_densities, by = 'agrarian_region_id')
    
    NO_trees <- crop_area
    
    # calculation --------------------------------------------------------
    yrs <- paste0('X', seq(1987,2017))
    NO_trees[, yrs] <- sapply(yrs, function(x) round(NO_trees[,x] * tree_densities[, param], 1))
    
    return(NO_trees)
    rm(list=c('crop_area', 'tree_densities'))
  }
}



compute_PC_burnt_residues_N <- function(main_param, param) {
  # calculates N in crop residues burnt of PERMANENT CROPS, EXCEPT VINEYARDS
  # BURNT_RES_N = NO_TREES * RES_TREE * RES_N
  # Unit: kg N yr-1
  
  pc_trees <- compute_PC_trees(main_param, param)
  
  tree_residues <-  get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Residues',pattern = 'PermaCrops_params')
  tree_residues <- find_crop_variable(df = tree_residues, param_col = 'crop', param = param, var = 'Res_trees') #kg residue / tree / yr
  
  N_residues <- get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'Crops', subfolderX3 = 'Residues',pattern = 'PermaCrops_Ncontent') #kg N / tonnes residues / yr
  N_residues <- find_crop_variable(df = N_residues, param_col = 'crop', param = param, var = 'N_res')
  
  burnt_resN <- pc_trees
  
  # calculation --------------------------------------------------------
  yrs <- paste0('X', seq(1987,2017))
  burnt_resN[, yrs] <- sapply(yrs, function(x) round( (burnt_resN[, x] * tree_residues ) / 1000 * N_residues, 1))
  
  return(burnt_resN)
  rm(list=c('pc_trees', 'tree_residues', 'N_residues', 'burnt_resN'))
}


compute_vineyard_burnt_residues_N <- function(main_param = 'Vineyard', param = 'Vineyard') {
  # computes total N in vineyard residues burnt in situ
  # unit: kg N yr-1
  
  vine_area <- compute_crop_burnt_areas(main_param, param)
  vine_res <- 1.4 # tonnes residues ha-1 yr-1
  
  vine_Nres <- get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = 'N', subfolderX2 = 'Crops', subfolderX3 = 'Residues',pattern = 'PermaCrops_Ncontent') #kg N / tonnes residues / yr
  vine_Nres <- find_crop_variable(df = vine_Nres, param_col = 'crop', param = 'Vineyard', var = 'N_res')
  
  burnt_vineN <- vine_area
  
  # calculation --------------------------------------------------------
  yrs <- paste0('X', seq(1987,2017))
  burnt_vineN[, yrs] <- sapply(yrs, function(x) round(burnt_vineN[, x] * vine_res * vine_Nres, 1))
  
  return(burnt_vineN)
  rm(list=c('vine_area', 'vine_res', 'vine_Nres', 'burnt_vineN'))
}


compute_cereals_burnt_residues_N <- function(main_param = 'Cereals', param) {
  # computes N in cereal burnt residues
  # unit: kg N yr-1
  
  crop_yield <- compute_crop_DM_yield(main_param, param)
  crop_area <- compute_crop_burnt_areas(main_param, param)
  R_AG <- compute_ratio_R_AG_yield(main_param, param)
  N_AG <- get_residues_nutrient_content('N', param, 'AG')

  burnt_cerN <- crop_area

  # calculation --------------------------------------------------------
  yrs <- paste0('X', seq(1987,2017))
  burnt_cerN[, yrs] <- sapply(yrs, function(x) round(burnt_cerN[, x] * N_AG * crop_yield[, x] * R_AG[, x], 1))
  
  return(burnt_cerN)
  rm(list=c('crop_area', 'R_AG', 'N_AG'))
}


compute_all_crop_residues_burnt_N <- function() {
  
  main_crops <- c('Cereals','Vineyard', 'Olive_grove','Citrus','Dried_fruits','Fresh_fruits')
  standard_params <- get_standard_params_list(main_param = 'Crops')
  
  for (i in main_crops) {
    
    crops <- standard_params[which(standard_params$Main_crop==i), 'Crop']
    
    for (j in crops) {
      
      if (i == 'Cereals') {
        burntN <- compute_cereals_burnt_residues_N(i, j)
      }
      else if (i == 'Vineyard') {
        burntN <- compute_vineyard_burnt_residues_N(i, j) 
      }
      else {
        burntN <- compute_PC_burnt_residues_N(i, j)
      }
      export_file(module = 'Nutrients', 
                  file = burntN, 
                  filename = j, 
                  folder = 'Crop_residues', 
                  subfolder = 'Burnt', 
                  subfolderX2 = 'N',
                  subfolderX3 = i)
    }
  }
}






## CARBON MODULE ---------------------------------------------------------------------- 

# implements the methodology described to calculate C efficient inputs in Le Noe et al (2016)
# How the structure of agro-food systems shapes nitrogen, phosphorus, and carbon fluxes:
# The generalized representation of agro-food system applied at the regional scale in France
# http://dx.doi.org/10.1016/j.scitotenv.2017.02.040


get_crop_Cparams = function(residue_param, main_param, param, nutrient='C') {
  # unit: kg C kg N-1

  res_param <- get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = nutrient, subfolderX2 = 'Crops',  subfolderX3 = 'Residues', pattern = 'params')
  param2 = ifelse(residue_param == 'HI', 'HI','Root_shoot_ratio')
  res_param = subset(res_param, crop == param)[, param2]
  
  return(res_param)
}


get_crop_HU_coefficients = function(residues_section, main_param, param, nutrient='C') {
  # unit: %
  
  HU_coeff <- get_activity_data(module = 'Nutrients', folder = 'Nutrient_params', subfolder = nutrient, subfolderX2 = 'Crops',  subfolderX3 = 'Residues', pattern = 'HU')
  res_param = ifelse(residues_section == 'AG', 'HU_AG','HU_BG')
  
  HU_coeff = subset(HU_coeff, crop == param)[, res_param]
  
  return(HU_coeff)
}

compute_Cefficient_residues_section = function(residues_section, main_param, param, nutrient = 'C') {
  # calculates the C efficient inputs in either above- or belowground biomass in crop residues
  # these include the annual amount of C mineralized
  # residues_section = "AG" or "BG"
  # unit: kg C  yr-1
  
  yrs  = paste0('X',seq(1987,2017))
  
  # get params
  HU_coeff = get_crop_HU_coefficients(residues_section, main_param, param, nutrient='C')
  crop_harvested = get_activity_data(module = 'Nutrients', mainfolder = 'Output', folder = 'Crop_offtake', subfolder = 'C', subfolderX2 = main_param, pattern = param)
  RS_ratio = get_crop_Cparams(residue_param = 'Root_shoot_ratio', main_param, param, nutrient)
  HI = get_crop_Cparams(residue_param = 'HI', main_param, param, nutrient)
  
  C_input = crop_harvested
  
  if (residues_section == 'AG') {
    
    C_input[, yrs] = sapply(yrs, function(x) round((1-HI)/HI*crop_harvested[,x] * HU_coeff, 1))
  }
  else {
    
    C_input[, yrs] = sapply(yrs, function(x) round( RS_ratio/HI * crop_harvested[,x] * HU_coeff, 1))
  }
  
  return(C_input)
  rm(list=c('HU_coeff','crop_harvested','RS_ratio','HI'))  
}


compute_total_residues_Cefficient_input = function(main_param, param, nutrient = 'C') {
  # sum of above and root C efficient inputs
  # unit: kg C yr-1
  
  yrs  = paste0('X',seq(1987,2017))
  
  AB_efficient_inp = compute_Cefficient_residues_section(residues_section = 'AB', main_param, param, nutrient)
  root_efficient_inp = compute_Cefficient_residues_section(residues_section = 'BG', main_param, param, nutrient)
  
  total_efficient_inp = root_efficient_inp
  total_efficient_inp[, yrs] = sapply(yrs, function(x) round(AB_efficient_inp[,x] + root_efficient_inp[,x], 1))
  
  return(total_efficient_inp)
  rm(list=c('AB_efficient_inp','root_efficient_inp'))
}

loop_residues_Cefficient_input = function(nutrient='C') {
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  # main_crops <- c('Cereals', 'Industry_crops', 'Potato', 'Pulses', 'Pastures')
  main_crops <- c('Cereals', 'Industry_crops', 'Potato', 'Pulses', 'Horticulture', 'Forage', 'Pastures')
  # main_crops <- c('Forage')
  for (i in main_crops) {
    
    crops <- standard_params[which(standard_params$Main_crop==i), 'Crop']
    
    for (j in crops) {
      print(j)
      eff_Cinp = compute_total_residues_Cefficient_input(i, j, nutrient)
      export_file(module = 'Nutrients', 
                  file = eff_Cinp, 
                  filename = j, 
                  folder = 'Crop_residues', 
                  subfolder = 'Left', 
                  subfolderX2 = 'C',
                  subfolderX3 = i)
    }
  }
}





## TOTALS -----------------------------------------------------------------------------

compute_total_crop_residues <- function(nutrient) {
  # calculate only after the implementation of the "Fodder_production" module
  # currently excludes fodder crops !!!!
  # unit: kg nutrient yr-1
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  
  yrs <- paste0('X', seq(1987,2017))
  
  if (nutrient=='N') { manag = c('Burnt','Left','Removed') } else if (nutrient == 'P') { manag = c('Left','Removed') } else { manag = 'Left'}
  
  for (i in manag) {
    
    # create calculation df for the ith management practice
    store_manag <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
    store_manag[, yrs] <- sapply(yrs, function(x) store_manag[,x] <- 0)
    
    main_param <- get_dir_files(module = 'Nutrients', folder = 'Output', param_pattern = 'Crop_residues', subfolder = i, subfolderX2 = nutrient, file_names =  T)
    
    for (j in main_param) {
      
      param_rows <-  which(standard_params[, 'Main_crop'] == j)
      params <- standard_params[param_rows, 'Crop']
      
      store_main_param <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
      store_main_param[, yrs] <- sapply(yrs, function(x) store_main_param[,x] <- 0)
      
     # if (j == 'Pastures') { next } else {
        
        for (z in params) {
          
          res_param <- get_activity_data(module = 'Nutrients',mainfolder = 'Output', folder = 'Crop_residues', subfolder = i, subfolderX2 = nutrient, subfolderX3 = j,  pattern = z) 
          
          store_main_param[, yrs] <- sapply(yrs, function(x) round( store_main_param[, x] + res_param[,x], 1))
          store_manag[, yrs] <- sapply(yrs, function(x) round( store_manag[, x] + res_param[,x], 1))
        }
        # export main param
        export_file(module = 'Nutrients', 
                    file = store_main_param, 
                    filename =j, 
                    folder = 'Crop_residues', 
                    subfolder =  i, 
                    subfolderX2 = nutrient,
                    subfolderX3 = 'Total')
      #}
    }
    # export manag
    export_file(module = 'Nutrients', 
                file = store_manag, 
                filename = paste0('Total_',i), 
                folder = 'Crop_residues', 
                subfolder =  i, 
                subfolderX2 = nutrient,
                subfolderX3 = 'Total')
  }
}


loop_total_crop_residues <- function() {
  
  nutrients <- c('N','P','C')
  sapply(nutrients, function(x) compute_total_crop_residues(x))
}



# AGGREGATE NUTRIENT FLOWS PER REERENCE AREA ------------------------------------------

compute_total_residues = function(crop_type = 'Fodder', residue_practice='Removed',nutrient='N') {
  
  yrs  = paste0('X',seq(1987,2017))
  store <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store[, yrs] <- sapply(yrs, function(x) store[,x] <- 0)
  
  standard_params <- get_standard_params_list(main_param = 'Crops')
  
  if (crop_type == 'Fodder') {
    
    main_crops = c('Forage','Pastures')
    standard_params = subset(standard_params, Main_crop %in% main_crops)
  }
  else {
    
    main_crops =  c('Cereals','Horticulture','Industry_crops','Potato', 'Pulses')
    standard_params = subset(standard_params, Main_crop %in% main_crops)
  }
  
  
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i, 1]
    param = standard_params[i, 2]
    res_flows = get_activity_data(module = 'Nutrients',mainfolder = 'Output', folder = 'Crop_residues', subfolder = residue_practice, subfolderX2 = nutrient, subfolderX3 = main_param,  pattern = param) 
    store[, yrs] = sapply(yrs, function(x) round(store[,x] + res_flows[,x], 1))
  }
  return(store)
}

compute_total_crop_residues_flows_referenceArea = function(crop_type = 'Fodder', 
                                                           reference_area = 'Cropland',
                                                           residue_practice='Removed',
                                                           nutrient = 'N') {
  yrs  = paste0('X',seq(1987,2017))
  store <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store[, yrs] <- sapply(yrs, function(x) store[,x] <- 0)
  
  standard_params <- get_standard_params_list(main_param = 'Crops')

  if (crop_type == 'Fodder') {
    
    main_crops = if(reference_area == 'Cropland') { c('Forage','Pastures') } else { c('Pastures') }
    standard_params = subset(standard_params, Main_crop %in% main_crops)
  }
  else {
    
    main_crops =  c('Cereals','Horticulture','Industry_crops','Potato', 'Pulses')
    standard_params = subset(standard_params, Main_crop %in% main_crops)
  }
  standard_params
  
  
  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i, 1]
    param = standard_params[i, 2]
    
    if (crop_type == 'Fodder' & reference_area == 'Cropland' & param == 'Extensive_pasture') { 
      next
    }
    else if (crop_type == 'Fodder' & reference_area =='Grassland' & param == 'Intensive_pasture') {
      next 
    }
    else {
      print(param)
      res_flows = get_activity_data(module = 'Nutrients',mainfolder = 'Output', folder = 'Crop_residues', subfolder = residue_practice, subfolderX2 = nutrient, subfolderX3 = main_param,  pattern = param) 
      store[, yrs] = sapply(yrs, function(x) round(store[,x] + res_flows[,x], 1))
    }
  }
  
  return(store)
  rm(list=c('main_param','param','res_flows','main_crops'))
}

