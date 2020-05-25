source('./Main/Global_functions.R')



##  ANIMAL DIVISION INTO RUMINANTS AND NON-RUMINANTS -----------------------


## Manure distribution assumptions
## 25 % of pig manure AND 100% ruminant manure --> fodder crops + intensive pasture
## 100% of poultry, equides, rabbits and 75% pig manure --> non-fodder crops


get_animal_class_distribution <- function(animal_type) {
  
  if (animal_type == 'ruminants' | animal_type == 'non_ruminants') {

    if (animal_type == 'ruminants') { type = c('Bovine', 'Sheep', 'Goats') } else { type = c('Equides', 'Poultry', 'Rabbits') }
    
    return(type)
  }
  else {
    stop('Ruminants or non_ruminants, please.')
  }
}


## CROP DIVISION INTO FODDER AND NON-FODDER --------------------------------


get_fodder_nonfodder_crops <- function(is_fodder) {
  # automatically excludes industry and horticulture crops
  
  if (is_fodder == TRUE) {
    
    params = c('Forage','Pastures')
  }
  else {
    
    params = get_standard_params_list(main_param = 'Crops')
    params = subset(params, Main_crop != 'Industry_crops' & Main_crop != 'Horticulture' & Main_crop != 'Forage' & Main_crop != 'Pastures')
    params = unique(params[,1])
  }
  return(params)
}


## EQUIVALENT FERTILIZER RECOMMENDED VALUE --------------------------------

set_N_FRV = function(nutrient, org_fertiliser) {
  # gets the fertilizer replacement values from Hijbeek et al (2018)
  # to distribute the organic N to the crop N requirements
  
  if (org_fertiliser == 'Biosolids' | org_fertiliser == 'Slurry' | org_fertiliser == 'Solid') {
    
    FRNV <- data.frame(fert_type = c('Slurry', 'Solid', 'Biosolids'),
                         FRV = c(1.12, 0.58, 0.58))
    sb_FRNV = FRNV[which(FRNV[, 1] == org_fertiliser), 2]
    return(sb_FRNV)
  } 
  else {
    stop('Biosolids, Slurry or Solid, please.')
  }
}


compute_gross_man_NITROGEN_FRV = function(nutrient, manure_type, manure_df) {
  # updates a given gross manure dataframe (i.e., total of a given animal_type (ruminant/non_ruminant))
  # updates only if nutrient == 'N' otherwise the dataframe won't be changed
  # unit: "N" --> kg N-FRNV yr-1
  # unit: "C" or "P" kg nutrient yr-1
  
  if (nutrient == 'N') {
    
    FRNV = set_N_FRV(nutrient, org_fertiliser = manure_type)
    
    # update new manure FRNV for all years
    yrs = paste0('X', seq(1987,2017))
    manure_df[, yrs] = sapply(yrs, function(x) round(manure_df[, x] * FRNV, 1))
    
    return(manure_df)
  }
  else {
    return(manure_df)
  }
}


# MANURE SPREADING CROP ALLOCATION METHOD I -------------------------------------------------------------------------------------------------------

#* Assumptions (Serra et al 2019):
#** fodder crops receive 25% and 100% of pig and ruminant manure, respectively
#** non-fodder crops receive 100% of the manure from poultry, horses, rabbits and 75% of pig manure


#* ALGORITHM EXPLAINED

#*** allocate the gross manure spreading (before application) for ruminants and non-ruminants according to the previous assumption
#*** sum the solid and slurry nutrient content of the gross manure spreading for ruminants and non-ruminants (total_Gross_man_alloc)
#*** get the FRACTION of total_gross_man_alloc to be allocated for a given crop according to the assumptions (i.e., whether fodder or non-fodder crop) (Crop_gross_man_alloc)

#** calculation I: Remaining nutrient requirements and manure rusplus
#*** crop nutrient requirements - Crop_gross_man_alloc (eq 1)
#*** MANURE SURPLUS --> if eq 1 < 0, there is manure surplus ;; this should be stored when looping (see bellow) ;;
#*** REMAINING NUTRIENT REQUIREMENTS --> if eq1 >=0, everything is ok ;; if eq 1 <0, set this municipality to 0

#** calculation I.I: use any remaining manure rusplus
#**** when looping, keep storing manure surplus into one datafram
#**** after crop looping, do a municipality-crop loop to use any remaining manure surplus to where there are nutrient requirements to be fulfilled (same municipality)
#**** update crop nutrient requirements


#** calculation I.II: compute crop manure application rates (kg N crop_area-1 yr-1)



pigs_manure_assumption = function(animal_type) {
  
  if (animal_type == 'ruminants') { FRAC = 0.25 } else {FRAC = 0.75 }
  return(FRAC)
}



compute_animal_type_manure_spreading = function(animal_type, nutrient, manure_type) {
  # IMPORTANT, SEE NOTE !!!!
  # NOTE: an error may arise from "Manure_total_sum.R", if so loop_totals_per_animalParam() must be implemented to gross_manure
  # better yet, implement a general loop function to calculate this for each pathway
  
  # 1 - sum the total nutrient content of gross manure to be spreading (- NH3 emissions following/during application) for a given manure_type according to animal type (ruminant/non_ruminant)
  # 2 - accordingly, get pig manure to be allocated
  # 3 - sum #1 and #2
  # unit: kg nutrient yr-1
  

  animal_type_params = get_animal_class_distribution(animal_type = animal_type)
  
  # prepare dataframe to store all gross manure spreading
  yrs <- paste0('X', seq(1987,2017))
  gross_man_spread <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  gross_man_spread[, yrs] <- sapply(yrs, function(x) gross_man_spread[,x] <- 0)
  
  # sum total gross manure for either ruminant or non-ruminant animals (EXCLUDING PIGS)
  for (main_param in animal_type_params) {
    
    animal_gross_man_spread <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'Gross_spreading', subfolderX3 =manure_type, subfolderX4 = 'Total', pattern = main_param)
    gross_man_spread[, yrs] = sapply(yrs, function(x) round( gross_man_spread[,x] + animal_gross_man_spread[,x], 1))
  }
  
  # get pig assumption and calculate total pig manure to be allocated
  #* 25% if ruminants, 75% if non-ruminants
  pig_gross_man_spread <- get_activity_data(module = 'Nutrients', mainfolder = 'Output',  folder = 'Gross_manure', subfolder = nutrient, subfolderX2 = 'Gross_spreading', subfolderX3 =manure_type, subfolderX4 = 'Total', pattern = 'Pigs')
  FRAC_pig = pigs_manure_assumption(animal_type)
  pig_gross_man_spread[, yrs] = sapply(yrs, function(x) round(pig_gross_man_spread[,x] * FRAC_pig, 1))
  
  
  # sum everything accordingly
  gross_man_spread[, yrs] = sapply(yrs, function(x) round(gross_man_spread[,x] * pig_gross_man_spread[, x], 1))
  
  # if nutrient is nitrogen, calculates the equivalent fertiliser replacement value, otherwise gross_man_spread stays the same
  gross_man_spread = compute_gross_man_NITROGEN_FRV(nutrient = nutrient, manure_type = manure_type, manure_df = gross_man_spread)
  
  return(gross_man_spread)
  rm(list=c('animal_type_params', 'yrs','animal_gross_man_spread','pig-gross_man_spread','FRAC_pig'))
}






allocate_crop_animalType_manureType = function(nutrient, manure_type, param) {
  # allocates gross manure nutrient content for the ith manure type (Solid, Slurry) and specified param (crop) 
  # unit: kg nutrient yr-1 
  
  crop_allocation_weights = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Fertilisers', subfolderX3 = 'Manure_crop_distribution', pattern = 'Manure_distribution_weights')
  
  find_crop = which(crop_allocation_weights[, 'crops'] == param)
  
  # get allocation fraction of the total nutrient content of the allocated manure portion for a given crop (param)
  FRAC_crop_manure_allocation = crop_allocation_weights[find_crop, 'Weights']
  
  # get animal_type and respective gross manure spreading for a given crop (param)
  animal_type = crop_allocation_weights[find_crop, 'animal_division']
  gross_man_animal_type = compute_animal_type_manure_spreading(animal_type = animal_type, nutrient = nutrient, manure_type = manure_type)
  
  # allocate nutrient content of manure to the specified crop (param)  
  yrs = paste0('X', seq(1987,2017))
  gross_man_animal_type[, yrs] = sapply(yrs, function(x) round(gross_man_animal_type[, x] * FRAC_crop_manure_allocation, 1))
  
  return(gross_man_animal_type)
  rm(list=c('animal_type','find_crop','FRAC_crop_manure_allocation', 'yrs'))
}


allocate_crop_animalType_totalManure = function(nutrient, param) {
  # allocates the sum of solid and slurry manure for the speciried param (crop)
  # unit: kg nutrient yr-1
  
  slurry_man_alloc = allocate_crop_animalType_manureType(nutrient, 'Slurry', param)
  solid_man_alloc = allocate_crop_animalType_manureType(nutrient, 'Solid', param)
  
  yrs = paste0('X', seq(1987,2017))
  slurry_man_alloc[, yrs] = sapply(yrs, function(x) round(slurry_man_alloc[, x] + slurry_man_alloc[,x], 1))
  
  return(slurry_man_alloc)
  rm(list='solid_man_alloc')
}



# CALCULATION 1 -- CROP NUTRIENT REQUIREMENTS MINUS ALLOCATED MANURE ---------------------------------------------- 

compute_manure_surplus = function(calculation_df) {
  # serves two purposes:
    #* 1 - calculations the remaining nutrient demand (Nutrient req - Nutrient_manure)
    #* 2 - Calculations any excess of manure (i.e., if Nutrient req - Nutrient_manure <0)
  # output: index1 - remaining nutrient demand ;; index2 - excess of manure
  
  remaining_nutrient_req = calculation_df
  man_surplus = calculation_df
  
  manure_surplus_ids = which(calculation_df<0, arr.ind = TRUE)
  
  for (i in 1:nrow(manure_surplus_ids)) {
    
    r = manure_surplus_ids[i, 1]
    c = manure_surplus_ids[i, 2]
    
    # set to 0 for further fertilisation calculations (i.e., remaining crop nutrient demand)
    remaining_nutrient_req[r,c] = 0
    
    # set to positive because there is an excess of manure
    man_surplus[r,c] = -1 * calculation_df[r,c]
  }
  return(list(remaining_nutrient = remaining_nutrient_req, man_surplus = man_surplus))
}



calc1_crop_nutrientReq_minus_Man = function(nutrient, main_param, param) {
  
  gross_man_alloc = allocate_crop_animalType_totalManure(nutrient, param)
  nutrient_req = get_activity_data(module = 'Nutrients', mainfolder =  'Output', folder = 'Fertilisation', subfolder = nutrient, subfolderX2 = 'Crop_requirements', subfolderX3 = main_param, pattern = param)
  
  # calculation: Nutrient_requirement - Gross_man_allocated_nutrient
  yrs = paste0('X', seq(1987,2017))
  nutrient_req[, yrs] = sapply(yrs, function(x) round(nutrient_req[, x] - gross_man_alloc[,x], 1))
  
  # remaining nutrient demand
  remaining_nutrient_req = nutrient_req
  remaining_nutrient_req = compute_manure_surplus(remaining_nutrient_req)[[1]]
  
  # manure surplus
  man_surplus = nutrient_req
  man_surplus = compute_manure_surplus(man_surplus)[[2]]
  
  return(list(remaining_nutrient = remaining_nutrient_req, man_surplus = man_surplus))
}





# MANURE SPREADING CROP ALLOCATION METHOD II -------------------------------------------------------------------------------------------------------

#* HIERARCHICAL APPROACH
#** priority was given to forage crops > intensitve pastures > cereals > potatoes > vineyard + olive groves > other permanent crops > pulses (generally speaking)
#*** Use the same allocation approach based on Serra et al (2019) assumptions on fodder/non_fodder crops and ruminants/non_ruminant manure allocation
#*** Allocate manure according to the hieraarchy established
#**** update manure nutrient allocated accordingly 
#**** higher priority crops receive the largest amounts of manure
#**** if in the end there is still yet some manure nutrient yet to be allocated, this is considered the manure surplus


