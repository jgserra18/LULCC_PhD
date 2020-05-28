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
    params = subset(params, Main_crop != 'Horticulture' & Main_crop != 'Forage' & Main_crop != 'Pastures')
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
  
  if (animal_type == 'ruminants') { FRAC = 0.25 } else { FRAC = 0.75 }
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
  gross_man_spread[, yrs] = sapply(yrs, function(x) round(gross_man_spread[,x] + pig_gross_man_spread[, x], 1))
  
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
  slurry_man_alloc[, yrs] = sapply(yrs, function(x) round(slurry_man_alloc[, x] + solid_man_alloc[,x], 1))
  
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
  
  # where the data.frame > 0 (i.e., where crop nutrient demand is unfulfilled, set to 0 as there is no manure surplus
  man_surplus[man_surplus>0] = 0
  
  manure_surplus_ids = which(calculation_df<0, arr.ind = TRUE)
  
  if (length(manure_surplus_ids)==0) {
    
    return(list(remaining_nutrient = remaining_nutrient_req, man_surplus = man_surplus))
  }
  else {
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
}



calc1_crop_nutrientReq_minus_Man = function(nutrient, main_param, param) {
  # computes two things:
  #** 1 - manure surplus if the crop nutrient demand is exceeded by the manure application
  #** 2 - the remaining crop nutrient demand following application 
  # output: index is the remaining nutrient demand; index 2 is the manure surplus if it exists
  # unit: kg nutrient yr-1
  
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


compute_municipality_manure_surplus_animalType = function(animal_type, nutrient) {
  
  
  crops_animalType = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Fertilisers', subfolderX3 = 'Manure_crop_distribution', pattern = 'Manure_distribution_weights')
  crops_animalType = subset(crops_animalType, animal_division == animal_type)
  
  yrs = paste0('X', seq(1987,2017))
  
  # starts at 0
  store_man_surplus <- get_activity_data(module = 'Nutrients', folder = 'Raw_data_Municipality', pattern = 'Muni_INE') 
  store_man_surplus[, yrs] <- sapply(yrs, function(x) store_man_surplus[,x] <- 0)
  
  for (i in 1:nrow(crops_animalType)) {
    
    main_param = crops_animalType[i, 'main_crops']
    param = crops_animalType[i, 'crops']
    
    # calculate manure surplus
    crop_man_surplus = calc1_crop_nutrientReq_minus_Man(nutrient, main_param, param)[[2]]
    store_man_surplus[, yrs] = sapply(yrs, function(x) round(store_man_surplus[,x] + crop_man_surplus[,x], 1))
  }
  
  return(store_man_surplus)
  rm(list=c('crop_man_surplus'))
}



# CALCULATION 2 - MANURE CROP APPLICATION RATES -------------------------------------------

compute_crop_manure_application_rate = function(nutrient, main_param, param, nutrient_remaining_df) {
  # computes manure application for the specified crop
  # if nutrient_remaining_df (self_explanatory) is missing (i.e., non-fodder crop), it calls the function "calc1_crop_nutrientReq_minus_Man"
  # unit: kg nutrient crop_ha-1 yr-1
  
  
  if (missing(nutrient_remaining_df)==TRUE) {
    
    rem_nutrient_req = calc1_crop_nutrientReq_minus_Man(nutrient, main_param, param)[[1]]
  }
  else {
    rem_nutrient_req = nutrient_remaining_df
  }
  
  nutrient_req = get_activity_data(module = 'Nutrients', mainfolder =  'Output', folder = 'Fertilisation', subfolder = nutrient, subfolderX2 = 'Crop_requirements', subfolderX3 = main_param, pattern = param)
  area = get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
  
  # calculate manure crop application rate
  yrs = paste0('X', seq(1987,2017))
  man_app_rate = nutrient_req
  
  man_app_rate[, yrs] = sapply(yrs, function(x) round( (nutrient_req[,x] - rem_nutrient_req[,x]) / area[, x], 1))
  man_app_rate = data_cleaning(man_app_rate)
  
  return(man_app_rate)
  rm(list=c('nutrient_req','rem_nutrient_req','area'))
}



update_fodder_crop_nutrientReq_manureSurplus = function(man_surplus_df, nutrient, main_param, param) {
  # FUNCTION TO BE APPLIED ONLY TO FODDER CROPS WHERE IT WAS USED RUMINANT MANURE
  # THIS IS UNDER THE ASSUMPTION THAT THE NUTRIENT DEMAND OF FODDER CROPS CAN BE 100% FULFILLED BY MANURE
  
  # GOAL1 - updates the manure surplus dataframe after manure surplus is allocated to fulfill fodder crops nutrient demand if they are yet unfulfill
  # GOAL2 - see on which municipalityes and years fodder crop nutrient demand can be fulfilled by that municipality's manure surplus
  # GOAL3 - WILL BE FURTHER USED TO UPDATE FODDER CROPS' MANURE APPLICATION RATE
  
  # units: kg nutrient yr-1
  
  
  ruminant_man_surplus = man_surplus_df
  crop_nut_demand = calc1_crop_nutrientReq_minus_Man(nutrient, main_param, param)[[1]]
  new_crop_nut_demand = crop_nut_demand
  
  for (i in 1:nrow(ruminant_man_surplus)) {
    
    for (j in 4:ncol(ruminant_man_surplus)) {
      
      if (ruminant_man_surplus[i,j] == 0 | crop_nut_demand[i,j] == 0) {
        # if there are no manure surplus and/or all nutrient demand is fulfilled then ignore
        break
      }
      
      else {
        # there is some manure surplus and crop nutrient demand is yet to be fulfilled
        # allocate manure surplus to fulfill nutrient demand
        crop_nut_demand[i,j] = round ( crop_nut_demand[i,j] - ruminant_man_surplus[i,j] , 1)
        
        if (crop_nut_demand[i,j]<0) { 
          # correct new crop_nut_demand
          # and store new manure surplus after allocation
          
          excess_manure = crop_nut_demand[i,j] * -1
          crop_nut_demand[i,j] = 0
          
          # update ruminant_man_surplus
          ruminant_man_surplus[i,j] = round ( ruminant_man_surplus[i,j] - crop_nut_demand[i,j] + excess_manure , 1)
        }
        
        else {
          # if the manure surplus wasn't enough to fulfill crop nutrient demand
          # update ruminant_man_surplus
          
          ruminant_man_surplus[i,j] = round ( ruminant_man_surplus[i,j] - crop_nut_demand[i,j]  , 1)
          
          # correct the man surplus in case the ruminant_man_surplus - nutrient demand < 0
          if (ruminant_man_surplus[i,j] < 0) { ruminant_man_surplus[i,j] = 0 }
        }
      }
    }
  }
  return(list(updated_nutrient_req = crop_nut_demand, updated_man_surplus = ruminant_man_surplus))
}




loop_fodder_crops_manure_application_rate = function(nutrient) {
  
  
  crop_hierarchy = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Fertilisers', subfolderX3 = 'Manure_crop_distribution', pattern = 'Crop_hierarchy')
  crop_hierarchy = subset(crop_hierarchy, main_crops == 'Pastures' | main_crops == 'Forage')

  # crop = 0
  # manure surplus after the first round of application
  # uses crop hierarchy
  ruminant_man_surplus =  compute_municipality_manure_surplus_animalType('ruminants', nutrient)
  
  for (i in 1:nrow(crop_hierarchy)) {
    
    # get crop nutrient demand afrer manure application
    main_param = crop_hierarchy[i, 'main_crops']
    param = crop_hierarchy[i, 'crops']
    
    # updates the crop nutrient demand and manure surplus following manure surplus allocation (if the mechanism is applied)
    updated_crop_nut_demand = update_fodder_crop_nutrientReq_manureSurplus(man_surplus_df = ruminant_man_surplus, nutrient, main_param, param)[[1]]
    ruminant_man_surplus = update_fodder_crop_nutrientReq_manureSurplus(man_surplus_df = ruminant_man_surplus, nutrient, main_param, param)[[2]]
    
    crop_man_app = compute_crop_manure_application_rate(nutrient, main_param, param, updated_crop_nut_demand)
    export_file(module = 'Nutrients', 
                file = crop_man_app, 
                filename = param, 
                folder = 'Fertilisation', 
                subfolder = nutrient, 
                subfolderX2 = 'Manure_application_rates',
                subfolderX3 = main_param)
  }
  
  # export manure surplus
  export_file(module = 'Nutrients', 
              file = ruminant_man_surplus, 
              filename = 'Ruminant_manure_surplus', 
              folder = 'Fertilisation', 
              subfolder = nutrient, 
              subfolderX2 = 'Manure_surplus', 
              subfolderX3 = 'Method I')
}


loop_nonfodder_crops__manure_application_rate = function(nutrient) {
  
  standard_params <- get_standard_params_list(main_param = 'Crops')

  for (i in 1:nrow(standard_params)) {
    
    main_param = standard_params[i, 'Main_crop']
    param = standard_params[i, 'Crop']
    
    if (main_param == 'Pastures' | main_param == 'Forage' | param == 'Tomato' | main_param == 'Horticulture') {
      next
    }
    else {
      print(param)
      crop_man_app = compute_crop_manure_application_rate(nutrient, main_param, param)
      export_file(module = 'Nutrients', 
                  file = crop_man_app, 
                  filename = param, 
                  folder = 'Fertilisation', 
                  subfolder = nutrient, 
                  subfolderX2 = 'Manure_application_rates',
                  subfolderX3 = 'Method I',
                  subfolderX4 = main_param)
    }
  }
}

loop_manure_application_rates = function(nutrient) {
  
  loop_nonfodder_crops__manure_application_rate(nutrient)
  loop_fodder_crops_manure_application_rate(nutrient)
}




# MANURE SPREADING CROP ALLOCATION METHOD II -------------------------------------------------------------------------------------------------------

#* HIERARCHICAL APPROACH
#** priority was given to forage crops > intensitve pastures > cereals > potatoes > vineyard + olive groves > other permanent crops > pulses (generally speaking)
#*** Use the same allocation approach based on Serra et al (2019) assumptions on fodder/non_fodder crops and ruminants/non_ruminant manure allocation
#*** Allocate manure according to the hieraarchy established
#**** update manure nutrient allocated accordingly 
#**** higher priority crops receive the largest amounts of manure
#**** if in the end there is still yet some manure nutrient yet to be allocated, this is considered the manure surplus

# ASSUMPTIONS:
# ** fooder crops' nutrient demand can only be totally fulfilled by manure 
# ** for non-fodder crops, Veltho fet al 2009 assumption of setting the maximum manure application rate of 50%


compute_total_gross_manure_animalType = function(animal_type, nutrient) {
  # computes the total nutrient content of manure to be allocated for fodder and non-fodder crops
  #unit: kg nutrient yr.-1
  
  gross_man_solid = compute_animal_type_manure_spreading(animal_type = animal_type, nutrient = nutrient, manure_type = 'Solid')
  gross_man_slurry = compute_animal_type_manure_spreading(animal_type = animal_type, nutrient = nutrient, manure_type = 'Slurry')
  
  yrs = paste0('X',seq(1987,2017))
  gross_man_solid[, yrs] = sapply(yrs, function(x) round( gross_man_solid[, x] + gross_man_slurry[, x], 1))
  
  return(gross_man_solid)
}



methodII_fodder_nutrientReq_minus_Man = function(nutrient, main_param, param, man_surplus_df) {
  # similar to calc1_crop_nutrientReq_minus_Man, but it receives the gross man allocated as an argument
  # computes two things:
  #** 1 - manure surplus if the crop nutrient demand is exceeded by the manure application
  #** 2 - the remaining crop nutrient demand following application 
  # output: index is the remaining nutrient demand; index 2 is the manure surplus if it exists
  # unit: kg nutrient yr-1
  
  nutrient_req = get_activity_data(module = 'Nutrients', mainfolder =  'Output', folder = 'Fertilisation', subfolder = nutrient, subfolderX2 = 'Crop_requirements', subfolderX3 = main_param, pattern = param)

  # calculation: Nutrient_requirement - Gross_man_allocated_nutrient

  for (i in 1:nrow(nutrient_req)) {
    
    for (j in 4:ncol(nutrient_req)) {
      
      if ( ( nutrient_req[i,j] > man_surplus_df[i,j] ) == TRUE) {
        
        nutrient_req[i,j] = round (nutrient_req[i,j] - man_surplus_df[i,j] , 1)
        man_surplus_df[i,j] = 0
      }
      else if ( ( nutrient_req[i,j] == man_surplus_df[i,j] ) == TRUE) {
        
        nutrient_req[i,j] = 0
        man_surplus_df[i,j] = 0
      }
      else {
        
        nutrient_req[i,j] = 0
        man_surplus_df[i,j] = round ( man_surplus_df[i,j] - nutrient_req[i,j] , 1)
      }
    }
  }
  
  # remaining nutrient demand
  remaining_nutrient_req = compute_manure_surplus(nutrient_req)[[1]]

  return(list(remaining_nutrient_req = remaining_nutrient_req, man_surplus_df = man_surplus_df))
}

tot_nutrient_req = get_activity_data(module = 'Nutrients', mainfolder =  'Output', folder = 'Fertilisation', subfolder = 'N', subfolderX2 = 'Crop_requirements', subfolderX3 = 'Forage', pattern = 'forage_maize')
man = methodII_fodder_nutrientReq_minus_Man('N','Forage','forage_maize', fodder_gross_man)[[2]]
rem = methodII_fodder_nutrientReq_minus_Man('N','Forage','forage_maize', fodder_gross_man)[[1]]
N_app = tot_nutrient_req[, yrs] - rem[, yrs]

sum(fodder_gross_man$X1987)/1e6
sum(N_app$X1987)/1e6
sum(man$X1987)/1e6



methodII_nonFodder_nutrientReq_minus_man = function(nutrient='N', main_param='Cereals', param='Irrigated_maize', man_surplus_df) {
  # similar to methodII_fodder_nutrientReq_minus_Man
  # however, crop nutrient demand of nonfodder crops cannot exceed 50% of total crop nutrient demand
  # unit: kg nutrient yr-1
  
  
  tot_nutrient_req = get_activity_data(module = 'Nutrients', mainfolder =  'Output', folder = 'Fertilisation', subfolder = nutrient, subfolderX2 = 'Crop_requirements', subfolderX3 = main_param, pattern = param)
  nutrient_req = tot_nutrient_req
  
  yrs = paste0('X', seq(1987,2017))
  
  for (i in 1:nrow(nutrient_req)) {
    
    for (j in 4:ncol(nutrient_req)) {
      
      threshold_condition = 0.5 * nutrient_req[i,j]
      
      if ( (man_surplus_df[i,j] < threshold_condition ) == TRUE) {
        # *** CONDITION I ----------------------- 
        # where crop nutrient demand - manure applied <= 0.5 * nutrient demand --> condition satisfied
        
        nutrient_req[i,j] = round ( nutrient_req[i,j] - man_surplus_df[i,j] , 1)
        
        # set manure surplus to 0
        man_surplus_df[i, j] = 0
      }
    
      else {
        # *** CONDITION II ----------------------- 
        # remaining ids

       nutrient_req[i,j] = round ( nutrient_req[i,j] * 0.5 , 1)
       
       # update manure surplus 
       man_surplus_df[i, j] =  round ( man_surplus_df[i, j] - nutrient_req[i,j] , 1)
      }
    }
  }

  remaining_nutrient_req = compute_manure_surplus(nutrient_req)[[1]]
  
  # update man_surplus_df
  #man_surplus_df[, yrs] = sapply(yrs, function(x) round( man_surplus[,x] - (tot_nutrient_req[, x] - remaining_nutrient_req[, x]) , 1))
  #man_surplus_df[man_surplus_df<0] = 0

  return(list(remaining_nutrient_req = remaining_nutrient_req, man_surplus = man_surplus_df))
}







compute_methodII_nutrient_flows = function(nutrient) {
  
  man_app_hierarchy = get_activity_data(module = 'Nutrients', folder = 'General_params', subfolder = 'Crops', subfolderX2 = 'Fertilisers', subfolderX3 = 'Manure_crop_distribution', pattern = 'Crop_hierarchy')
  
  # initial gross manure for fodder and non-fodder crops
  # this will progressively decrease as manure is allocated according to the crop hierarchy established
  fodder_gross_man = compute_total_gross_manure_animalType('ruminants',nutrient)
  nonFodder_gross_man = compute_total_gross_manure_animalType('non_ruminants',nutrient)
  
  for (i in 1:nrow(man_app_hierarchy)) {
    
    main_param = man_app_hierarchy[i, 'main_crops']
    param = man_app_hierarchy[i, 'crops']
    animal_type = man_app_hierarchy[i, 'animal_division']
    
    crop_area =  get_activity_data(module = 'Nutrients', folder = 'Correct_data_Municipality', subfolder = 'Areas', subfolderX2 = main_param, pattern = param)
    yrs = paste0('X', seq(1987,2017))
    
    # clean manure surplus accumulated dataframe for inconsistencies (i.e., negative values)
    fodder_gross_man = data_cleaning(fodder_gross_man)
    
    if (animal_type == 'ruminants') {
      
      # 1 - calculates remaining crop nutrient remands;
      # 2- computes manure surplus 
      # 3 - calculates crop manure application rates
      # 4 - update gross manure from ruminants
      nut_req = methodII_crop_nutrientReq_minus_Man(nutrient, main_param, param)[[1]]
      man_surplus = methodII_crop_nutrientReq_minus_Man(nutrient, main_param, param, fodder_gross_man)[[2]]
      man_app_rate = compute_crop_manure_application_rate(nutrient, main_param, param, nutrient_remaining_df = nut_req)
      fodder_gross_man[, yrs] = sapply(yrs, function(x) round ( fodder_gross_man[,x] - man_app_rate[,x] * crop_area[,x] + man_surplus[,x], 1 ))
      
      # export 
      export_file(module = 'Nutrients', 
                  file = man_app_rate, 
                  filename = param, 
                  folder = 'Fertilisation', 
                  subfolder = nutrient, 
                  subfolderX2 = 'Manure_application_rates',
                  subfolderX3 = 'Method II',
                  subfolderX4 = main_param)
    }
    
    else {
      # 1 - calculates remaining crop nutrient remands;
      # 2- computes manure surplus 
      # 3 - calculates crop manure application rates
      # 4 - update gross manure from non_ruminants
      
      nut_req = methodII_nonFodder_nutrientReq_minus_man(nutrient, main_param, param, nonFodder_gross_man)[[1]]
      man_surplus = methodII_crop_nutrientReq_minus_Man(nutrient, main_param, param, nonFodder_gross_man)[[2]]
      man_app_rate = compute_crop_manure_application_rate(nutrient, main_param, param, nut_req)
      nonFodder_gross_man[, yrs] = sapply(yrs, function(x) round ( nonFodder_gross_man[,x] - man_app_rate[,x] * crop_area[,x] + man_surplus[,x], 1 ))
      
      # export 
      export_file(module = 'Nutrients', 
                  file = man_app_rate, 
                  filename = param, 
                  folder = 'Fertilisation', 
                  subfolder = nutrient, 
                  subfolderX2 = 'Manure_application_rates',
                  subfolderX3 = 'Method II',
                  subfolderX4 = main_param)
    }
  }
  
  # export manure surplus left
  export_file(module = 'Nutrients', 
              file = fodder_gross_man, 
              filename = 'Ruminant_manure_surplus', 
              folder = 'Fertilisation', 
              subfolder = nutrient, 
              subfolderX2 = 'Manure_surplus', 
              subfolderX3 = 'Method II')
  export_file(module = 'Nutrients', 
              file = nonFodder_gross_man, 
              filename = 'Non_ruminant_manure_surplus', 
              folder = 'Fertilisation', 
              subfolder = nutrient, 
              subfolderX2 = 'Manure_surplus', 
              subfolderX3 = 'Method II')
}
compute_methodII_nutrient_flows('N')

