source('./Nutrients/Model/INE_DB/Populate_INE_AgrarianRegion.R')

# scrapes data from Portugal Statistics database at the agrarian region ---------------------------------------------------------------


# CROP DATA ----------------------------------------


# get data for permanent pastures ------------------
populate_perma_pastures_DB()

# get crop areas for some crop classes  ------------
# (forage, intensive pastures)
populate_other_crops_param_DB()

# get crop yield and areas -------------------------
# EXCEPTIONS: FORAGE, HORTICULTURE, POULTRY, RABBITS
main_populate_crop_param_DB()

# correct missing values after scraping ------------
correct_all_missing_values()

# temporal interpolation of crop data --------------
loop_interpolate_other_crops_timeseries()



# ANIMAL DATA --------------------------------------

# get animal population data -----------------------
main_populate_animals_pop_DB()

# correct missing data -----------------------------
correct_animal_population_DB()
correct_animal_population_numbers()

# temporal interpolation animal population ---------
loop_interpolate_animals()

# disaggregate laying and reproductive hens --------
disaggregate_poultry_hens()



