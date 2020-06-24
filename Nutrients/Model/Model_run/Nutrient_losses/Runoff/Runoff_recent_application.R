source('./Nutrients/Model/Soil_losses/Runoff/Nutrient_runoff_recentApplication.R')

# Runoff fraction approach I - land use allocation -----------------------------------------------

# recent memory for grazing and field application of fertilisers 
# average f_runoff at the municipality level for the period 1987-2017
# unit: %N_input
loop_avg_muni_runoff_field_application_LU(spatial_res = '500')



# Nutrient runoff losses approach I - land use allocation ----------------------------------------

# GRAZING ---------------------------------------------
# unit: kg nutrient yr-1
loop_recentMemory_runoff_nutrient_grazing(nutrient, manure_surplus_fills_nutDemand, manure_method)


# FIELD APPLICATION -----------------------------------
# calculates runoff losses for each crop, each fertiliser type and each main crop param
# unit: kg nutrient yr-1
loop_recentMemory_runoff_nutrient_fieldApplication(nutrient, manure_surplus_fills_nutDemand, manure_method)



# TOTAL RUNOFF LOSSES ----------------------------------
# unit: kg nutrient yr-1
compute_total_runoff_nutrient_losses(nutrient, manure_surplus_fills_nutDemand, manure_method)