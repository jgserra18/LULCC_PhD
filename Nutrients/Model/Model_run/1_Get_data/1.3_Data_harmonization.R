source('./Nutrients/Model/INE_DB/Standardize_INE_params.R')


# DATA CORRECTION --------------------------------------------------------------
# Municipality scale for agricultural census years

# 1 - Area adjusment for irrigated and rainfed potatoes and maize
# 2 - Area correction for other fresh fruits, other pulses and the conversion of tomatoes to other industry crops
# 3 - corrects AG_muni params for Sows and Rep_laying hens based on AR data for the disaggregated reproductive and non-reproductive species

compute_correct_ALL_params()



# TEMPORAL AND SPATIAL INTERPOLATION ------------------------------------------

finish