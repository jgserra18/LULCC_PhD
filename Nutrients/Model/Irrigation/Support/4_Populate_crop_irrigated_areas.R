source('./Nutrients/Model/Irrigation/Support/4_Populate_crop_irrigated_areas.R')


#* crop irrigated areas per irrigation method are interpolated for other years (Pseudocode):
#* 
#* if there is crop area in this municipality and also in 2009
#*     use X2009_FRACTIOn
#* else if there is crop area but not in 2009
#*     use X2009_FRACTION per agrarian region

#* check if the total irrigated crop areas are not higher than crop areas
#* if yes, pass to the next step
#* else correct the total irrigated crop area


# 1 - interpolate crop irrigated areas for the other years using 2009 fractions ------------------------


d = compute_adjusted_irrigated_areas()
