source('./Nutrients/Model/Reference_areas/Compute_ReferenceAreas.R')

# TOTAL UTILISED AGRICULTURAL AREA AT THE MUNICIPALITY SCALE FOR 1987-2017 -----------------------------
loop_municipality_UAA()

# TOTAL ARABLE LAND AT THE MUNICIPALITY SCALE FOR 1987-2017 --------------------------------------------
# UAA minus permanent pasture
export_municipality_ArableLand()


