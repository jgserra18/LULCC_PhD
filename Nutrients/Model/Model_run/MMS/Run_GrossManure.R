
## GROSS MANURE NUTRIENT FLOWS --------------------------------------------------------------------------------------------------------------------





## NUTRIENTS: PHOSPHORUS AND NITROGEN ----------------------------------------------------------------------

## gross manure -------------------------

source('./Nutrients/Model/MMS/Gross_manure/Compute_Nutrient_excretion.R')

loop_gross_manure_nutrient()

## allocate gross manure to grazing, yards and housing ------------------
loop_manure_allocation_pathway()


## separate manure by liquid and solid ----------------------------------
loop_disaggregate_gross_manure_type()


## NUTRIENTS: CARBON  ---------------------------------------------------------------------------------------

## gross manure allocated to the different pathways and separated by manure type
loop_gross_manure_C_all_params()


## compute total (slurry + solid) for housing
loop_gross_manure_C_totals()


## compute total gross manure C excretion
loop_gross_manure_Cexcretion()



## ALL NUTRIENTS --------------------------------------------------------------------------------------------

## compute gross manure nutrient flows
loop_total_gross_manure_nutrient_flow()
loop_total_manure_nutrient_flows()
