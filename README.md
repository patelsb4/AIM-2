# AIM-2
The analysis examines the effects of extreme heat on emergency department visits among pregnant and postpartum individuals enrolled in Medicaid in Philadelphia (2016–2019).

Scripts:
0_edvisits2.R: Loads and cleans emergency department visit data for the pregnant and postpartum Medicaid population in Philadelphia (2016–2019).
0_tempcleaning.R: Loads and cleans city-level temperature data in Philadelphia (2016-2019)
1_casecrossv2.R:Defines a function to generate the case-crossover dataset by identifying control days matched to each case day within the same day of week, month, and year.
2_dlnm_ns_func.R:Defines a function for exploring various knot placements in the DLNMs with up to 7-day lags.
3_run_dlnm.R: Executes the DLNM models
3_cts.qmd: Creates a time series structure with strata for day of week, month, and year and runs DLNMs
