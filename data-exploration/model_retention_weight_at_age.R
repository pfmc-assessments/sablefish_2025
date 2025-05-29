#===============================================================================
# Retention Model - Weight-at-Age
#===============================================================================
watage_ret <- SS_output(here::here("model", "_retention_model", "_weight_at_age", "22.0_revised_fleet_structure"))
# gradiant = 0.000405473
# NLL = 8.18141e+06
# R0 = 8.44943
# M = 0.115879
# Population crashes!!!!!!!!!!!!!!!!!!!!!!
# Triennial Q = 
# NWFSC Slope Q = 
# WCGBT Q =

r4ss::tune_comps(
  replist = watage_ret , 
  dir = here::here("model", "_retention_model", "_weight_at_age", "22.0_revised_fleet_structure"),
  option = "Francis")