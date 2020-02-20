#################################################################
##            script data cleaning Swiss First data            ##
#################################################################

setwd("C:/Users/EricM/ownCloud/HUG/Projet sinergia Margitta/Data_analysis/Swiss_first")

# Load the functions
load_functions <- function(){
  source("function_divide_forms.R")
  source("reshaping_data/reshape_eligibility.R")
  source("sanity_check_functions/SC_eligibility.R")
  source("reshaping_data/reshape_ed_first_cs.R")
  source("sanity_check_functions/SC_ed_first_cs.R")
  source("reshaping_data/reshape_workup.R")
  source("sanity_check_functions/SC_workup.R")
}
load_functions()
  

# Divides the continuous data into the different forms as present in the redcap
divide_forms(data = data)

# Reshaping the data for eligibility

reshape_eligibility(eligibility = eligibility)
reshape_ed_first_cs(ed_first_consult = ed_first_consult)
reshape_workup(work_up = work_up)

# sanity_check

SC_eligibility(eligibility = eligibility)
SC_ed_first.cs(ed_first_consult = ed_first_consult)
SC_workup(work_up = work_up)

# Export sanity check information
library(writexl)
write_xlsx(SC_eligibility_output, "SC_outputs/SC_eligibility_output.xlsx")
write_xlsx(SC_ed_first_output, "SC_outputs/SC_ed_first_output.xlsx")
write_xlsx(SC_workup_output, "SC_outputs/SC_workup_output.xlsx")
