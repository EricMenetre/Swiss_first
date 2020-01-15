#################################################################
##            script data cleaning Swiss First data            ##
#################################################################

# Load the functions
load_functions <- function(){
  source("function_divide_forms.R")
  source("reshaping_data/reshape_eligibility.R")
  source("sanity_check_functions/SC_eligibility.R")
}
load_functions()
  

# Divides the continuous data into the different forms as present in the redcap
divide_forms(data = data)

# Reshaping the data for eligibility

reshape_eligibility(eligibility = eligibility)

# sanity_check data eligibility 

rm(ed_first_consult, end_study_or_withdrawal, first_cs_after_ed, follow_up_2y, work_up)
SC_eligibility(eligibility = eligibility)


