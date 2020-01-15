#################################################################
##            script data cleaning Swiss First data            ##
#################################################################

# Load the functions
load_functions <- function(){
  source("function_divide_forms.R")
  source("reshaping_data/reshape_eligibility.R")
  source("sanity_check_functions/SC_eligibility.R")
  source("reshaping_data/reshape_ed_first_cs.R")
}
load_functions()
  

# Divides the continuous data into the different forms as present in the redcap
divide_forms(data = data)

# Reshaping the data for eligibility

reshape_eligibility(eligibility = eligibility)
reshape_ed_first_cs(ed_first_consult = ed_first_consult)


# sanity_check data eligibility 

SC_eligibility(eligibility = eligibility)


