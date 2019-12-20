# Reshaping the data for the eligibility form

reshape_eligibility <- function(eligibility) {
  require(lubridate)
eligibility$ic1 <<- NULL
eligibility$ic2 <<- NULL
eligibility$ic3 <<- NULL
eligibility$sex <<- factor(eligibility$sex)
eligibility$ic_date <<- as_date(eligibility$ic_date)
}