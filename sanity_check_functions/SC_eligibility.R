# Sanity check function for the eligibility data

SC_eligibility <- function(eligibility){
  # Check that the patient's code is correct
  
  library(stringr)
  summary_check_code <- rep("NA", nrow(eligibility))
  sanity_eligibility <- data.frame(summary_check_code)
  sanity_eligibility$summary_check_code <- as.logical(sanity_eligibility$summary_check_code)
  sanity_eligibility$sex <- NA
  sanity_eligibility$age <- NA
  sanity_eligibility$eligibility_complete <- NA
  
  for(i in 1:nrow(eligibility)){
    code_decomp <- str_split(as.character(eligibility$pat_id[i]), "_", simplify = TRUE)
    temp <- str_extract_all(code_decomp[4], "[:digit:]")
    temp <- as.numeric(temp[[1]])
    temp <- sum(100*temp[1], 10*temp[2], temp[3])
    code_decomp[4] <- temp
    
    
    check_code <- NA
    if (code_decomp[1] == "EpiCH1st"){
      check_code[1] <- TRUE
    } else {
      check_code[1] <- FALSE
    }
    
    if (is.na(code_decomp[2])){
      check_code[2] <- FALSE
    } else if (code_decomp[2] == "BE" |
               code_decomp[2] == "GE" |
               code_decomp[2] == "AA" |
               code_decomp[2] == "BS" |
               code_decomp[2] == "GH" |
               code_decomp[2] == "TI" |
               code_decomp[2] == "SG" |
               code_decomp[2] == "ZH") {
      check_code[2] <- TRUE
    } else {
      check_code[2] <- FALSE
    }
    
    if (is.na(code_decomp[3])) {
      check_code[3] <- FALSE
    } else if (code_decomp [3] == "Insel" |
               code_decomp[3] == "KS" |
               code_decomp[3] == "US" |
               code_decomp[3] == "Ghent" |
               code_decomp[3] == "HU" |
               code_decomp[3] == "EOC") {
      check_code[3] <- TRUE
    } else {
      check_code[3] <- FALSE
    }
    
    if (is.na(code_decomp[4])){
      check_code[4] <- FALSE
    } else if (as.numeric(code_decomp[4]) >0 ) {
      check_code[4] <- TRUE
    } else {
      check_code[4] <- FALSE
    }
    
    if (is.na(code_decomp[5])) {
      check_code[5] <- FALSE
    } else if (code_decomp[5] == "m" |
               code_decomp[5] == "M" |
               code_decomp[5] == "f" |
               code_decomp[5] == "F") {
      check_code[5] <- TRUE
    } else {
      check_code[5] <- FALSE
    }
    
    if (is.na(code_decomp[6])){
      check_code[6] <- FALSE
    } else if (as.numeric(code_decomp[6]) >= 18 & as.numeric(code_decomp[6]) < 111) {
      check_code[6] <- TRUE
    } else {
      check_code[6] <- FALSE
    }
    
    summary_check_code_temp <- NA
    if (sum(check_code) == 6) {
      summary_check_code_temp <- TRUE
    } else {
      summary_check_code_temp <- FALSE
    }
    
    sanity_eligibility$summary_check_code[i] <- summary_check_code_temp
    
    # check for Sex
    
    if (is.na(code_decomp[5])) {
      sanity_eligibility$sex[i] <- FALSE
    } else if(code_decomp[5] == "m" | code_decomp[5] == "M"){
      if (eligibility$sex[i] == 1) {
        sanity_eligibility$sex[i] <- TRUE
      } else {
        sanity_eligibility$sex[i] <- FALSE
      }
    } else if (code_decomp[5] == "F" | code_decomp[5] == "f"){
      if (eligibility$sex[i] == 0) {
        sanity_eligibility$sex[i] <- TRUE
      } else {
        sanity_eligibility$sex[i] <- FALSE
      }
    } else {
      sanity_eligibility$sex[i] <- FALSE
    }
    
    # Check age
    
    if (is.na(eligibility$age[i])){
      sanity_eligibility$age[i] <- FALSE
    } else if (eligibility$age[i] >= 18 & 
               eligibility$age[i] <= 110 &
               code_decomp[length(code_decomp)] == eligibility$age[i]){
      sanity_eligibility$age[i] <- TRUE
    } else {
      sanity_eligibility$age[i] <- FALSE
    }
    
    # Eligibility complete
    
    if (is.na(eligibility$eligibility_complete[i])){
      sanity_eligibility$eligibility_complete[i] <- FALSE
    } else if (eligibility$eligibility_complete[i] == 2){
      sanity_eligibility$eligibility_complete[i] <- TRUE
    } else {
      sanity_eligibility$eligibility_complete[i] <- FALSE
    }
    
  }
  
  # Summary of the sanity check
  
  patient_number <- 1:nrow(eligibility)
  output <- as.data.frame(patient_number)
  output$incoherent_fields <- NA
  sanity_eligibility <- rename(sanity_eligibility,
                               patient_code = summary_check_code)
  
  for(i in 1:nrow(eligibility)){
    temp <- sanity_eligibility[i,which(sanity_eligibility[i,] == FALSE)]
    fields_problem <- colnames(temp)
    if (length(fields_problem) == 0){
      output[i,2] <- "No problem detected"
    } else {
      output[i,2] <- do.call(paste, c(as.list(fields_problem), sep = " ; "))
    }
  }
  
  
  
  SC_eligibility_output <<- output
}



