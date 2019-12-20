# Sanity check function for the eligibility data

# NEED TO DEBUG

# Check that the patient's code is correct

library(stringr)

for(i in 1:2){
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

if (code_decomp[2] == "BE" |
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

if (code_decomp [3] == "Insel" |
    code_decomp[3] == "KS" |
    code_decomp[3] == "US" |
    code_decomp[3] == "Ghent" |
    code_decomp[3] == "HU" |
    code_decomp[3] == "EOC") {
  check_code[3] <- TRUE
} else {
  check_code[3] <- FALSE
}

if (as.numeric(code_decomp[4]) >0 ) {
  check_code[4] <- TRUE
} else {
  check_code[4] <- FALSE
}

if (code_decomp[5] == "m" |
    code_decomp[5] == "M" |
    code_decomp[5] == "f" |
    code_decomp[5] == "F") {
  check_code[5] <- TRUE
} else {
  check_code[5] <- FALSE
}

if (as.numeric(code_decomp[6]) > 18 & as.numeric(code_decomp[6]) < 105) {
  check_code[6] <- TRUE
} else {
  check_code[6] <- FALSE
}

summary_check_code <- NA
if (sum(check_code) == 6) {
  summary_check_code[i] <- TRUE
} else {
  summary_check_code[i] <- FALSE
}
}
