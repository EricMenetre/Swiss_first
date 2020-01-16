##################################################################
##                        SC_ed_first_cs                        ##
##################################################################

SC_ed_first.cs <- function(ed_first_consult){
  
  # Definition of a data frame with the columns to check to indicate where are the problems
  pat_number <- 1:nrow(ed_first_consult)
  SC_ed_first <- data.frame(pat_number)
  SC_ed_first$date.1st.consultation <- NA
  SC_ed_first$date.1st.consultation_comment <- NA
  SC_ed_first$date.1st.sz <- NA
  SC_ed_first$date.1st.sz_comment <- NA
  SC_ed_first$date.cerebral_img <- NA
  SC_ed_first$date.cerebral_img_comment <- NA
  
  
  # Check that all the dates are coherent
  for(i in 1:nrow(ed_first_consult)){
    # Date of first consultation
    if(is.na(ed_first_consult$date.1st.consultation[i])){
      SC_ed_first$date.1st.consultation[i] <- FALSE
      SC_ed_first$date.1st.consultation_comment[i] <- "No date"
    } else if(as.numeric(ed_first_consult$date.1st.consultation[i]) < as.numeric(ed_first_consult$date.1st.sz[i])){
      SC_ed_first$date.1st.consultation[i] <- FALSE
      SC_ed_first$date.1st.consultation_comment[i] <- "Date 1st consult. before date first sz or no date"
    } else if(as.numeric(ed_first_consult$date.1st.consultation[i]) < as.numeric(ed_first_consult$date.current_episode[i])){
      SC_ed_first$date.1st.consultation[i] <- FALSE
      SC_ed_first$date.1st.consultation_comment[i] <- "Date 1st consult. before date current episode or no date"
    } else {
      SC_ed_first$date.1st.consultation[i] <- TRUE
    }
    # Date of the first seizure
    if(is.na(ed_first_consult$date.1st.sz[i])){
      SC_ed_first$date.1st.sz[i] <- FALSE
      SC_ed_first$date.1st.sz_comment[i] <- "No date"
    } else if(as.numeric(ed_first_consult$date.1st.sz[i]) > as.numeric(ed_first_consult$date.1st.consultation[i])){
      SC_ed_first$date.1st.sz[i] <- FALSE
      SC_ed_first$date.1st.sz_comment[i] <- "Date first sz after date 1st consult. or no date"
    } else if(as.numeric(ed_first_consult$date.1st.sz[i]) > as.numeric(ed_first_consult$date.current_episode[i])) {
      SC_ed_first$date.1st.sz[i] <- FALSE
      SC_ed_first$date.1st.sz_comment[i] <- "Date of 1st sz after date of the current episode or no current episode date"
    }
    # Date of the cerebral img
    
    if(is.na(ed_first_consult$date.cerebral_img[i])){
      SC_ed_first$date.cerebral_img[i] <- FALSE
      SC_ed_first$date.cerebral_img_comment[i] <- "No date"
    } else if (is.na(ed_first_consult$suspicious_imaging[i])) {
      SC_ed_first$date.cerebral_img[i] <- FALSE
      SC_ed_first$date.cerebral_img_comment[i] <- "Date for imagery but no result"
    } else {
      SC_ed_first$date.cerebral_img[i] <- TRUE
    }
    
    # Date of the standard EEG
    
    if(is.na(ed_first_consult$date.std.eeg[i])) {
      
    }
    
  }
  
  
  
}