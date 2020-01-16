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
  SC_ed_first$date.std_EEG <- NA
  SC_ed_first$date.std_EEG_comment <- NA
  SC_ed_first$site <- NA
  SC_ed_first$medical_history <- NA
  SC_ed_first$medical_history_comment <- NA
  SC_ed_first$aura <- NA
  SC_ed_first$aura_comment <- NA
  SC_ed_first$contrib_fact <- NA
  SC_ed_first$contrib_fact_comment <- NA
  SC_ed_first$trly_first_episode <- NA
  SC_ed_first$total_sz <- NA
  SC_ed_first$neurostatus <- NA
  SC_ed_first$standard_eeg_res <- NA
  SC_ed_first$standard_eeg_res_comment <- NA
  SC_ed_first$laboratory <- NA
  SC_ed_first$laboratory_comment <- NA
  SC_ed_first$cerebral_imaging <- NA
  SC_ed_first$cerebral_imaging_comment <- NA
  SC_ed_first$diagnosis <- NA
  SC_ed_first$diagnosis_comment <- NA
  SC_ed_first$drugs <- NA
  SC_ed_first$drugs_commment <- NA
  SC_ed_first$dose_VPA <- NA
  SC_ed_first$dose_LEV <- NA
  SC_ed_first$dose_LTG <- NA
  SC_ed_first$dose_CBZ <- NA
  SC_ed_first$dose_PHT <- NA
  SC_ed_first$dose_LCM <- NA
  SC_ed_first$dose_other <- NA
  SC_ed_first$dose_VPA_comment <- NA
  SC_ed_first$dose_LEV_comment <- NA
  SC_ed_first$dose_LTG_comment <- NA
  SC_ed_first$dose_CBZ_comment <- NA
  SC_ed_first$dose_PHT_comment <- NA
  SC_ed_first$dose_LCM_comment <- NA
  SC_ed_first$dose_other_comment <- NA
  SC_ed_first$unfit_to_drive <- NA
  SC_ed_first$consent <- NA
  
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
      SC_ed_first$date.std_EEG[i] <- FALSE
      SC_ed_first$date.std_EEG_comment[i] <- "No date"
    } else if(is.na(ed_first_consult$std_EEG.result[i])){
      SC_ed_first$date.std_EEG[i] <- FALSE
      SC_ed_first$date.std_EEG_comment[i] <- "Results entered but no EEG dates"
    } else if (is.na(ed_first_consult$date.1st.sz[i])){
        SC_ed_first$date.std_EEG[i] <- TRUE
    } else if (as.numeric(ed_first_consult$date.std.eeg[i]) < as.numeric(ed_first_consult$date.1st.sz[i])) {
      SC_ed_first$date.std_EEG[i] <- FALSE
      SC_ed_first$date.std_EEG_comment[i] <- "EEG before the first seizure"
    } else {
      SC_ed_first$date.std_EEG[i] <- TRUE
    }
    
    # Check that the site is not NA
    
    if (is.na(ed_first_consult$site[i])){
      SC_ed_first$site[i] <- FALSE
    } else {
      SC_ed_first$site[i] <- TRUE
    }
    
    # Check that medical history is not NA and values are coherent
    ed_first_consult$medical_history.psychiatric[ed_first_consult$medical_history.psychiatric == ""] <- NA
    ed_first_consult$medical_history.other[ed_first_consult$medical_history.other == ""] <- NA
    if (is.na(ed_first_consult$medical_history[i])){
      SC_ed_first$medical_history[i] <- FALSE
      SC_ed_first$medical_history_comment[i] <- "Missing data"
    } else if (is.na(ed_first_consult$medical_history.neurological[i])&
               is.na(ed_first_consult$medical_history.cardiac[i])&
               is.na(ed_first_consult$medical_history.psychiatric[i])&
               is.na(ed_first_consult$medical_history.other[i])&
               is.na(ed_first_consult$medical_history.psych_retar[i])&
               is.na(ed_first_consult$medical_history.febrile_conv[i])&
               ed_first_consult$medical_history[i] != 0){
      SC_ed_first$medical_history[i] <- FALSE
      SC_ed_first$medical_history_comment[i] <- "medical history specified but no medical history"
    } else if (!is.na(ed_first_consult$medical_history.neurological[i])&
               !is.na(ed_first_consult$medical_history.cardiac[i])&
               !is.na(ed_first_consult$medical_history.psychiatric[i])&
               !is.na(ed_first_consult$medical_history.other[i])&
               !is.na(ed_first_consult$medical_history.psych_retar[i])&
               !is.na(ed_first_consult$medical_history.febrile_conv[i])){
      if (ed_first_consult$medical_history[i] != 0){
        SC_ed_first$medical_history[i] <- FALSE
        SC_ed_first$medical_history_comment[i] <- "medical history is 1 but nothing is specified"
        
      }
    } else {
      SC_ed_first$medical_history[i] <- TRUE
    }
    
    # Check that aura is not NA and if the values are coherent
    if (is.na(ed_first_consult$aura[i])){
      SC_ed_first$aura[i] <- FALSE
      SC_ed_first$aura_comment[i] <- "Missing value"
    } else if(ed_first_consult$aura.gust_olf[i] == 0 &
              ed_first_consult$aura.sensitive[i] == 0&
              ed_first_consult$aura.epigastric[i] == 0&
              ed_first_consult$aura.motor[i] == 0&
              ed_first_consult$aura.deja_vu[i] == 0&
              ed_first_consult$aura.other[i] == 0&
              ed_first_consult$aura[i] != 0){
      SC_ed_first$aura[i] <- FALSE
      SC_ed_first$aura_comment[i] <- "aura specified but aura set as no in the aura column"
    } else if(ed_first_consult$aura.gust_olf[i] != 0 |
             ed_first_consult$aura.sensitive[i] != 0|
             ed_first_consult$aura.epigastric[i] != 0|
             ed_first_consult$aura.motor[i] != 0|
             ed_first_consult$aura.deja_vu[i] != 0|
             ed_first_consult$aura.other[i] != 0){
      if (ed_first_consult$aura[i] != 0){
        SC_ed_first$aura[i] <- FALSE
        SC_ed_first$aura_comment[i] <- "aura specified but aura set as no in the aura column"
      }

    } else {
      SC_ed_first$aura[i] <- TRUE
    }
    
    #Check that contributing factor is not NA and that values are coherent
    if (is.na(ed_first_consult$contributing_factors[i])){
      SC_ed_first$contrib_fact[i] <- FALSE
      SC_ed_first$contrib_fact_comment[i] <- "Missing value"
    } else if (ed_first_consult$contr_fact.OH_intox[i] == 0&
               ed_first_consult$contr_fact.BZD_with[i] == 0&
               ed_first_consult$contr_fact.antibio[i] == 0&
               ed_first_consult$contr_fact.other_drugs[i] == 0&
               ed_first_consult$contr_fact.high_BP[i] == 0&
               ed_first_consult$contr_fact.sleep_depr[i] == 0&
               ed_first_consult$contributing_factors[i] != 0){
      SC_ed_first$contrib_fact[i] <- FALSE
      SC_ed_first$contrib_fact_comment[i] <- "Contributing factor set as present but not specified"
    } else if  (ed_first_consult$contr_fact.OH_intox[i] != 0&
                ed_first_consult$contr_fact.BZD_with[i] != 0&
                ed_first_consult$contr_fact.antibio[i] != 0&
                ed_first_consult$contr_fact.other_drugs[i] != 0&
                ed_first_consult$contr_fact.high_BP[i] != 0&
                ed_first_consult$contr_fact.sleep_depr[i] != 0&
                ed_first_consult$contributing_factors[i] == 0){
      SC_ed_first$contrib_fact[i] <- FALSE
      SC_ed_first$contrib_fact_comment[i] <- "Contributing factor specified but not set as present"
    } else {
      SC_ed_first$contrib_fact[i] <- TRUE
    }
    
    # Truly first episode
    if (is.na(ed_first_consult$truly_first_episode[i])){
      SC_ed_first$trly_first_episode[i] <- FALSE
    } else {
      SC_ed_first$trly_first_episode[i] <- TRUE
    }
    
    # Neuro status
    if(is.na(ed_first_consult$neurostatus[i])){
      SC_ed_first$neurostatus[i] <- FALSE
    } else {
      SC_ed_first$neurostatus[i] <- TRUE
    }
    
    # Std EEG results
    if (is.na(ed_first_consult$std_EEG.result[i])){
      SC_ed_first$standard_eeg_res[i] <- FALSE
      SC_ed_first$standard_eeg_res_comment[i] <- "Missing value"
    } else if (ed_first_consult$std_EEG.result[i] == 1 &
               !is.na(ed_first_consult$slowing[i])){
      SC_ed_first$standard_eeg_res[i] <- FALSE
      SC_ed_first$standard_eeg_res_comment[i] <- "Slowing and/or spikes are specified but EEG marked as normal"
    } else if (ed_first_consult$std_EEG.result[i] == 1&
               !is.na(ed_first_consult$spikes[i])){
      SC_ed_first$standard_eeg_res[i] <- FALSE
      SC_ed_first$standard_eeg_res_comment[i] <- "Slowing and/or spikes are specified but EEG marked as normal"
    }
    
    # Laboratory
    if(is.na(ed_first_consult$normal_laboratory[i])){
      SC_ed_first$laboratory[i] <- FALSE
      SC_ed_first$laboratory_comment[i] <- "Missing value"
    } else if (ed_first_consult$normal_laboratory[i] == 0 &
               is.na(ed_first_consult$abnormal_values[i])){
      SC_ed_first$laboratory[i] <- FALSE
      SC_ed_first$laboratory_comment[i] <- "laboratory abnormalities specified but labo marked as normal"
    } else if (ed_first_consult$normal_laboratory[i] == 1 &
               !is.na(ed_first_consult$abnormal_values[i])){
      SC_ed_first$laboratory[i] <- FALSE
      SC_ed_first$laboratory_comment[i] <- "laboratory marked as abnormal but abnormalities not specified"
    } else {
      SC_ed_first$laboratory[i] <- TRUE
    }
    
    # Cerebral imaging
    if (is.na(ed_first_consult$suspicious_imaging[i])){
      SC_ed_first$cerebral_imaging[i] <- FALSE
      SC_ed_first$cerebral_imaging_comment[i] <- "Missing value"
    } else if (is.na(ed_first_consult$acute_lesion[i])) {
      SC_ed_first$cerebral_imaging[i] <- FALSE
      SC_ed_first$cerebral_imaging_comment[i] <- "Missing value in Acute lesion"
    } else if (ed_first_consult$lesion_img.vasc[i] == 0&
               ed_first_consult$lesion_img.infec[i] == 0&
               ed_first_consult$lesion_img.tumor[i] == 0&
               ed_first_consult$lesion_img.trauma[i] == 0&
               ed_first_consult$lesion_img.malformation[i] == 0&
               ed_first_consult$lesion_img.other[i] == 0&
               ed_first_consult$acute_lesion[i] != 0){ ##### NEED TO CORRECT ? ed_first_consult$suspicious_imaging[i] != 0
      SC_ed_first$cerebral_imaging[i] <- FALSE
      SC_ed_first$cerebral_imaging_comment[i] <- "Lesion on the CT/MRI but no specification"
    } else if (ed_first_consult$lesion_img.vasc[i] != 0&
               ed_first_consult$lesion_img.infec[i] != 0&
               ed_first_consult$lesion_img.tumor[i] != 0&
               ed_first_consult$lesion_img.trauma[i] != 0&
               ed_first_consult$lesion_img.malformation[i] != 0&
               ed_first_consult$lesion_img.other[i] != 0&
               ed_first_consult$acute_lesion[i] == 0) { ##### NEED TO CORRECT ? ed_first_consult$suspicious_imaging[i] != 0
      SC_ed_first$cerebral_imaging[i] <- FALSE
      SC_ed_first$cerebral_imaging_comment[i] <- "Lesion specified but imagery set as normal"
    } else {
      SC_ed_first$cerebral_imaging[i] <- TRUE
    }
    
    # Diagnosis
    if(ed_first_consult$most_likely_diagnosis[i] == 2 &
       is.na(ed_first_consult$subtype[i])){
      SC_ed_first$diagnosis[i] <- FALSE
      SC_ed_first$diagnosis_comment[i] <- "The subtype of epilepsy is not specified"
    } else {
      SC_ed_first$diagnosis[i] <- TRUE
    }
    
    # Drugs
    if(ed_first_consult$drug.CBZ[i] == 0&
       ed_first_consult$drug.VPA[i] == 0&
       ed_first_consult$drug.LEV[i] == 0&
       ed_first_consult$drug.LTG[i] == 0&
       ed_first_consult$drug.PHT[i] == 0&
       ed_first_consult$drug.LCM[i] == 0&
       ed_first_consult$drug.other[i] == 0&
       ed_first_consult$drug.none[i] == 0){
      SC_ed_first$drugs[i] <- FALSE
      SC_ed_first$drugs_commment[i] <- "If no drug was prescribed, the none option should be chosen"
      } else if(ed_first_consult$drug.CBZ[i] == 1|
                ed_first_consult$drug.VPA[i] == 1|
                ed_first_consult$drug.LEV[i] == 1|
                ed_first_consult$drug.LTG[i] == 1|
                ed_first_consult$drug.PHT[i] == 1|
                ed_first_consult$drug.LCM[i] == 1|
                ed_first_consult$drug.other[i] == 1){
                  if (ed_first_consult$drug.none[i] == 1){
                    SC_ed_first$drugs[i] <- FALSE
                    SC_ed_first$drugs_commment[i] <- "If a drug has been prescribed, the none option should not be chosen"
                  } else {
                    SC_ed_first$drugs[i] <- TRUE
                  }
                } else {
                  SC_ed_first$drugs[i] <- TRUE
                }
    # Dose VPA
    if(is.na(ed_first_consult$dose_vpa[i])&
       ed_first_consult$drug.VPA[i] == 1){
      SC_ed_first$dose_VPA[i] <- FALSE
      SC_ed_first$dose_VPA_comment[i] <- "VPA prescribed but dose unspecified"
    } else {
      SC_ed_first$dose_VPA[i] <- TRUE
    }
    
    # Dose LEV
    if(is.na(ed_first_consult$dose_lev[i])&
       ed_first_consult$drug.LEV[i] == 1){
      SC_ed_first$dose_LEV[i] <- FALSE
      SC_ed_first$dose_LEV_comment[i] <- "LEV prescribed but dose unspecified"
    } else {
      SC_ed_first$dose_LEV[i] <- TRUE
    }
ed_first_consult$dose_ltg <- as.character(ed_first_consult$dose_ltg)

    # Dose LTG
    if(typeof(ed_first_consult$dose_LTG[i]) == "NULL"&
       ed_first_consult$drug.LTG[i] == 1){
      SC_ed_first$dose_LTG[i] <- FALSE
      SC_ed_first$dose_LTG_comment[i] <- "LTG prescribed but dose unspecified"
    } else {
      SC_ed_first$dose_LTG[i] <- TRUE
    }
    # Dose CBZ
    if(is.na(ed_first_consult$dose_cbz[i])&
       ed_first_consult$drug.CBZ[i] == 1){
      SC_ed_first$dose_CBZ[i] <- FALSE
      SC_ed_first$dose_CBZ_comment[i] <- "CBZ prescribed but dose unspecified"
    } else {
      SC_ed_first$dose_CBZ[i] <- TRUE
    }
    
    # Dose PHT
    if(is.na(ed_first_consult$dose_pht[i])&
       ed_first_consult$drug.PHT[i] == 1){
      SC_ed_first$dose_PHT[i] <- FALSE
      SC_ed_first$dose_PHT_comment[i] <- "PHT prescribed but dose unspecified"
    } else {
      SC_ed_first$dose_PHT[i] <- TRUE
    }
    
    # Dose LCM
    if(is.na(ed_first_consult$dose_lcm[1])&
       ed_first_consult$drug.LCM[i] == 1){
      SC_ed_first$dose_LCM[i] <- FALSE
      SC_ed_first$dose_LCM_comment[i] <- "LCM prescribed but dose unspecified"
    } else {
      SC_ed_first$dose_LCM[i] <- TRUE
    }
    # Dose other
    if(is.na(ed_first_consult$other_medication[i])&
       ed_first_consult$drug.other[i] == 1){
      SC_ed_first$dose_other[i] <- FALSE
      SC_ed_first$dose_other_comment[i] <- "VPA prescribed but dose unspecified"
    } else {
      SC_ed_first$dose_other[i] <- TRUE
    }

    # Unfit to drive
    if (ed_first_consult$unfit_to_drive[i] == 1&
        ed_first_consult$duration[i] == 0){
     SC_ed_first$unfit_to_drive[i] <- FALSE
    } else {
      SC_ed_first$unfit_to_drive[i] <- TRUE
    }

    # Consent
    if(ed_first_consult$signed_informed_consent[i] == 0){
      SC_ed_first$consent[i] <- FALSE
    } else {
      SC_ed_first$consent[i] <- TRUE
    }

  }
  
  # Summary of the sanity check
  
  patient_number <- 1:nrow(eligibility)
  output <- as.data.frame(patient_number)
  output$incoherent_fields <- NA
  
  full_names <- colnames(SC_ed_first)
  for (i in 1:nrow(eligibility)){
    temp <- SC_ed_first[i,which(SC_ed_first[i,] == FALSE)]
    fields_problem <- colnames(temp)
    for (j in 1:length(fields_problem)){
      for(k in 1:length(full_names)){
        if(full_names[k] == paste(fields_problem[j], "_comment", sep = "")){
          fields_problem[j] <- paste(fields_problem[j], SC_ed_first[i,full_names[k]],sep = " : ")
        }
      }
    }
    if (length(fields_problem) == 0){
      output[i,2] <- "No problem detected"
    } else {
      output[i,2] <- do.call(paste, c(as.list(fields_problem), sep = " ; "))
    }
  }
  
    SC_ed_first_output <<- output
  
  
}
