#################################################################
##                          SC_workup                          ##
#################################################################

SC_workup <- function(work_up){
  library(lubridate)
  
  patients_number <- 1:nrow(work_up)
  summary_SC <- data.frame(patients_number)
  summary_SC$month_sz_numerical <- NA
  summary_SC$month_sz_numerical_comment <- NA
  summary_SC$new_events_N_events_dates <- NA
  summary_SC$new_events_N_events_dates_comment <- NA
  summary_SC$MRI_done_date <- NA
  summary_SC$night_eeg_done_date <- NA
  summary_SC$LP_done_date <- NA
  summary_SC$psy_counc_date <- NA
  summary_SC$other_exam_specification <- NA
  summary_SC$other_exam_specification_comment <- NA
  summary_SC$two_dates_night_eeg <- NA
  summary_SC$two_dates_night_eeg_comment <- NA
  summary_SC$results_night_eeg <- NA
  summary_SC$results_night_eeg_comment <- NA
  summary_SC$lesion_MRI <- NA
  summary_SC$lesion_MRI_comment <- NA
  summary_SC$localization_MRI <- NA
  summary_SC$localization_MRI_comment <- NA
  summary_SC$AED_coherence <- NA
  summary_SC$AED_coherence_comment <- NA
  summary_SC$AED_continue_specify <- NA
  summary_SC$AED_continue_other_speicified <- NA
  summary_SC$AED_continue_other_speicified_comment <- NA
  summary_SC$AED_new_specify <- NA
  summary_SC$AED_new_specify_comment <- NA
  summary_SC$diagnosis_subtype <- NA
  summary_SC$diagnosis_subtype_comment <- NA
  summary_SC$diagnosis_subtype_spec <- NA
  summary_SC$diagnosis_subtype_spec_comment <- NA
  
  for(i in 1:nrow(work_up)){
    # Month after the seizure is not numerical
    if(is.na(work_up$months_after_first_seizure[i])){
      summary_SC$month_sz_numerical[i] <- TRUE
    } else if (is.na(as.numeric(work_up$months_after_first_seizure[i]))){
      summary_SC$month_sz_numerical[i] <- FALSE
      summary_SC$month_sz_numerical_comment[i] <- "The months after seizure format is not numerical"
    } else {
      summary_SC$month_sz_numerical[i] <- TRUE
    }

    # if new events, check that !is.na(dates and number of new events)
    if(is.na(work_up$new_events2_workup[i])){
      summary_SC$new_events_N_events_dates[i] <- TRUE
    } else if(work_up$new_events2_workup[i] == 1){
      if(is.na(work_up$number_of_new_events_workup[i]) | is.na(work_up$dates.new.events[i])){
        summary_SC$new_events_N_events_dates[i] <- FALSE
        summary_SC$new_events_N_events_dates[i] <- "Number of new events not numerical or no dates for new events entered "
      } else {
        summary_SC$new_events_N_events_dates[i] <- TRUE
      }
    } else {
      summary_SC$new_events_N_events_dates[i] <- TRUE
    }
    # Number of new events id not numerical
    if(is.na(work_up$number_of_new_events_workup[i])){
      summary_SC$new_events_numerical[i] <- TRUE
    } else if(is.na(as.numeric(work_up$number_of_new_events_workup[i]))){
      summary_SC$new_events_numerical[i] <- FALSE
    } else{
      summary_SC$new_events_numerical[i] <- TRUE
    }
    # if exams complete, check that dates are not missing
      # MRI
    if(work_up$exams_done.MRI[i] == 1 & is.na(work_up$date_mri_workup[i])){
      summary_SC$MRI_done_date[i] <- FALSE
    } else {
      summary_SC$MRI_done_date[i] <- TRUE
    }
      # Night EEG
    if(work_up$exams_done.Night_EEG[i] == 1 & is.na(work_up$date_night_eeg_workup[i])){
      summary_SC$MRI_done_date[i] <- FALSE
    } else {
      summary_SC$MRI_done_date[i] <- TRUE
    }
      # LP
    if(work_up$exams_done.LP[i] == 1 & is.na(work_up$date_lp_workup[i])){
      summary_SC$LP_done_date[i] <- FALSE
    } else {
      summary_SC$LP_done_date[i] <- TRUE
    }
      # Psychiatric council
    if(work_up$exams_done.psy_council[i] == 1 & is.na(work_up$date_psychiatric_council_workup[i])){
      summary_SC$psy_counc_date[i] <- FALSE
    } else {
      summary_SC$psy_counc_date[i] <- TRUE
    }
      # other exam
    if(work_up$exams_done.other[i] == 1 & is.na(work_up$other_exam_specification[i])){
      summary_SC$other_exam_specification[i] <- FALSE
      summary_SC$other_exam_specification_comment[i] <- "other exams is selected but no specification were entered"
    } else {
      summary_SC$other_exam_specification[i] <- TRUE
    }
    # Dates for night EEG must match
    if(!is.na(work_up$date_night_eeg_workup[i])){
      if(as_date(work_up$date_night_eeg_workup[i]) != as_date(as.character(work_up$date.night.EEG_2[i]))) {
        summary_SC$two_dates_night_eeg[i] <- FALSE
        summary_SC$two_dates_night_eeg_comment[i] <- "The two dates for the night EEG do not match"
      } else {
        summary_SC$two_dates_night_eeg[i] <- TRUE
      }
    } else {
      summary_SC$two_dates_night_eeg[i] <- TRUE
    }
    # if normal night EEG = 0 --> no missing data in slow and spikes
    if(!is.na(work_up$date_night_eeg_workup[i]) & is.na(work_up$normal_night_eeg_workup[i])){
      summary_SC$results_night_eeg[i] <- FALSE
      summary_SC$results_night_eeg_comment[i] <- "Night EEG date specified but no results entered"
    } else if(is.na(work_up$normal_night_eeg_workup[i])){
      summary_SC$results_night_eeg[i] <- TRUE
    } else if(work_up$normal_night_eeg_workup[i] == 0 & is.na(work_up$night.eeg.slow[i])){
      summary_SC$results_night_eeg[i] <- FALSE
      summary_SC$results_night_eeg_comment[i] <- "Abnormal night EEG but unspecified slow activity field"
    } else if(work_up$normal_night_eeg_workup[i] == 0 & is.na(work_up$night.eeg.spikes[i])){
      summary_SC$results_night_eeg[i] <- FALSE
      summary_SC$results_night_eeg_comment[i] <- "Abnormal night EEG but unspecified spike activity field"
    } else {
      summary_SC$results_night_eeg[i] <- TRUE
    }
    # if MRI is not normal, lesion must be specified
    sum_lesion <- sum(work_up$lesion.MRI_workup.vasc[i],
                      work_up$lesion.MRI_workup.infec[i],
                      work_up$lesion.MRI_workup.tumoral[i],
                      work_up$lesion.MRI_workup.trauma[i],
                      work_up$lesion.MRI_workup.hipp_scl[i],
                      work_up$lesion.MRI_workup.other[i])
    
    if(is.na(work_up$normal_mri_workup[i])){
      summary_SC$lesion_MRI[i] <- TRUE
    } else if (work_up$normal_mri_workup[i] == 0 & sum_lesion == 0){
      summary_SC$lesion_MRI[i] <- FALSE
      summary_SC$lesion_MRI_comment[i] <- "MRI marked as abnormal but no lesion selected"
      
    } else {
      summary_SC$lesion_MRI[i] <- TRUE
    }
    # if lesion was specified for MRI, localization must be specified
      # Vascular
    sum_localization <- sum(work_up$local.MRI_workup.frontal[i],
                            work_up$local.MRI_workup.left[i],
                            work_up$local.MRI_workup.occipital[i],
                            work_up$local.MRI_workup.right[i],
                            work_up$local.MRI_workup.parietal[i],
                            work_up$local.MRI_workup.temporal[i])
    
    if (sum_lesion != 0 & sum_localization == 0){
      summary_SC$localization_MRI[i] <- FALSE
      summary_SC$localization_MRI_comment[i] <- "Lesion selected but no localization entered"
    } else {
      summary_SC$localization_MRI[i] <- TRUE
    }
    # if AED = 1, continue AED mut be different from NA
    if(is.na(work_up$aeds_workup[i])){
      summary_SC$AED_coherence[i] <- TRUE
    }else if(work_up$aeds_workup[i] == 1 & is.na(work_up$continue_aed_2_workup[i])){
      summary_SC$AED_coherence[i] <- FALSE
      summary_SC$AED_coherence_comment[i] <- "AED taken, indicate if treatment continues or not"
    } else {
      summary_SC$AED_coherence[i] <- TRUE
    }
    # if continue AED == 1, !is.na(AED taken)
    sum_continue_AED <- sum(work_up$aed_taken.workup.VPA[i],
                            work_up$aed_taken.workup.LEV[i],
                            work_up$aed_taken.workup.LTG[i],
                            work_up$aed_taken.workup.CBZ[i],
                            work_up$aed_taken.workup.PHT[i],
                            work_up$aed_taken.workup.LCM[i],
                            work_up$aed_taken.workup.other[i])
    if(is.na(work_up$continue_aed_2_workup[i])){
      summary_SC$AED_continue_specify[i] <- TRUE
    } else if(work_up$continue_aed_2_workup[i] == 1 & sum_continue_AED == 0){
      summary_SC$AED_continue_specify[i] <- FALSE
    } else {
      summary_SC$AED_continue_specify[i] <- TRUE
    }
    # if other medication, specify which medication
    if (is.na(work_up$aed_taken.workup.other[i])){
      summary_SC$AED_continue_other_speicified[i] <- TRUE
    } else if(work_up$aed_taken.workup.other[i] == 1 &is.na(work_up$what_other_medication_workup[i])){
      summary_SC$AED_continue_other_speicified[i] <- FALSE
      summary_SC$AED_continue_other_speicified_comment[i] <- "other treatment selected, need to specify which one"
    } else {
      summary_SC$AED_continue_other_speicified[i] <- TRUE
    }
    # if diagnosis = 1 --> !is.na(subtype)
    if(is.na(work_up$diagnosis_follow_up_workup[i])){
      summary_SC$diagnosis_subtype[i] <- TRUE
    } else if (work_up$diagnosis_follow_up_workup[i] == 1){
        if(is.na(work_up$subtype1_workup[i]) | work_up$subtype1_workup[i] == 0){
          summary_SC$diagnosis_subtype[i] <- FALSE
          summary_SC$diagnosis_subtype_comment[i] <- "Diagnosis is epilepsy but unspecified subtype"
        } else {
          summary_SC$diagnosis_subtype[i] <- TRUE
        }
    } else {
      summary_SC$diagnosis_subtype[i] <- TRUE
    }
    # if subtype = 2 or 3, !is.na(localization)
    sum_subtype <- sum(work_up$local.epi.left.frontal[i],
                       work_up$local.epi.left.parietal[i],
                       work_up$local.epi.left.temporal[i],
                       work_up$local.epi.left.occipital[i],
                       work_up$local.epi.right.frontal[i],
                       work_up$local.epi.right.parietal[i],
                       work_up$local.epi.right.temporal[i],
                       work_up$local.epi.right.occipital[i])
    if(is.na(work_up$subtype1_workup[i])){
      summary_SC$diagnosis_subtype_spec[i] <- TRUE
    } else if(work_up$subtype1_workup[i] == 2 | work_up$subtype1_workup[i] == 3){
      if(sum_subtype == 0){
        summary_SC$diagnosis_subtype_spec[i] <- FALSE
        summary_SC$diagnosis_subtype_spec_comment[i] <- "Focal epilepsy specified but unspecified localization of the foyer"
      } else {
        summary_SC$diagnosis_subtype_spec[i] <- TRUE
      }
    } else {
      summary_SC$diagnosis_subtype_spec[i] <- TRUE
    }
    
    # if new medication == 1 !is.na(new medication)
    sum_new_medication <- sum(work_up$aed_new.workup.VPA[i],
                              work_up$aed_new.workup.LEV[i],
                              work_up$aed_new.workup.LTG[i],
                              work_up$aed_new.workup.CBZ[i],
                              work_up$aed_new.workup.PHT[i],
                              work_up$aed_new.workup.LCM[i],
                              work_up$aed_new.workup.other[i])
    if(is.na(work_up$new_medication_follow_up_workup[i])){
      summary_SC$AED_new_specify[i] <- TRUE
    } else if(work_up$new_medication_follow_up_workup[i] == 1 &
              sum_new_medication == 0){
      summary_SC$AED_new_specify[i] <- FALSE
      summary_SC$AED_new_specify_comment[i] <- "New AED prescribed, specify which one"
    } else {
      summary_SC$AED_new_specify[i] <- TRUE
    }
  }
  
  # Preparing the output
  patient_number <- 1:nrow(eligibility)
  output <- as.data.frame(patient_number)
  output$incoherent_fields <- NA
  
  for (i in 1:nrow(summary_SC)){
    names <- colnames(summary_SC)
    ind <- which(summary_SC[i,] == FALSE)
    fields_problem <- names[ind]
    for(j in 1:length(fields_problem)){
      for(k in 1:length(names)){
        if(names[k] == paste(fields_problem[i], "_comment", sep = "")){
          fields_problem[j] <- paste(fields_problem[j], ":",summary_SC[i,k])
        }
      }
    } 
    if (length(fields_problem) == 0){
      output$incoherent_fields[i] <- "No problem detected"
    } else {
      output$incoherent_fields[i] <- do.call(paste, c(as.list(fields_problem), sep = " ; "))
    }
  }
  
  SC_workup_output <<- output
  
  }
  