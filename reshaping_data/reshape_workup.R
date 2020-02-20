##################################################################
##                        Reshape workup                        ##
##################################################################

reshape_workup <- function(work_up){
  library(dplyr)
  library(lubridate)
  data_temp <- work_up%>%
    rename(exams_done.MRI = exams_done_so_far_workup___1,
           exams_done.Night_EEG = exams_done_so_far_workup___2,
           exams_done.LP = exams_done_so_far_workup___3,
           exams_done.psy_council = exams_done_so_far_workup___4,
           exams_done.labo = exams_done_so_far_workup___5,
           exams_done.other = exams_done_so_far_workup___6,
           exams_done.none = exams_done_so_far_workup___7,
           local.night_eeg.slow = localization_sa_workup,
           local.night_eeg.spike = localization_sp_workup,
           lesion.MRI_workup.vasc = lesion_workup___1,
           lesion.MRI_workup.infec = lesion_workup___2,
           lesion.MRI_workup.tumoral = lesion_workup___3,
           lesion.MRI_workup.trauma = lesion_workup___4,
           lesion.MRI_workup.hipp_scl = lesion_workup___5,
           lesion.MRI_workup.other = lesion_workup___6,
           local.MRI_workup.right = localization_mri_workup___1,
           local.MRI_workup.left = localization_mri_workup___2,
           local.MRI_workup.frontal = localization_mri_workup___3,
           local.MRI_workup.temporal = localization_mri_workup___4,
           local.MRI_workup.parietal = localization_mri_workup___5,
           local.MRI_workup.occipital = localization_mri_workup___6,
           local.epi.left.frontal = left_workup___1,
           local.epi.left.parietal = left_workup___2,
           local.epi.left.temporal = left_workup___3,
           local.epi.left.occipital = left_workup___4,
           local.epi.right.frontal = right_workup___1,
           local.epi.right.parietal = right_workup___2,
           local.epi.right.temporal = right_workup___3,
           local.epi.right.occipital = right_workup___4,
           aed_taken.workup.VPA = aed_taken_workup___1,
           aed_taken.workup.LEV = aed_taken_workup___2,
           aed_taken.workup.LTG = aed_taken_workup___3,
           aed_taken.workup.CBZ = aed_taken_workup___4,
           aed_taken.workup.PHT = aed_taken_workup___5,
           aed_taken.workup.LCM = aed_taken_workup___6,
           aed_taken.workup.other = aed_taken_workup___7,
           aed_new.workup.VPA = new_aed_workup___1,
           aed_new.workup.LEV = new_aed_workup___2,
           aed_new.workup.LTG = new_aed_workup___3,
           aed_new.workup.CBZ = new_aed_workup___4,
           aed_new.workup.PHT = new_aed_workup___5,
           aed_new.workup.LCM = new_aed_workup___6,
           aed_new.workup.other = new_aed_workup___7,
           other_exam_specification = specification1_workup,
           date.night.EEG_2 = night_eeg_workup,
           night.eeg.slow = slow_activity2_workup,
           night.eeg.spikes = spikes1_workup,
           dates.new.events = dates_workup)
  
  
  data_temp$date_night_eeg_workup <- as_date(as.character(work_up$date_night_eeg_workup))
  data_temp$date_lp_workup <- as_date(work_up$date_lp_workup)
  data_temp$date_mri_workup <- as_date(as.character(work_up$date_mri_workup))
  data_temp$date_psychiatric_council_workup <- as_date(work_up$date_psychiatric_council_workup)

  work_up <<- data_temp
}
