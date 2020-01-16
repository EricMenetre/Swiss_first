#################################################################
##                     reshape ed_first_cs                     ##
#################################################################


reshape_ed_first_cs <- function(ed_first_consult){
  require(dplyr)
  require(lubridate)
  # Rename the unclear columns and change the date format
  ed_first_consult <<- ed_first_consult%>%
    rename(aura.gust_olf = others___1,
           aura.sensitive = others___2,
           aura.epigastric = others___3,
           aura.motor = others___4,
           aura.deja_vu = others___5,
           aura.other = others___6,
           contr_fact.OH_intox = contributing_factor___1,
           contr_fact.OH_with = contributing_factor___2,
           contr_fact.faver = contributing_factor___3,
           contr_fact.BZD_with = contributing_factor___4,
           contr_fact.antibio = contributing_factor___5,
           contr_fact.other_drugs = contributing_factor___6,
           contr_fact.high_BP = contributing_factor___7,
           contr_fact.sleep_depr = contributing_factor___8,
           lesion_img.vasc = specification2___1,
           lesion_img.infec = specification2___2,
           lesion_img.tumor = specification2___3,
           lesion_img.trauma = specification2___4,
           lesion_img.malformation = specification2___5,
           lesion_img.other = specification2___6,
           planned_exams.MRI = planned_examinations___1,
           planned_exams.LT_EEG = planned_examinations___2,
           planned_exams.LP = planned_examinations___3,
           planned_exams.other = planned_examinations___4,
           drug.VPA = drug___1,
           drug.LEV = drug___2,
           drug.LTG = drug___3,
           drug.CBZ = drug___4,
           drug.PHT = drug___5,
           drug.LCM = drug___6,
           drug.other = drug___7,
           drug.none = drug___8,
           medical_history.other = other_2,
           medical_history.neurological = neurological,
           medical_history.cardiac = card_disease,
           medical_history.psychiatric = psych_disorder,
           medical_history.psych_retar = psychomotor_retardation,
           medical_history.febrile_conv = febrile_convulsions,
           family_history.epilepsy = epilepsy,
           family_history.psychiatric = psychiatric_disorder,
           family_history.other = other_family_history_or_de,
           other_drug.dose = dose2,
           std_EEG.result = normal,
           date.current_episode = current_episode,
           date.1st.consultation = date_of_the_first_consulta,
           date.1st.sz = date_of_first_seizure,
           date.std.eeg = standart_eeg,
           date.cerebral_img = cerebral_imaging)%>%
    mutate(date.1st.consultation = as_date(date.1st.consultation),
           date.1st.sz = as_date(date.1st.sz),
           date.std.eeg = as_date(date.std.eeg),
           date.cerebral_img = as_date(date.cerebral_img))%>%
    select(-age_at_first_seizure)
  
  ed_first_consult$pat_code <<- eligibility$pat_id
}


