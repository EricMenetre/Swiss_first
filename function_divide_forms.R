#Cutting into different forms


divide_forms <- function(data) {
eligibility <<- data[,1:15]
ed_first_consult <<- data [, 16:112]
work_up <<- data[,113:197]
follow_up_2y <<- data[,198:282]
end_study_or_withdrawal <<- data[,283:288]
first_cs_after_ed <<- data[,289:342]
}