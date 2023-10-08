var_assign <- function(group_name, cnames0, Disease0) {
  stopifnot(is.character(group_name),
            is.character(cnames0),
            length(cnames0) <= 1,
            is.character(Disease0),
            length(Disease0) < 19)
  if (group_name == "7yr"){
    group_sheet = "1. 7y_BCCDC"
    cnames = cnames0 # n = 25
    Disease_coverage = Disease0[-c(5,6,7,8,13, 16)]# n = 13
    Disease_reason = Disease0[-c(1,3,5,6,7,8,10,13,16,18,19)]# n = 8
  } else if (group_name == "6gr"){
    group_sheet = "2. Grade_BCCDC"
    cnames = append("grade", cnames0) # n = 26
    Disease_coverage = c(Disease0[1],"D_T", Disease0[-c(1,2,3,7,8,10, 16)])# n = 14
    #!!!"D_T" is "td"!!! above
    Disease_reason = Disease0[c(4:5,7:9,11:12,14:15,17)]
  } else if (group_name == "7gr"){
    group_sheet = "2. Grade_BCCDC"
    cnames = append("grade", cnames0) # n = 26
    Disease_coverage = Disease0[c(5,6)]# n = 2
    Disease_reason = Disease0[c(5,7:8)]# n =3
  }else if (group_name %in% c("8gr", "10gr", "11gr", "12gr")){
    group_sheet = "4. Grade_BCCDC_noint"
    cnames = append("grade", cnames0) # n = 26
    Disease_coverage = Disease0[c(5,6)]# n = 2
    Disease_reason = Disease0[c(5,7:8)]# n =3
  }else if (group_name == "9gr"){
    group_sheet = "4. Grade_BCCDC_noint"
    cnames = append("grade", cnames0) # n = 26
    Disease_coverage = c(Disease0[c(1,4,5,6,9)],"Meningococcal quadrivalent", Disease0[c(12:19)])# n = 14
    #!!!"men" is "Meningococcal quadrivalent" above!!!
    Disease_reason = Disease0[c(4:5,7:9,11:17)]
  }else{
    stop("The group should be in either 7-year-old or 6-12grade")
  }

  return(list(group_sheet,cnames, Disease_coverage, Disease_reason))

}
