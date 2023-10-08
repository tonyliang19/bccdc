#' Variable Assignment
#'
#' @description Select sheet, columns and diseases for Coverage & Reasons
#' analysis per age/grade group
#'
#' @param group_name A string selected from
#' c("7yr","6gr","7gr","8gr","9gr","10gr","11gr","12gr")
#'
#' @param cnames0 A list of strings including all column names of
#' all the spreadsheets in the dataset
#'
#' @param Disease0 A list of strings including all disease, variation and notes
#'  related to Coverage among all ages&grades in the dataset
#'
#' @return A list of assigned variables in length of 4, including
#'
#' group_sheet, cnames, Disease_coverage and Disease_reason as per group
#'
#' group_sheet: String of the sheet name in the dataset to be analyzed
#'
#' cnames: Vector of the codable column names in selected sheet
#'
#' Disease_coverage: Vector of readable diseases&variants&notes to be listed in the Coverage file
#'
#' Disease_reason: Vector of readable diseases to be listed in the NonImmReason file
#'
#' @export
#'
#' @examples
#' cols = c("antigen", "HSDA", "students", "refusal_all.n",
#' "refusal_all.percent", "no_imm_record.n", "no_imm_record.percent",
#' "utd_all.n", "utd_all.percent", "utd.n", "utd.percent", "immunity.n",
#' "immunity.percent", "partial_contra.n", "partial_contra.percent",
#' "partial_refusal.n", "partial_refusal.percent", "partial_other.n",
#' "partial_other.percent", "unimm_contra.n", "unimm_contra.percent",
#' "unimm_refusal.n", "unimm_refusal.percent", "unimm_other.n",
#' "unimm_other.percent")
#'
#' dis0 = c("1UptoDate","D_T_aP", "D_T_aP_IPV", "HepB", "HPV",
#' "HPV Series Initiation but not Completion", "HPV-Female", "HPV-Male",
#' "Measles", "Measles - 1 Dose Only", "Meningo", "Mumps", "Pertussis", "Polio",
#' "Rubella", "Td", "Varicella", "xNo Information", "yRefusalstoAll&NoImms")
#'
#' var_assign("7gr", cols, dis0)
#' var_assign("7yr", cols, dis0)
#' var_assign("12gr", cols, dis0)
#'
var_assign <- function(group_name, cnames0, Disease0) {
  stopifnot(is.character(group_name),
            is.character(cnames0),
            length(cnames0) > 1,
            is.character(Disease0),
            length(Disease0) >= 19)
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
