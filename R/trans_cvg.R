#' Dataset Preparation
#'
#' @description Initial manipulate the dataset as per group for Coverage analysis
#' @param group_name A string selected from c("7yr","6gr","7gr","8gr","9gr",
#' "10gr","11gr","12gr")
#' @param grade_label String of grade in 2 digits converted from group_name
#' @param df Original dataset; Defult value vch_df0 is the imported raw data
#'
#' @return A list in length 9, including
#'
#' vch_df_ha: dataset for HA Coverage analysis
#'
#' vch_df_hsda: dataset for HDSA Coverage analysis
#'
#' disease_tbmutate: string of a disease to be deeper explored
#'
#' afternrows: number of the row to inserted the mutated disease
#'
#' disease_rename: string of the new name of the variant disease
#'
#' vch_hpv_ha: dataset for HA HPV related analysis
#'
#' vch_hpv_hsda: dataset for HSDA related analysis
#'
#' reason_df_ha: dataset for HA reason analysis
#'
#' reason_df_hsda: dataset for HSDA reason analysis
#'
#' @export
#' @examples
#' vch_df0 <- data.frame(
#' antigen = c("dtap", "dtap_p", "hpv_F", "hpv_M", "men", "hpv", "measle",
#' "td", "poilo"),
#' HSDA = c("Vancouver", "Richmond", "VCHA", "Richmond", "Vancouver",
#' "VCHA", "VCHA", "Vancouver", "Richmond")
#' )
#' trans_cvg("7yr")
#'
#' vch_df_g <- data.frame(
#' grade = c("06","06","07","08", "09", "10", "10", "11", "12"),
#' antigen = c("dtap", "dtap_p", "hpv_F", "hpv_M", "men", "hpv", "measle",
#' "td", "poilo"),
#' HSDA = c("Vancouver", "Richmond", "VCHA", "Richmond", "Vancouver",
#' "VCHA", "VCHA", "Vancouver", "Richmond")
#' )
#' trans_cvg("6gr", "06", vch_df_g)
#'
#'
trans_cvg <- function(group_name, grade_label = "", df = vch_df0){
  HPV_dis = c("hpv_M", "hpv_F")

  if (group_name == "7yr"){
    #case 1
    vch_df_ha <- atg_ex(df,TRUE, c("td", "pertussis"),group_name)
    vch_df_hsda <- atg_ex(df, FALSE, c("td", "pertussis"),group_name)
    disease_tbmutate <- "measles"
    disease_rename <- "measles - 1 Dose Only"
    afternrows <- 5
    vch_hpv_ha <- NA
    vch_hpv_hsda <- NA
    reason_df_ha <- atg_ex(df, TRUE,
                           c("td", "pertussis", "dtap_p"),group_name)
    reason_df_hsda <- atg_ex(df, FALSE,
                             c("td", "pertussis", "dtap_p"), group_name)

  } else if (group_name == "6gr"){
    #case 2
    vch_df_ha <- atg_ex(df, TRUE, c("dtap", "dtap_p", "hpv_F", "hpv_M"),
                        group_name, grade_label)
    vch_df_hsda <- atg_ex(df, FALSE, c("dtap", "dtap_p", "hpv_F", "hpv_M"),
                          group_name, grade_label)
    disease_tbmutate <- "hpv"
    afternrows <- 4
    disease_rename <- "hpv Series Initiation but not Completion"
    vch_hpv_ha <- atg_in(df, TRUE, HPV_dis ,group_name, grade_label)
    vch_hpv_hsda <- atg_in(df, FALSE, HPV_dis ,group_name, grade_label)
    reason_df_ha <- atg_in(df, TRUE,
                           c("polio","measles","mumps","rubella","varicella",
                             "men","hbv","hpv","hpv_M","hpv_F"),
                           group_name, grade_label)
    reason_df_hsda <- atg_in(df, FALSE,
                             c("polio","measles","mumps","rubella","varicella",
                               "men","hbv","hpv","hpv_M","hpv_F"),
                             group_name, grade_label)

  } else if (group_name == "9gr"){
    #case 3
    vch_df_ha <- atg_ex(df, TRUE, c("dtap", "dtap_p", "hpv_F", "hpv_M"),
                        group_name, grade_label)
    vch_df_hsda <- atg_ex(df, FALSE, c("dtap", "dtap_p", "hpv_F", "hpv_M"),
                          group_name, grade_label)
    disease_tbmutate <- "hpv"
    afternrows <- 3
    disease_rename <- "hpv Series Initiation but not Completion"
    vch_hpv_ha <- atg_in(df, TRUE, HPV_dis ,group_name, grade_label)
    vch_hpv_hsda <- atg_in(df, FALSE, HPV_dis ,group_name, grade_label)
    reason_df_ha <- atg_in(df, TRUE,
                           c("polio","measles","mumps","rubella","varicella",
                             "men","hbv","hpv","hpv_M","hpv_F","pertussis",
                             "td"), group_name, grade_label)
    reason_df_hsda <- atg_in(df, FALSE,
                             c("polio","measles","mumps","rubella","varicella",
                               "men","hbv","hpv","hpv_M","hpv_F","pertussis",
                               "td") ,group_name, grade_label)


  } else if (group_name %in% c("7gr", "8gr", "10gr", "11gr", "12gr")){
    #case 4
    vch_df_ha <- atg_in(df, TRUE, "hpv", group_name, grade_label)
    vch_df_hsda <- atg_in(df, FALSE, "hpv", group_name, grade_label)
    disease_tbmutate <- "hpv"
    afternrows <- 1
    disease_rename <- "hpv Series Initiation but not Completion"
    vch_hpv_ha <- atg_in(df, TRUE, HPV_dis, group_name, grade_label)
    vch_hpv_hsda <-  atg_in(df, FALSE, HPV_dis, group_name, grade_label)
    reason_df_ha <- atg_in(df, TRUE, c("hpv", "hpv_M","hpv_F"), group_name,
                           grade_label)
    reason_df_hsda <- atg_in(df, FALSE, c("hpv", "hpv_M","hpv_F"), group_name,
                             grade_label)

  } else {
    stop("Out of range")
  }

  return(
    list(vch_df_ha,
         vch_df_hsda,
         disease_tbmutate,
         afternrows,
         disease_rename,
         vch_hpv_ha,
         vch_hpv_hsda,
         reason_df_ha,
         reason_df_hsda))
}
