#' Target group and filter out excluded antigen
#'
#' @description Select correct grade / year level & HA / HSDA
#' and exclude unnecessary antigen
#'
#' @param df Original dataset
#' @param HA Boolean, indicating whether used for HA analysis or HSDA analysis
#' @param atg_ex List of all excluded antigen in the original dataset
#' @param group_name String of group name
#' @param grade_label String of grade in 2 digit converted from group_name
#'
#' @return Dataset with only correct antigen and HA / HSDA of target group
#'
#' @export
#'
#' @examples
#' df_g <- data.frame(
#' grade = c("06","06","07","08", "09", "10", "10", "11", "12"),
#' antigen = c("dtap", "dtap_p", "hpv_F", "hpv_M", "men", "hpv", "measle",
#' "td", "poilo"),
#' HSDA = c("Vancouver", "Richmond", "VCHA", "Richmond", "Vancouver",
#' "VCHA", "VCHA", "Vancouver", "Richmond")
#' )
#'
#' df_y<- data.frame(
#' antigen = c("dtap", "dtap_p", "hpv_F", "hpv_M", "men", "hpv", "measle",
#' "td", "poilo"),
#' HSDA = c("Vancouver", "Richmond", "VCHA", "Richmond", "Vancouver",
#' "VCHA", "VCHA", "Vancouver", "Richmond")
#' )
#'
#' atg_ex(df_y, TRUE, c("dtap", "dtap_p", "hpv_F", "hpv_M"), "7yr")
#' atg_ex(df_g, TRUE, c("dtap", "dtap_p", "hpv_F", "hpv_M"), "7gr","07")
#' atg_ex(df_g, FALSE, "td","12gr","12")
#'
atg_ex <- function(df, HA = TRUE, atg_ex, group_name, grade_label = ""){

  if (!is.data.frame(df)) {
    stop("`df` should be a data frame or data frame extension (e.g. a tibble)")
  }

  gradeage = substr(group_name,
                    stringr::str_length(group_name) - 1,
                    stringr::str_length(group_name))

  if (gradeage == "gr"){
    if (HA ==TRUE) {
      df %>%
        dplyr::filter(!df$antigen %in% atg_ex
                      & df$HSDA == "VCH"
                      & df$grade == grade_label) %>%
        dplyr::select(-1)
    } else if (HA ==FALSE) {
      df %>%
        dplyr::filter(!df$antigen %in% atg_ex
                      & df$HSDA != "VCH"
                      & df$grade == grade_label) %>%
        dplyr::select(-1)
    } else {
      stop("HA is TRUE for HA, FALSE for HSDA")
    }

  } else if (gradeage == "yr"){
    if (HA ==TRUE){
      df %>% dplyr::filter(!df$antigen %in% atg_ex & df$HSDA == "VCH")
    } else if (HA ==FALSE) {
      df %>% dplyr::filter(!df$antigen %in% atg_ex & df$HSDA != "VCH")
    } else {
      stop("HA is TRUE for HA, FALSE for HSDA")
    }
  } else {
    stop("choose from 7yr,6gr,7gr,8gr,9gr,10gr,11gr,12gr")
  }
}
