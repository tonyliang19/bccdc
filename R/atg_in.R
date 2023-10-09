atg_in <- function(df, HA = TRUE, atg_in, group_name, grade_label = ""){

  gradeage = substr(group_name,
                    stringr::str_length(group_name) - 1,
                    stringr::str_length(group_name))


  if (gradeage == "gr"){
    if (HA ==TRUE) {
      df %>%
        dplyr::filter(df$antigen %in% atg_in
                      & df$HSDA == "VCH"
                      & df$grade == grade_label)%>%
        dplyr::select(-1)
    } else if (HA ==FALSE) {
      df %>%
        dplyr::filter(df$antigen %in% atg_in
                      & df$HSDA != "VCH"
                      & df$grade == grade_label)%>%
        dplyr::select(-1)
    } else {
      stop("HA is TRUE for HA, FALSE for HSDA")
    }
  } else if (gradeage == "yr"){
    if (HA ==TRUE){
      df %>%
        dplyr::filter(df$antigen %in% atg_in
                      & df$HSDA == "VCH")
    } else if (HA ==FALSE) {
      df %>%
        dplyr::filter(df$antigen %in% atg_in
                      & df$HSDA != "VCH")
    } else {
      stop("HA is TRUE for HA, FALSE for HSDA")
    }
  } else {
    stop("choose from 7yr,6gr,7gr,8gr,9gr,10gr,11gr,12gr")
  }
}
