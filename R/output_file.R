#' Path Generator
#'
#' @description Generate absoulte path to the output dataset
#' @param group_name String of group name from c("7yr","6gr","7gr","8gr","9gr",
#' "10gr","11gr","12gr")
#' @param suffix String of table type from c("coverage_final",
#' "reasons_for_Non_Imms")
#' @param root_dir String of root dirctory; defult: "result"
#'
#' @return Absolute path including the file name to the result dataset
#' @export
#'
#' @examples
#' output_file("7yr", "coverage_final")
#' output_file("12gr", "reasons_for_Non_Imms")
#' output_file("12gr", "reasons_for_Non_Imms","test_result_newcode")
#'
output_file <- function(group_name,suffix,root_dir = "result"){
  if(!is.character(group_name) |
     !is.character(suffix) |
     !is.character(root_dir)){
    stop("`group_name`, `suffix` and `root_dir` should be strings")
  }
  here::here(paste0(root_dir,"/VCHA_",group_name,"_",suffix,".xlsx"))
}
