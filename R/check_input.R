#' Check input parameters
#'
#' Check input parameters belong to accepted code list for data validation.
#'
#' Each clinical variable has a specific list of accepted values. If an invalid
#' value is provided, e.g. "IE" in stage, then an error should be thrown. The
#' purpose is to prevent situations where classification algorithms
#' such as ESMO may assign "unassignable" for these cases, and instead flag
#' the user of invalid input parameters.
#'
#' The list of accepted values for each clinical variable are stored as
#' constants in the package.
#'
#' @param param input parameter
#' @param codelist vector of accepted values for the input parameter
#' @return if input parameter contains only accepted values, return `TRUE`;
#'   otherwise, an error message is displayed indicating the invalid values in
#'   `param`
#' @author Derek Chiu
#' @export
#' @examples
#' STAGE_STD <- c("I", "IA", "IB", "IC", "II", "IIA", "IIB", "III", "IIIA",
#' "IIIB", "IIIC", "IIIC1", "IIIC2", "IV", "IVA", "IVB")
#' check_input(emdb$stage_full, STAGE_STD)
check_input <- function(param, codelist) {
  param <- rlang::enquo(param)
  lvls <- levels(factor(rlang::eval_tidy(param)))
  valid <- lvls %in% codelist
  invalids <- paste(dQuote(lvls[!valid], q = FALSE), collapse = ", ")
  param_name <- rlang::as_label(param)
  assertthat::assert_that(all(valid),
                          msg = paste0("Invalid values in `", param_name, "`: ", invalids))
}
