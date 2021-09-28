check_input <- function(param, codelist) {
  param <- enquo(param)
  lvls <- levels(rlang::eval_tidy(param))
  chk <- lvls %in% codelist
  invalid <- paste(sQuote(lvls[!chk]), collapse = ", ")
  param_name <- paste0("`", rlang::as_name(param), "`")
  assertthat::assert_that(all(chk), msg = paste0("Invalid values in ", param_name, ": ", invalid))
}

tmp <- emdb$stage_full
check_input(tmp, STAGE_STD)
