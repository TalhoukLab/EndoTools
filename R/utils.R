# magrittr placeholder
globalVariables(".")

#' Simulate ACE variables
#' @noRd
simulate_ace <- function(symptoms, size, prob) {
  assertthat::assert_that(length(symptoms) == length(prob),
                          sum(prob) <= 0.13)
  base_prob <- c(prob, 0.87)
  all_symptoms <- c(symptoms, "none")
  prob <-
    purrr::map2(base_prob, lengths(all_symptoms), ~ rep((.x / .y), .y)) %>%
    purrr::flatten_dbl()
  x <- purrr::flatten_chr(all_symptoms)
  sample(x = x, size = size, replace = TRUE, prob = prob)
}
