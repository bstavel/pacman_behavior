mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  # this function is like mutate but only acts on the rows satisfying the condition #
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}