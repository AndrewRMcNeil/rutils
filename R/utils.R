#' @export
`%not_in%` <- purrr::negate(`%in%`)

#' @export
not_null <- purrr::negate(is.null)

#' @export
not_na <- purrr::negate(is.na)

#' @export
as_assertion <- function(.predicate) {

  function(...) {
    checkmate::assert_true(.predicate(...))
  }

}

#' @export
assert_not_na <- as_assertion(not_na)

#' @export
n_unique <- purrr::compose(length, unique)

#' @export
add_class <- function(.object, .class) {

  checkmate::assert_character(.class)

  class_x <- class(.object)

  class(.object) <- unique(c(class_x, .class))

  .object

}

#' @export
div_zero <- function(.x, .y) {

  if (.y == 0) return(0)

  .x / .y

}

#' @export
is_weekend <- function(.dttm) {

  checkmate::assert_posixct(.dttm)

  lubridate::wday(.dttm, label = TRUE) %in% c("Sat", "Sun")

}

#' @export
not_weekend <- purrr::negate(is_weekend)

#' @export
floor_month <- function(.dttm) {

  checkmate::assert_posixct(
    .dttm,
    len = 1,
    any.missing = FALSE
  )

  lubridate::floor_date(.dttm, unit = "P1M")

}

#' @export
floor_week <- function(.dttm) {

  checkmate::assert_posixct(
    .dttm,
    len = 1,
    any.missing = FALSE
  )

  lubridate::floor_date(
    .dttm,
    unit = "P1W",
    week_start = 1
  )

}

#' @export
empty_tbl <- function(.tbl) {

  isTRUE(nrow(.tbl) == 0)

}
