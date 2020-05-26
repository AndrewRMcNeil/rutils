#' @export
`%not_in%` <- negate(`%in%`)

#' @export
not_null <- negate(is.null)

#' @export
not_na <- negate(is.na)

#' @export
assert_not_na <- as_assertion(not_na)

#' @export
n_unique <- compose(length, unique)

#' @export
add_class <- function(.object, .class) {

  assert_character(.class)

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

  assert_posixct(.dttm)

  wday(.dttm, label = TRUE) %in% c("Sat", "Sun")

}

#' @export
not_weekend <- negate(is_weekend)

#' @export
floor_month <- function(.dttm) {

  assert_posixct(.dttm, len = 1,
                 any.missing = FALSE)

  floor_date(.dttm, unit = "P1M")

}

#' @export
floor_week <- function(.dttm) {

  assert_posixct(.dttm, len = 1,
                 any.missing = FALSE)

  floor_date(.dttm, unit = "P1W",
             week_start = 1)

}

#' @export
empty_tbl <- function(.tbl) {

  isTRUE(nrow(.tbl) == 0)

}
