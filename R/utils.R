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

#' @export
valid_hms <- function(.hms) {

  valid_string <- stringr::str_detect(
    string = .hms,
    pattern = "^[0-9]{2}:[0-9]{2}:[0-9]{2}$"
  )

  suppressWarnings({
    valid_time <- not_na(
      hms::as_hms(.hms)
    )
  })

  valid_string && valid_time

}

#' @export
valid_label_ids <- function(.label_id) {

  stringr::str_detect(
    string = .label_id,
    pattern = "^\\[([0-9],)*[0-9]{0,1}\\]$"
  )

}

#' @export
valid_oui <- function(.oui) {

  is.na(.oui) |
    stringr::str_detect(
      string = .oui,
      pattern = "^\\[([0-9],)*[0-9]\\]$"
    )

}

#' @export
valid_logical <- function(.logical) {

  isTRUE(.logical) || isFALSE(.logical)

}

#' @export
valid_tz <- function(.tz) {

  isTRUE(.tz %in% OlsonNames())

}

#' @export
POSIXct_to_sf_dttm <- function(.dttm) {

  checkmate::assert_posixct(.dttm)
  checkmate::assert_true(lubridate::tz(.dttm) == "UTC")

  strftime(
    .dttm,
    format = "%Y-%m-%d %H:%M:%S+00",
    tz = "UTC"
  )

}

#' @export
POSIXct_to_ISO <- function(.dttm) {

  checkmate::assert_posixct(
    .dttm,
    min.len = 1,
    any.missing = FALSE
  )

  strftime(.dttm, format = "%Y-%m-%dT%H:%M:%S%z",
           tz = lubridate::tz(.dttm))

}

#' @export
ISO_to_POSIXct <- function(.iso_dttm) {

  checkmate::assert_character(
    .iso_dttm,
    min.len = 1,
    any.missing = FALSE
  )

  suppressWarnings({
    dttm <- lubridate::as_datetime(.iso_dttm)
  })

  assert_not_na(dttm)

  dttm

}

#' @export
assert_utc <- function(.dttm) {

  lubridate::tz(.dttm) == "UTC"

}

#' @export
object_name <- purrr::compose(deparse, substitute)
