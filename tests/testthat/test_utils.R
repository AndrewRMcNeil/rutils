test_that("floor_month throws if .dttm is empty or NULL", {
  expect_error(floor_month(.dttm = c()))
  expect_error(floor_month(.dttm = NULL))
})

test_that("floor_month throws if .dttm is not of class POSIXct", {
  expect_error(floor_month(.dttm = 1))
})

test_that("floor_month returns value with same timezone as input", {
  dttm_utc <- ymd_hms("2020-01-01 00:00:00", tz = "UTC")
  dttm_est <- ymd_hms("2020-01-01 00:00:00", tz = "EST")
  expect_equal(tz(dttm_utc), tz(floor_month(dttm_utc)))
  expect_equal(tz(dttm_est), tz(floor_month(dttm_est)))
})

test_that("floor_month returns .dttm when .dttm is first of month", {
  dttm <- ymd_hms("2020-01-01 00:00:00", tz = "UTC")
  expect_equal(dttm, floor_month(dttm))
})

test_that("floor_month returns first day of month", {
  dttm <- ymd_hms("2020-01-10 00:00:00", tz = "UTC")
  dttm_first_day <- ymd_hms("2020-01-01 00:00:00", tz = "UTC")
  expect_equal(floor_month(dttm), dttm_first_day)
})

test_that("floor_week throws if .dttm is empty or NULL", {
  expect_error(floor_week(.dttm = c()))
  expect_error(floor_week(.dttm = NULL))
})

test_that("floor_week throws if .dttm is not of class POSIXct", {
  expect_error(floor_week(.dttm = 1))
})

test_that("floor_week returns value with same timezone as input", {
  dttm_utc <- ymd_hms("2020-01-01 00:00:00", tz = "UTC")
  dttm_est <- ymd_hms("2020-01-01 00:00:00", tz = "EST")
  expect_equal(tz(dttm_utc), tz(floor_week(dttm_utc)))
  expect_equal(tz(dttm_est), tz(floor_week(dttm_est)))
})

test_that("floor_week return value is a Monday", {
  expect_equal(
    weekdays(floor_week(.dttm = ymd_hms("2020-01-01 00:00:00", tz = "UTC"))),
    "Monday")
})

test_that("floor_week returns .dttm when .dttm is first day of week", {
  dttm <- ymd_hms("2020-01-01 00:00:00", tz = "UTC")
  dttm_first_day <- ymd_hms("2019-12-30 00:00:00", tz = "UTC")
  expect_equal(floor_week(.dttm = dttm), dttm_first_day)
})

test_that("empty_tbl returns FALSE if a tibble has any rows", {
  expect_false(empty_tbl(tibble(a = 1)))
})

test_that("empty_tbl returns TRUE if a tibble has 0 rows", {
  expect_true(empty_tbl(tibble(a = 1)[0,]))
})

test_that("is_weekend throws if .dttm is not of class POSIXct", {

  expect_error(
    is_weekend(1))

})

test_that("is_weekend returns TRUE if .dttm is during a weekend", {

  dttm <- ymd_hms("2020-03-15 00:00:00", tz = "UTC")

  expect_true(
    is_weekend(.dttm = dttm))

})

test_that("is_weekend returns FALSE if .dttm is not during a weekend", {

  dttm <- ymd_hms("2020-03-17 00:00:00", tz = "UTC")

  expect_false(
    is_weekend(.dttm = dttm))

})

test_that("ISO_to_POSIXct throws if .iso_dttm is not of class character", {
  
  expect_error(
    ISO_to_POSIXct(.iso_dttm = 1)
  )
  
})

test_that("ISO_to_POSIXct throws if .iso_dttm is empty", {
  
  expect_error(
    ISO_to_POSIXct(.iso_dttm = character(0))
  )
  
})

test_that("ISO_to_POSIXct return value is of class POSIXct", {
  
  iso_dttm <- "2020-01-01T00:00:00+0000"
  
  expect_class(
    ISO_to_POSIXct(.iso_dttm = iso_dttm),
    classes = "POSIXct"
  )
  
})

test_that("ISO_to_POSIXct return value timezone is UTC", {
  
  iso_dttm_est <- "2020-01-01T00:00:00-0500"
  
  expect_equal(
    ISO_to_POSIXct(.iso_dttm = iso_dttm_est) %>% tz(),
    "UTC"
  )
  
})

test_that("ISO_to_POSIXct throws if .iso_dttm is invalid", {
  
  invalid_iso_dttm <- "2020-01-01T25:00:00+0000"
  
  expect_error(
    ISO_to_POSIXct(.iso_dttm = invalid_iso_dttm)
  )
  
})

test_that("POSIXct_to_ISO throws if .dttm is not of class POSIXct", {
  
  expect_error(
    POSIXct_to_ISO(.dttm = "")
  )
  
})

test_that("POSIXct_to_ISO throws if .dttm is empty", {
  
  empty_dttm <- as.POSIXct("2020-01-01 00:00:00")[0]
  
  expect_error(
    POSIXct_to_ISO(.dttm = empty_dttm)
  )
  
})

test_that("POSIXct_to_ISO returns a character", {
  
  dttm <- as.POSIXct("2020-01-01 00:00:00")
  
  expect_class(
    POSIXct_to_ISO(.dttm = dttm),
    classes = "character"
  )
  
})

test_that("POSIXct_to_ISO returns expected value when .dttm is UTC", {
  
  dttm <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
  
  expect_equal(
    POSIXct_to_ISO(.dttm = dttm),
    "2020-01-01T00:00:00+0000"
  )
  
})

test_that("POSIXct_to_ISO returns expected value when .dttm is local", {
  
  dttm <- as.POSIXct("2020-01-01 00:00:00", tz = "EST")
  
  expect_equal(
    POSIXct_to_ISO(.dttm = dttm),
    "2020-01-01T00:00:00-0500"
  )
  
})

test_that("add_class returns .object if .class is 
          the original class of .object", {
  
  expect_equal(
    1 %>% class(),
    1 %>% add_class("numeric") %>% class()
  )
            
})

test_that("add_class adds a new class while preserving original classes", {
  
  expect_equal(
    1 %>% add_class("test_class") %>% class(),
    c("numeric", "test_class")
  )
  
})