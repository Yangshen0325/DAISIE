context("check_island_state")

test_that("check_island_state produces correct output", {
  mainland <- rbind(
    c("1", "1", "0", "E", "A", NA, NA, "0", "0.292906805531114"),
    c("15", "1", "0", "E", "AA", "0.292906805531114", NA, "0.292906805531114",
      "0.541999222479509"),
    c("16", "1", "0", "C", "AB", "0.292906805531114", NA, "0.292906805531114",
      "1"))
  stt_table <- matrix(ncol = 4)
  stt_table[1, ] <- c(1, 0, 0, 0)

  island_state <- check_island_state(
    timeval = 0.05858136,
    island_spec = NULL,
    mainland = mainland,
    stt_table = stt_table)
  expect_true(is.list(island_state))
  expect_null(island_state$island_spec)
  expect_equal(island_state$stt_table, stt_table)

  mainland <- rbind(
    c("1", "1", "0", "E", "A", NA, NA, "0", "0.292906805531114"),
    c("15", "1", "0", "E", "AA", "0.292906805531114", NA, "0.292906805531114",
      "0.541999222479509"),
    c("16", "1", "0", "C", "AB", "0.292906805531114", NA, "0.292906805531114",
      "1"))
  stt_table <- matrix(ncol = 4)
  stt_table[1, ] <- c(1, 0, 0, 0)
  stt_table <- rbind(stt_table, c(0.7070932, 1, 0, 0))

  island_state <- check_island_state(
    timeval = 0.6,
    island_spec = rbind(c("15", "15", "0.292906805531114", "I", NA, NA, NA)),
    mainland = mainland,
    stt_table = stt_table)
  expect_equal(island_state$island_spec,
               rbind(c("15", "15", "0.292906805531114", "A", NA, NA,
                       "mainland_extinction")))
  stt_table[2, ] <- c(0.7070932, 0, 1, 0)
  expect_equal(island_state$stt_table, stt_table)
})
