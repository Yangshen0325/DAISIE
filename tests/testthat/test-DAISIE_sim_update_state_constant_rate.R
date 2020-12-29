context("DAISIE_sim_update_state_constant_rate")

test_that("DAISIE_sim_update_state_constant_rate is correct for immigration", {
  set.seed(1)
  timeval <- 0.1
  totaltime <- 1
  possible_event <- 1
  maxspecID <- 1
  mainland_spec <- 1
  island_spec <- NULL
  stt_table <- matrix(ncol = 4)
  colnames(stt_table) <- c("Time", "nI", "nA", "nC")
  stt_table[1, ] <- c(totaltime, 0, 0, 0)

  updated_state <- DAISIE_sim_update_state_constant_rate(
    timeval = timeval,
    totaltime = totaltime,
    possible_event = possible_event,
    maxspecID = maxspecID,
    mainland_spec = mainland_spec,
    island_spec = island_spec,
    stt_table = stt_table)

  expect_equal(updated_state$island_spec,
               rbind(c("1", "1", "0.1", "I", NA, NA, NA)))
  expect_equal(updated_state$maxspecID, 1)
  expect_equal(updated_state$stt_table,
               rbind(stt_table, c(0.9, 1, 0, 0)))
})

test_that("DAISIE_sim_update_state_constant_rate is correct for anagenesis", {
  timeval <- 0.2
  totaltime <- 1
  possible_event <- 3
  maxspecID <- 1
  mainland_spec <- 1
  island_spec <- rbind(c("1", "1", "0.1", "I", NA, NA, NA))
  stt_table <- matrix(ncol = 4)
  colnames(stt_table) <- c("Time", "nI", "nA", "nC")
  stt_table[1, ] <- c(totaltime, 0, 0, 0)
  stt_table <- rbind(stt_table, c(0.9, 1, 0, 0))

  updated_state <- DAISIE_sim_update_state_constant_rate(
    timeval = timeval,
    totaltime = totaltime,
    possible_event = possible_event,
    maxspecID = maxspecID,
    mainland_spec = mainland_spec,
    island_spec = island_spec,
    stt_table = stt_table)

  expect_equal(updated_state$island_spec,
               rbind(c("2", "1", "0.1", "A", NA, NA, "Immig_parent")))
  expect_equal(updated_state$maxspecID, 2)
  expect_equal(updated_state$stt_table,
               rbind(stt_table,
                     c(0.8, 0, 1, 0)))
})

test_that("DAISIE_sim_update_state_constant_rate is correct for extinction of
          singleton", {
  timeval <- 0.3
  totaltime <- 1
  possible_event <- 2
  maxspecID <- 2
  mainland_spec <- 1
  island_spec <- rbind(c("2", "1", "0.1", "A", NA, NA, "Immig_parent"))
  stt_table <- matrix(ncol = 4)
  colnames(stt_table) <- c("Time", "nI", "nA", "nC")
  stt_table[1, ] <- c(totaltime, 0, 0, 0)
  stt_table <- rbind(stt_table,
                     c(0.9, 1, 0, 0),
                     c(0.8, 0, 1, 0))

  updated_state <- DAISIE_sim_update_state_constant_rate(
    timeval = timeval,
    totaltime = totaltime,
    possible_event = possible_event,
    maxspecID = maxspecID,
    mainland_spec = mainland_spec,
    island_spec = island_spec,
    stt_table = stt_table)

  expect_equal(updated_state$island_spec,
               island_spec[-1, ])
  expect_equal(updated_state$maxspecID, 2)
  expect_equal(updated_state$stt_table,
               rbind(stt_table,
                     c(0.7, 0, 0, 0)))
})

test_that("DAISIE_sim_update_state_constant_rate is correct for cladogenesis", {
  timeval <- 0.2
  totaltime <- 1
  possible_event <- 4
  maxspecID <- 1
  mainland_spec <- 1
  island_spec <- rbind(c("1", "1", "0.1", "I", NA, NA, NA))
  stt_table <- matrix(ncol = 4)
  colnames(stt_table) <- c("Time", "nI", "nA", "nC")
  stt_table[1, ] <- c(totaltime, 0, 0, 0)
  stt_table <- rbind(stt_table, c(0.9, 1, 0, 0))

  updated_state <- DAISIE_sim_update_state_constant_rate(
    timeval = timeval,
    totaltime = totaltime,
    possible_event = possible_event,
    maxspecID = maxspecID,
    mainland_spec = mainland_spec,
    island_spec = island_spec,
    stt_table = stt_table)

  expect_equal(updated_state$island_spec,
               rbind(c("2", "1", "0.1", "C", "A", "0.1", NA),
                     c("3", "1", "0.1", "C", "B", "0.2", NA)))
  expect_equal(updated_state$maxspecID, 3)
  expect_equal(updated_state$stt_table,
               rbind(stt_table,
                     c(0.8, 0, 0, 2)))
})

test_that("DAISIE_sim_update_state_constant_rate is correct for re-immigration", {
  skip("needs to be finished, find a case when this occurs")
})
