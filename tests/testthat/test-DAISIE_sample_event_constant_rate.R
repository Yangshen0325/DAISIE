context("DAISIE_sample_event_constant_rate")

test_that("DAISIE_sample_event_constant_rate produces correct output", {
  set.seed(1)
  rates <- list(immig_rate = 1,
                ext_rate = 1,
                ana_rate = 1,
                clado_rate = 1)
  event <- DAISIE_sample_event_constant_rate(rates = rates)
  expect_equal(event, 3)
})

test_that("abuse DAISIE_sample_event_constant_rate", {
  expect_error(DAISIE_sample_event_constant_rate(
    rates = list(immig_rate = "string",
                 ext_rate = 1,
                 ana_rate = 1,
                 clado_rate = 1)))
  expect_error(DAISIE_sample_event_constant_rate(
    rates = list(immig_rate = 1,
                 ext_rate = "string",
                 ana_rate = 1,
                 clado_rate = 1)))
  expect_error(DAISIE_sample_event_constant_rate(
    rates = list(immig_rate = 1,
                 ext_rate = 1,
                 ana_rate = "string",
                 clado_rate = 1)))
  expect_error(DAISIE_sample_event_constant_rate(
    rates = list(immig_rate = 1,
                 ext_rate = 1,
                 ana_rate = 1,
                 clado_rate = "string")))
})
