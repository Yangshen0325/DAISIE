context("DAISIE_rates_mainland_ex")

test_that("update_rates_mainland_ex produces correct output", {
  rates <- update_rates_mainland_ex(timeval = 1,
                                    totaltime = 10,
                                    gam = 1,
                                    laa = 1,
                                    lac = 1,
                                    mu = 1,
                                    K = 10,
                                    num_spec = 1,
                                    num_immigrants = 1,
                                    mainland_n = 1,
                                    island_spec = NULL)
  expect_true(is.list(rates))
  expect_length(rates, 4)
  expect_equal(names(rates), c("immig_rate", "ext_rate",
                               "ana_rate", "clado_rate"))
  expect_equal(rates, list(immig_rate = 0.9,
                           ext_rate = 1,
                           ana_rate = 1,
                           clado_rate = 0.9))
})

test_that("abuse update_rates_mainland_ex", {
  expect_error(update_rates_mainland_ex(timeval = "string",
                                        totaltime = 10,
                                        gam = 1,
                                        laa = 1,
                                        lac = 1,
                                        mu = 1,
                                        K = 10,
                                        num_spec = 1,
                                        num_immigrants = 1,
                                        mainland_n = 1,
                                        island_spec = NULL))

  expect_error(update_rates_mainland_ex(timeval = 1,
                                        totaltime = "string",
                                        gam = 1,
                                        laa = 1,
                                        lac = 1,
                                        mu = 1,
                                        K = 10,
                                        num_spec = 1,
                                        num_immigrants = 1,
                                        mainland_n = 1,
                                        island_spec = NULL))

  expect_error(update_rates_mainland_ex(timeval = 1,
                                        totaltime = 10,
                                        gam = "string",
                                        laa = 1,
                                        lac = 1,
                                        mu = 1,
                                        K = 10,
                                        num_spec = 1,
                                        num_immigrants = 1,
                                        mainland_n = 1,
                                        island_spec = NULL))

  expect_error(update_rates_mainland_ex(timeval = 1,
                                        totaltime = 10,
                                        gam = 1,
                                        laa = "string",
                                        lac = 1,
                                        mu = 1,
                                        K = 10,
                                        num_spec = 1,
                                        num_immigrants = 1,
                                        mainland_n = 1,
                                        island_spec = NULL))

  expect_error(update_rates_mainland_ex(timeval = 1,
                                        totaltime = 10,
                                        gam = 1,
                                        laa = 1,
                                        lac = "string",
                                        mu = 1,
                                        K = 10,
                                        num_spec = 1,
                                        num_immigrants = 1,
                                        mainland_n = 1,
                                        island_spec = NULL))

  expect_error(update_rates_mainland_ex(timeval = 1,
                                        totaltime = 10,
                                        gam = 1,
                                        laa = 1,
                                        lac = 1,
                                        mu = "string",
                                        K = 10,
                                        num_spec = 1,
                                        num_immigrants = 1,
                                        mainland_n = 1,
                                        island_spec = NULL))

  expect_error(update_rates_mainland_ex(timeval = 1,
                                        totaltime = 10,
                                        gam = 1,
                                        laa = 1,
                                        lac = 1,
                                        mu = 1,
                                        K = "string",
                                        num_spec = 1,
                                        num_immigrants = 1,
                                        mainland_n = 1,
                                        island_spec = NULL))

  expect_error(update_rates_mainland_ex(timeval = 1,
                                        totaltime = 10,
                                        gam = 1,
                                        laa = 1,
                                        lac = 1,
                                        mu = 1,
                                        K = 10,
                                        num_spec = "string",
                                        num_immigrants = 1,
                                        mainland_n = 1,
                                        island_spec = NULL))

  expect_error(update_rates_mainland_ex(timeval = 1,
                                        totaltime = 10,
                                        gam = 1,
                                        laa = 1,
                                        lac = 1,
                                        mu = 1,
                                        K = 10,
                                        num_spec = 1,
                                        num_immigrants = "string",
                                        mainland_n = 1,
                                        island_spec = NULL))

  expect_error(update_rates_mainland_ex(timeval = 1,
                                        totaltime = 10,
                                        gam = 1,
                                        laa = 1,
                                        lac = 1,
                                        mu = 1,
                                        K = 10,
                                        num_spec = 1,
                                        num_immigrants = 1,
                                        mainland_n = "string",
                                        island_spec = NULL))
})
