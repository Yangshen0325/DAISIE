#' Internal function of the DAISIE simulation
#'
#' @inheritParams default_params_doc
#' @keywords internal
DAISIE_sim_core_mainland_ex <- function(
  time,
  M,
  pars,
  mainland,
  mainland_sample_prob
) {

  #### Initialization ####
  timeval <- 0
  totaltime <- time

  mainland_spec <- as.numeric(mainland[which(
    as.numeric(mainland[, 8]) <= timeval &
      as.numeric(mainland[, 9]) > timeval), 1])
  mainland_n <- length(mainland_spec)

  maxspecID <- max(as.numeric(mainland[, 1]))
  if (nrow(mainland) > 1) {
    mainland_brts <- c(as.numeric(mainland[2:nrow(mainland), 8]), totaltime + 1)
    mainland_brts <- mainland_brts[-which(duplicated(mainland_brts))]
  } else {
    mainland_brts <- totaltime + 1
  }

  island_spec <- c()
  stt_table <- matrix(ncol = 4)
  colnames(stt_table) <- c("Time", "nI", "nA", "nC")
  stt_table[1, ] <- c(time, 0, 0, 0)

  lac <- pars[1]
  mu <- pars[2]
  K <- pars[3]
  gam <- pars[4]
  laa <- pars[5]

  num_spec <- length(island_spec[, 1])
  num_immigrants <- length(which(island_spec[, 4] == "I"))

  #### Start Monte Carlo iterations ####
  while (timeval < totaltime) {
    rates <- update_rates_mainland_ex(
      timeval = timeval,
      totaltime = totaltime,
      gam = gam,
      laa = laa,
      lac = lac,
      mu = mu,
      K = K,
      num_spec = num_spec,
      num_immigrants = num_immigrants,
      mainland_n = mainland_n
    )

    totalrate <- rates$immig_rate + rates$ext_rate +
      rates$ana_rate + rates$clado_rate
    if (totalrate != 0) {
      dt <- stats::rexp(1, totalrate)
      timeval <- timeval + dt
    } else {
      timeval <- totaltime + 1
    }

    # If a mainland speciation event has occurred since the last time step
    if (timeval > mainland_brts[1]) {
      timeval <- mainland_brts[1]
      mainland_brts <- mainland_brts[-1]
    } else {
      mainland_spec <- as.numeric(mainland[which(
        as.numeric(mainland[, 8]) <= timeval &
          as.numeric(mainland[, 9]) > timeval), 1])
      mainland_n <- length(mainland_spec)

      # Changes island species to endemic when a mainland species goes extinct
      island_state <- check_island_state(timeval = timeval,
                                         totaltime = totaltime,
                                         island_spec = island_spec,
                                         mainland = mainland,
                                         stt_table = stt_table)
      island_spec <- island_state$island_spec
      stt_table <- island_state$stt_table
      num_spec <- length(island_spec[, 1])
      num_immigrants <- length(which(island_spec[, 4] == "I"))

      if (timeval <= totaltime) {
        rates <- update_rates_mainland_ex(
          timeval = timeval,
          totaltime = totaltime,
          gam = gam,
          laa = laa,
          lac = lac,
          mu = mu,
          K = K,
          num_spec = num_spec,
          num_immigrants = num_immigrants,
          mainland_n = mainland_n
        )

        totalrate <- rates$immig_rate + rates$ext_rate +
          rates$ana_rate + rates$clado_rate
        if (totalrate == 0) {
          timeval <- totaltime + 1

        } else {

          possible_event <- DAISIE_sample_event_constant_rate(
            rates = rates
          )

          updated_state <- DAISIE_sim_update_state_constant_rate(
            timeval = timeval,
            totaltime = totaltime,
            possible_event = possible_event,
            maxspecID = maxspecID,
            mainland_spec = mainland_spec,
            island_spec = island_spec,
            stt_table = stt_table
          )

          island_spec <- updated_state$island_spec
          maxspecID <- updated_state$maxspecID
          stt_table <- updated_state$stt_table
          num_spec <- length(island_spec[, 1])
          num_immigrants <- length(which(island_spec[, 4] == "I"))
        }
      }
    }
  }
  #### Finalize STT ####
  stt_table <- rbind(
    stt_table,
    c(
      0,
      stt_table[nrow(stt_table), 2],
      stt_table[nrow(stt_table), 3],
      stt_table[nrow(stt_table), 4]
    )
  )

  island <- DAISIE_create_island_mainland_ex(
    stt_table = stt_table,
    totaltime = totaltime,
    island_spec = island_spec,
    mainland = mainland,
    mainland_sample_prob = mainland_sample_prob)
  return(island)
}
