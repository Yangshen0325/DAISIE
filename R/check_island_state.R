#' Checks if any changes on the mainland change the state of the island species
#'
#' @inheritParams default_params_doc
#'
#' @return a list
check_island_state <- function(timeval,
                               totaltime,
                               island_spec,
                               mainland,
                               stt_table) {
  if (any(island_spec[, 4] == "I")) {
    immig_spec <- island_spec[which(island_spec[, 4] == "I"), 1]
    mainland_ex_time <- mainland[which(mainland[, 1] %in% immig_spec), 9]
    for (ex_time in mainland_ex_time) {
      if (timeval > ex_time && totaltime < ex_time) {
        island_spec[which(island_spec[, 1] == immig_spec), 4] <- "A"
        island_spec[which(island_spec[, 1] == immig_spec), 7] <-
          "mainland_extinction"
      }
    }
    stt_table[nrow(stt_table), 2:4] <- c(
      length(which(island_spec[, 4] == "I")),
      length(which(island_spec[, 4] == "A")),
      length(which(island_spec[, 4] == "C")))
  }
  return(list(island_spec = island_spec,
              stt_table = stt_table))
}
