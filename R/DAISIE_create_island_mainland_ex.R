#' Converts simulation output into island output
#'
#' @inheritParams default_params_doc
#'
#' @return list with the island information, composed stt table,
#' branching times of extant species, status of species on
#' the island and number of missing species.
#' @keywords internal
DAISIE_create_island_mainland_ex <- function(stt_table,
                                             totaltime,
                                             island_spec,
                                             mainland) {

  mainland_n <- length(mainland[, 1])

  ideal_island <- DAISIE_create_island(stt_table = stt_table,
                                       totaltime = totaltime,
                                       island_spec = island_spec,
                                       mainland_n = mainland_n)

  reality_island <- DAISIE_create_reality_island(stt_table = stt_table,
                                                 totaltime = totaltime,
                                                 island_spec = island_spec,
                                                 mainland_n = mainland_n,
                                                 mainland = mainland)

  return(list(ideal_island = ideal_island,
              reality_island = reality_island))
}
