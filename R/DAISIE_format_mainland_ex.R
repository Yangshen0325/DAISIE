#' Formats clade-specific simulation output into standard
#' DAISIE list output
#'
#' @inheritParams default_params_doc
#'
#' @return List with CS DAISIE simulation output
#' @keywords internal
DAISIE_format_mainland_ex <- function(island_replicates,
                                      time,
                                      M,
                                      verbose = TRUE) {

  ideal_island_replicates <- list()
  reality_island_replicates <- list()
  for (i in seq_along(island_replicates)) {
    ideal_island_replicates[[i]] <- list()
    reality_island_replicates[[i]] <- list()
    for (j in seq_along(island_replicates[[i]])) {
      ideal_island_replicates[[i]][[j]] <- island_replicates[[i]][[j]]$ideal_island
      reality_island_replicates[[i]][[j]] <- island_replicates[[i]][[j]]$reality_island
    }
  }

  ideal_islands <- DAISIE_format_mainland_ex_core(
    island_replicates = ideal_island_replicates,
    time = time,
    M = M,
    verbose = verbose)

  reality_islands <- DAISIE_format_mainland_ex_core(
    island_replicates = reality_island_replicates,
    time = time,
    M = M,
    verbose = verbose)

  return(list(ideal_islands = ideal_islands,
              reality_islands = reality_islands))
}

