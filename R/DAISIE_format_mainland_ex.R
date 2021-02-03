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
                                      sample_freq,
                                      verbose = TRUE) {

  ideal_island_replicates <- list()
  reality_island_replicates <- list()
  ideal_island_replicates[[1]] <- list()
  reality_island_replicates[[1]] <- list()
  for (i in seq_along(island_replicates[[1]])) {
    ideal_island_replicates[[1]][[i]] <- island_replicates[[1]][[i]]$ideal_island
    reality_island_replicates[[1]][[i]] <- island_replicates[[1]][[i]]$reality_island
  }

  ideal_islands <- DAISIE_format_CS(
    island_replicates = ideal_island_replicates,
    time = time,
    M = M,
    sample_freq = sample_freq,
    verbose = verbose)

  reality_islands <- DAISIE_format_CS(
    island_replicates = reality_island_replicates,
    time = time,
    M = M,
    sample_freq = sample_freq,
    verbose = verbose)

  return(list(ideal_islands = ideal_islands,
              reality_islands = reality_islands))
}

