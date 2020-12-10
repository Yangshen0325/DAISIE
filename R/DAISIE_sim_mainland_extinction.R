#' @title Simulate oceanic islands with given parameters under
#'   time-constant rates with mainland dynamics
#'
#' @description
#' This function simulates islands with given cladogenesis,
#' extinction, Kprime, immigration and anagenesis parameters, and simulates
#' the mainland given a mainland extinction parameter, all of which
#' modelled as time-constant parameters.
#'
#' @inheritParams default_params_doc
#'
#' @return
#' A list. The highest level of the least corresponds to each individual
#' replciate. The first element of each replicate is composed of island
#' information containing:
#' \itemize{
#'   \item{\code{$island_age}: A numeric with the island age.}
#'   \item{\code{$not_present}: the number of mainland lineages that are not
#'     present on the island. It is only present if only 1 type of species is
#'     simulated. Becomes \code{$not_present_type1}: the number of mainland
#'     lineages of type 1 that are not present on the island and
#'     \code{$not_present_type2}: the number of mainland lineages of type 2
#'     that are not present on the island, if two types are simulated.}
#'   \item{\code{$stt_all}: STT table for all species on the island
#'     (nI - number of non-endemic species; nA - number of anagenetic species,
#'     nC - number of cladogenetic species, present - number of independent
#'     colonisations present)}
#'   \item{\code{$stt_stt_type1}: STT table for type 1 species on the island -
#'     only if 2 types of species were simulated (nI - number of non-endemic
#'     species; nA - number of anagenetic species, nC - number of cladogenetic
#'     species, present - number of independent colonisations present).}
#'   \item{\code{$stt_stt_type2}: STT table for type 2 species on the island
#'      - only if 2 types of species were simulated (nI - number of non-endemic
#'      species; nA - number of anagenetic species, nC - number of cladogenetic
#'      species, present - number of independent colonisations present ).}
#'   \item{\code{$brts_table}: Only for simulations under \code{"IW"}. Table
#' containing information on order of events in the data, for use in maximum
#' likelihood optimization.).}
#' }
#' The subsequent elements of the list pertaining to each replcate contain
#' information on a single colonist lineage on the island and have 4 components:
#' \itemize{
#'   \item{\code{$branching_times}: island age and stem age of the
#'     population/species in the case of Non-endemic, Non-endemic_MaxAge and
#'     Endemic anagenetic species.
#'
#'     For cladogenetic species these should
#'     be island age and branching times of the radiation including the
#'     stem age of the radiation.}
#'   \item{\code{$stac}: An integer ranging from 1 to 4
#'   indicating the status of the colonist:}
#'   \enumerate{
#'     \item Non_endemic_MaxAge
#'     \item Endemic
#'     \item Endemic&Non_Endemic
#'     \item Non_endemic_MaxAge
#' }
#' \item{\code{$missing_species}: number of island species that were
#' not sampled for particular clade (only applicable for endemic clades)}
#' \item{\code{$type_1or2}: whether the colonist belongs to type 1 or type 2}
#' }
#' @author Luis Valente, Albert Phillimore, Joshua Lambert, Shu Xie, Pedro
#' Neves, Richèl J. C. Bilderbeek, Rampal Etienne
#' @seealso \code{\link{DAISIE_plot_sims}()} for plotting STT of simulation
#' outputs.
#' @family simulation models
#' @references Valente, L.M., A.B. Phillimore and R.S. Etienne (2015).
#' Equilibrium and non-equilibrium dynamics simultaneously operate in the
#' Galapagos islands. Ecology Letters 18: 844-852.
#' @keywords models
#' @examples
#' ## Simulate 2 islands for 1 million years, where all species have equal
#' ## rates. Pool size 100.
#'
#' clado_rate <- 0.5
#' ext_rate <- 0.2
#' carr_cap <- Inf
#' immig_rate <- 0.005
#' ana_rate <- 1
#' sim_pars <- c(clado_rate, ext_rate, carr_cap, immig_rate, ana_rate)
#' set.seed(1)
#' island_replicates <- DAISIE_sim_constant_rate(
#'   time = 1,
#'   M = 100,
#'   pars = sim_pars,
#'   replicates = 2,
#'   plot_sims = FALSE,
#'   verbose = FALSE
#' )
#'
#' @export DAISIE_sim_mainland_extinction
DAISIE_sim_mainland_extinction <- function(
  time,
  M,
  island_pars,
  mainland_par,
  replicates,
  replacement,
  sample_freq = 25,
  plot_sims = TRUE,
  hyper_pars = create_hyper_pars(d = 0, x = 0),
  area_pars = create_area_pars(
    max_area = 1,
    current_area = 1,
    proportional_peak_t = 0,
    total_island_age = 0,
    sea_level_amplitude = 0,
    sea_level_frequency = 0,
    island_gradient_angle = 0),
  verbose = TRUE,
  ...) {
  testit::assert(length(island_pars) == 5)
  testit::assert(length(mainland_par) == 1)
  testit::assert(are_hyper_pars(hyper_pars = hyper_pars))
  testit::assert(are_area_pars(area_pars = area_pars))

  total_time <- time
  island_replicates <- list()
  mainland_replicates <- list()
  for (rep in 1:replicates) {
    island_replicates[[rep]] <- list()
    mainland_replicates[[rep]] <- mainland_extinction(time = total_time,
                                                      M = M,
                                                      mu_m = mainland_par,
                                                      replacement = replacement)

    full_list <- list()
    for (m_spec in 1:M) {
      cond_mainland <- FALSE
      while (cond_mainland == FALSE) {
        mainland_spec <- sample(1:length(mainland_replicates[[rep]]), 1)
        if (replacement == "dispersal") {
          mainland_n <- 1
        }
        if (replacement == "speciation") {
          mainland_n <- length(mainland_replicates[[1]][[rep]]$colonisation_time) +
            length(mainland_replicates[[1]][[rep]]$speciation_time) -
            length(mainland_replicates[[1]][[rep]]$extinction_time)
        }
        island_lineage <- DAISIE_sim_core_constant_rate(
          time = total_time,
          mainland_n = mainland_n,
          pars = island_pars,
          nonoceanic_pars = c(0, 0),
          hyper_pars = hyper_pars,
          area_pars = area_pars
        )
        cond_mainland <- check_island_mainland(
          total_time = total_time,
          mainland_lineage = mainland_replicates[[1]][[mainland_spec]],
          island_lineage = island_lineage)
        if (cond_mainland == TRUE) {
          full_list[[m_spec]] <- island_lineage
        }
      }
    }
    island_replicates[[rep]] <- full_list
    if (verbose == TRUE) {
      print(paste("Island replicate ", rep, sep = ""))
    }
  }
  island_replicates <- DAISIE_format_CS(
    island_replicates = island_replicates,
    time = total_time,
    M = M,
    sample_freq = sample_freq,
    verbose = verbose
  )
  if (plot_sims == TRUE) {
    DAISIE_plot_sims(
      island_replicates = island_replicates,
      sample_freq = sample_freq
    )
  }
  return(list(island_replicates, mainland_replicates))
}
