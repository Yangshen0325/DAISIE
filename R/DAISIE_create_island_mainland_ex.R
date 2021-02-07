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
  ### if there are no species on the island branching_times = island_age,
  ### stac = 0, missing_species = 0
  if (length(island_spec[, 1]) == 0) {
    ideal_island <- reality_island <-
      list(stt_table = stt_table,
           branching_times = totaltime,
           stac = 0,
           missing_species = 0)
  } else {
    cnames <- c("Species",
                "Mainland Ancestor",
                "Colonisation time (BP)",
                "Species type",
                "branch_code",
                "branching time (BP)",
                "Anagenetic_origin")
    colnames(island_spec) <- cnames
    ### set ages as counting backwards from present
    island_spec[, "branching time (BP)"] <- totaltime -
      as.numeric(island_spec[, "branching time (BP)"])
    island_spec[, "Colonisation time (BP)"] <- totaltime -
      as.numeric(island_spec[, "Colonisation time (BP)"])

    ### number of independent colonisations from different mainland species
    colonists_present <- sort(as.numeric(unique(
      island_spec[, "Mainland Ancestor"])))
    number_colonists_present <- length(colonists_present)

    ideal_island_clades_info <- list()
    reality_island_clades_info <- list()

    for (i in 1:number_colonists_present) {
      subset_island <- island_spec[which(island_spec[, "Mainland Ancestor"] ==
                                           colonists_present[i]), ]
      if (!is.matrix(subset_island)) {
        subset_island <- rbind(subset_island[1:7])
        colnames(subset_island) <- cnames
      }

      ideal_island_clades_info[[i]] <- DAISIE_ONEcolonist(
        time = totaltime,
        island_spec = subset_island,
        stt_table = NULL)
      ideal_island_clades_info[[i]]$stt_table <- NULL

      mainland_spec <- which(mainland[, 1] == colonists_present[i])
      ### is there any extant descendants of the immigrant on the mainland
      branching_code <- paste("^", mainland[mainland_spec, 5], sep = "")
      descending_branches <- grep(branching_code, mainland[, 5])
      extant_mainland <- any(mainland[descending_branches, 4] != "E")

      if (extant_mainland) {
        reality_island_clades_info[[i]] <- ideal_island_clades_info[[i]]
      } else {
        ### number of independent colonisations from the same mainland species
        number_colonisations <-
          length(unique(subset_island[, "Colonisation time (BP)"]))
        ### are there any branching events between the immig time and island
        ### age with extant descendants
        numberofsplits <- nchar(mainland[mainland_spec, 5]) - 1
        other_extant_mainland <- any(mainland[, 4] != "E")
        if (number_colonisations == 1) {
          if (other_extant_mainland) {
            branching_time <- common_ancestor_time(
              totaltime = totaltime,
              mainland_spec = mainland_spec,
              mainland = mainland)
            reality_island_clades_info[[i]] <- list(
              branching_times = c(
                totaltime,
                branching_time,
                sort(
                  as.numeric(subset_island[, "branching time (BP)"]),
                  decreasing = TRUE)
                ),
              stac = 2,
              missing_species = 0)
          } else {
            if (nrow(subset_island) == 1) {
              reality_island_clades_info[[i]] <- list(
                branching_times = c(
                  totaltime,
                  totaltime - 1e-5),
                stac = 5,
                missing_species = 0)
            } else {
              reality_island_clades_info[[i]] <- list(
                branching_times = c(
                  totaltime,
                  totaltime - 1e-5,
                  sort(
                    as.numeric(subset_island[, "branching time (BP)"]),
                    decreasing = TRUE)
                ),
                stac = 6,
                missing_species = 0)
            }
          }
        } else {
          if (other_extant_mainland) {
            branching_time <- common_ancestor_time(
              totaltime = totaltime,
              mainland_spec = mainland_spec,
              mainland = mainland)
            reality_island_clades_info[[i]] <- list(
              branching_times = c(
                totaltime,
                branching_time),
              stac = 3,
              missing_species = 0)
          } else {
            reality_island_clades_info[[i]] <- list(
              branching_times = c(
                totaltime,
                totaltime - 1e-5,
                sort(
                  as.numeric(subset_island[, "branching time (BP)"]),
                  decreasing = TRUE)
              ),
              stac = 6,
              missing_species = 0)
          }
        }
      }
    }
    ideal_island <- list(stt_table = stt_table,
                         taxon_list = ideal_island_clades_info)
    reality_island <- list(stt_table = stt_table,
                           taxon_list = reality_island_clades_info)
  }
  return(list(ideal_island = ideal_island,
              reality_island = reality_island))
}
