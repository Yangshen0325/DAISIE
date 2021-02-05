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
    ideal_island <- list(stt_table = stt_table,
                         branching_times = totaltime,
                         stac = 0,
                         missing_species = 0)
    reality_island <- list(stt_table = stt_table,
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
      ### is the mainland ancestor extant on the mainland
      extant_mainland_ancestor <-
        mainland[mainland_spec, 4] != "E"
      ### number of independent colonisations from the same mainland species
      uniquecolonisation <- as.numeric(unique(
        subset_island[, "Colonisation time (BP)"]))
      number_colonisations <- length(uniquecolonisation)
      ### is there any extant mainland descendants of the immigrant
      branching_code <- paste("^", mainland[mainland_spec, 5], sep = "")
      descending_branches <- grep(branching_code, mainland[, 5])
      extant_descending_branches <-
        any(mainland[descending_branches, 9] == totaltime)
      number_mainland_branches <- nchar(mainland[mainland_spec, 5]) - 1

      if (extant_mainland_ancestor) {
        reality_island_clades_info[[i]] <- ideal_island_clades_info[[i]]
      } else {
        if (number_colonisations == 1) {
          if (extant_descending_branches) {
            reality_island_clades_info[[i]] <- ideal_island_clades_info[[i]]
          } else {
            #are there any branching times between immig and island age
            if (number_mainland_branches == 0) {
              if (sum(island_spec[, 2] == i) == 1) {
                #singleton
                reality_island_clades_info[[i]] <- list(
                  branching_times = c(
                    totaltime,
                    totaltime - 1e-5),
                  stac = 5,
                  missing_species = 0)
              } else {
                #clade
                reality_island_clades_info[[i]] <- list(
                  branching_times = c(
                    totaltime,
                    totaltime - 1e-5,
                    sort(
                      as.numeric(island_spec[, "branching time (BP)"]),
                      decreasing = TRUE)
                    ),
                  stac = 6,
                  missing_species = 0)
              }
            } else {
              #are any of the descendants extant
              if (any(mainland[, 4] != "E")) {
                #which branching time
                #get all the extant descendants
                extant_sisters <- which(mainland[, 4] != "E")
                branching_times <- c()
                for (j in seq_along(extant_sisters)) {
                  #get the branching time of the focal species and them
                  numberofsplits <- nchar(mainland[mainland[, 1] == colonists_present[i], 5]) - 1
                  mostrecentspl <- substring(mainland[mainland[, 1] == colonists_present[i], 5], numberofsplits)
                  branching_times[j] <- mainland[mainland[, 5] == mostrecentspl, 9]
                }
                branching_time <- min(branching_times)
                reality_island_clades_info[[i]] <- list(
                  branching_times = c(
                    totaltime,
                    branching_time),
                  stac = 2,
                  missing_species = 0)
              } else {
                if (sum(island_spec[, 2] == i) == 1) {
                  #singleton
                  reality_island_clades_info[[i]] <- list(
                    branching_times = c(
                      totaltime,
                      totaltime - 1e-5),
                    stac = 5,
                    missing_species = 0)
                } else {
                  #clade
                  reality_island_clades_info[[i]] <- list(
                    branching_times = c(
                      totaltime,
                      totaltime - 1e-5,
                      sort(
                        as.numeric(island_spec[, "branching time (BP)"]),
                        decreasing = TRUE)
                      ),
                    stac = 6,
                    missing_species = 0)
                }
              }
            }
          }
        } else {
          if (extant_descending_branches == TRUE) {
            reality_island_clades_info[[i]] <- ideal_island_clades_info[[i]]
          } else {
            #are there any branching times between immig and island age
            if (number_mainland_branches == 0) {
              reality_island_clades_info[[i]] <- list(
                branching_times = c(
                  totaltime,
                  totaltime - 1e-5,
                  sort(
                    as.numeric(island_spec[, "branching time (BP)"]),
                    decreasing = TRUE)
                  ),
                stac = 6,
                missing_species = 0)
            } else {
              #are any of the descendants extant
              if (any(mainland[, 4] != "E")) {
                reality_island_clades_info[[i]] <- ideal_island_clades_info[[i]]
              } else {
                reality_island_clades_info[[i]] <- list(
                  branching_times = c(
                    totaltime,
                    totaltime - 1e-5,
                    sort(
                      as.numeric(island_spec[, "branching time (BP)"]),
                      decreasing = TRUE)
                    ),
                  stac = 6,
                  missing_species = 0)
              }
            }
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
