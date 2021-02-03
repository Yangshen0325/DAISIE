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

    ideal_island <- DAISIE_ONEcolonist(time = totaltime,
                                       island_spec = island_spec,
                                       stt_table = stt_table)

    ### number of independent colonisations from different mainland species
    diff_colonisations <- unique(island_spec[, "Mainland Ancestor"])
    ### is the mainland ancestor extinct on the mainland
    extinct_mainland_ancestor <-
      mainland[which(mainland[, 1] == diff_colonisations), 9] < totaltime
    ### number of independent colonisations from the same mainland species
    num_colonisation <- length(as.numeric(unique(
      island_spec[, "Colonisation time (BP)"])))
    ### is there any extant mainland descendants of the immigrant
    extant_mainland_descendants <-
      any(mainland[which(mainland[, 2] == diff_colonisations), 9] == totaltime)

    for (i in diff_colonisations) {
      if (extinct_mainland_ancestor == TRUE) {
        if (num_colonisation == 1) {
          if (extant_mainland_descendants == TRUE) {
            reality_island <- list(
              stt_table = stt_table,
              branching_times = c(
                totaltime,
                as.numeric(island_spec[1, "Colonisation time (BP)"])
              ),
              stac = 2,
              missing_species = 0)
          } else {
            #are there any branching times between immig and island age
            browser()
          }
        } else {
          if (extant_mainland_descendants == TRUE) {
            reality_island <- list(stt_table = stt_table,
                                   branching_times = NA,
                                   stac = 3,
                                   missing_species = 0,
                                   all_colonisations = list())

            # Get branching and colonisation times
            btimes_all_clado_desc <- rev(
              sort(as.numeric(island_spec[, "branching time (BP)"]))
            )
            col_times <- sort(
              unique(as.numeric(island_spec[, "Colonisation time (BP)"])),
              decreasing = TRUE
            )

            # If there are endemic descendants find youngest col time
            if (length(btimes_all_clado_desc) != 0) {
              # Ensure all col_times are in b_times at this point.
              # Covers cases of one recolonization followed by cladogenesis and
              # potential extinction
              if (any(!(col_times %in% btimes_all_clado_desc))) {
                miss_col_time <- which(!(col_times %in% btimes_all_clado_desc))
                btimes_all_clado_desc <- sort(
                  c(btimes_all_clado_desc, col_times[miss_col_time]),
                  decreasing = TRUE
                )
              }
              youngest_col_time <- min(col_times)
              i_youngest_col_btimes <- which(btimes_all_clado_desc == youngest_col_time)

              # Remove youngest col time in branching times
              testit::assert(youngest_col_time %in% btimes_all_clado_desc)
              btimes_all_clado_desc <- btimes_all_clado_desc[-i_youngest_col_btimes]

              island$branching_times <- c(totaltime, btimes_all_clado_desc)
              testit::assert(!(youngest_col_time %in% btimes_all_clado_desc))

              # If no cladogenetic species is present, remove the youngest col time
            } else if (length(btimes_all_clado_desc) == 0) {
              youngest_col_time <- min(col_times)
              i_youngest_col_time <- which(col_times == youngest_col_time)
              col_times <- col_times[-i_youngest_col_time]

              island$branching_times <- c(totaltime, col_times)
            }

            # all_colonisations section
            uniquecol <- sort(as.numeric(
              unique(island_spec[, "Colonisation time (BP)"])), decreasing = TRUE
            )
            for (i in seq_along(uniquecol)) {
              reality_island$all_colonisations[[i]] <- list(
                event_times = NA,
                species_type = NA
              )

              samecolonisation <- which(as.numeric(
                island_spec[, "Colonisation time (BP)"]) == uniquecol[i]
              )

              if (island_spec[samecolonisation[1], "Species type"] == "I") {
                reality_island$all_colonisations[[i]]$event_times <- as.numeric(
                  c(totaltime,island_spec[samecolonisation, "Colonisation time (BP)"])
                )
                reality_island$all_colonisations[[i]]$species_type <- "I"
              }

              if (island_spec[samecolonisation[1], "Species type"] == "A") {
                reality_island$all_colonisations[[i]]$event_times <- as.numeric(
                  c(totaltime, island_spec[samecolonisation, "Colonisation time (BP)"])
                )
                reality_island$all_colonisations[[i]]$species_type <- "A"
              }

              if (island_spec[samecolonisation[1], "Species type"] == "C") {
                reality_island$all_colonisations[[i]]$event_times <-
                  sort(c(totaltime, as.numeric(
                    island_spec[samecolonisation, "branching time (BP)"]
                  )), decreasing = TRUE)
                reality_island$all_colonisations[[i]]$species_type <- "C"
              }
            }
          } else {
            #are there any branching times between immig and island age
            browser()
          }
        }
      } else {
        if (num_colonisation == 1) {
          if (island_spec[, 4] == "I") {
            ### non-endemic species
            reality_island <- list(
              stt_table = stt_table,
              branching_times = c(
                totaltime,
                as.numeric(island_spec[1, "Colonisation time (BP)"])
              ),
              stac = 4,
              missing_species = 0)
          } else {
            ### endemic species
            reality_island <- list(
              stt_table = stt_table,
              branching_times = c(
                totaltime,
                as.numeric(island_spec[1, "Colonisation time (BP)"])
              ),
              stac = 2,
              missing_species = 0)
          }
        } else {
          reality_island <- list(stt_table = stt_table,
                                 branching_times = NA,
                                 stac = 3,
                                 missing_species = 0,
                                 all_colonisations = list())

          # Get branching and colonisation times
          btimes_all_clado_desc <- rev(
            sort(as.numeric(island_spec[, "branching time (BP)"]))
          )
          col_times <- sort(
            unique(as.numeric(island_spec[, "Colonisation time (BP)"])),
            decreasing = TRUE
          )

          # If there are endemic descendants find youngest col time
          if (length(btimes_all_clado_desc) != 0) {
            # Ensure all col_times are in b_times at this point.
            # Covers cases of one recolonization followed by cladogenesis and
            # potential extinction
            if (any(!(col_times %in% btimes_all_clado_desc))) {
              miss_col_time <- which(!(col_times %in% btimes_all_clado_desc))
              btimes_all_clado_desc <- sort(
                c(btimes_all_clado_desc, col_times[miss_col_time]),
                decreasing = TRUE
              )
            }
            youngest_col_time <- min(col_times)
            i_youngest_col_btimes <- which(btimes_all_clado_desc == youngest_col_time)

            # Remove youngest col time in branching times
            testit::assert(youngest_col_time %in% btimes_all_clado_desc)
            btimes_all_clado_desc <- btimes_all_clado_desc[-i_youngest_col_btimes]

            reality_island$branching_times <- c(totaltime, btimes_all_clado_desc)
            testit::assert(!(youngest_col_time %in% btimes_all_clado_desc))

            # If no cladogenetic species is present, remove the youngest col time
          } else if (length(btimes_all_clado_desc) == 0) {
            youngest_col_time <- min(col_times)
            i_youngest_col_time <- which(col_times == youngest_col_time)
            col_times <- col_times[-i_youngest_col_time]

            reality_island$branching_times <- c(totaltime, col_times)
          }

          # all_colonisations section
          uniquecol <- sort(as.numeric(
            unique(island_spec[, "Colonisation time (BP)"])), decreasing = TRUE
          )
          for (i in seq_along(uniquecol)) {
            reality_island$all_colonisations[[i]] <- list(
              event_times = NA,
              species_type = NA
            )

            samecolonisation <- which(as.numeric(
              island_spec[, "Colonisation time (BP)"]) == uniquecol[i]
            )

            if (island_spec[samecolonisation[1], "Species type"] == "I") {
              reality_island$all_colonisations[[i]]$event_times <- as.numeric(
                c(totaltime,island_spec[samecolonisation, "Colonisation time (BP)"])
              )
              reality_island$all_colonisations[[i]]$species_type <- "I"
            }

            if (island_spec[samecolonisation[1], "Species type"] == "A") {
              reality_island$all_colonisations[[i]]$event_times <- as.numeric(
                c(totaltime, island_spec[samecolonisation, "Colonisation time (BP)"])
              )
              reality_island$all_colonisations[[i]]$species_type <- "A"
            }

            if (island_spec[samecolonisation[1], "Species type"] == "C") {
              reality_island$all_colonisations[[i]]$event_times <-
                sort(c(totaltime, as.numeric(
                  island_spec[samecolonisation, "branching time (BP)"]
                )), decreasing = TRUE)
              reality_island$all_colonisations[[i]]$species_type <- "C"
            }
          }
        }
      }
    }
  }
  return(list(ideal_island = ideal_island,
              reality_island = reality_island))
}
