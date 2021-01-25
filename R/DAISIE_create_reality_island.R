#' Converts simulation output into island output
#'
#' @inheritParams default_params_doc
#'
#' @return list with the island information, composed stt table,
#' branching times of extant species, status of species on
#' the island and number of missing species.
#' @keywords internal
DAISIE_create_reality_island <- function(stt_table,
                                         totaltime,
                                         island_spec,
                                         mainland_n,
                                         mainland) {

  ### if there are no species on the island branching_times = island_age,
  ### stac = 0, missing_species = 0
  if (length(island_spec[, 1]) == 0) {
    island <- list(stt_table = stt_table,
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

    ### number of independent colonisations
    uniquecolonisation <- as.numeric(unique(
      island_spec[, "Colonisation time (BP)"]))
    number_colonisations <- length(uniquecolonisation)

    ### if there is only one independent colonisation - anagenetic and
    ### cladogenetic species are classed as stac=2; immigrant classed as stac=4:
    if (number_colonisations == 1) {
          if (island_spec[1, "Species type"] == "I") {
            descendants <- list(stt_table = stt_table,
                                branching_times = c(
                                  time,
                                  as.numeric(island_spec[1, "Colonisation time (BP)"])
                                ),
                                stac = 4,
                                missing_species = 0)
          }
          if (island_spec[1, "Species type"] == "A") {
            descendants <- list(stt_table = stt_table,
                                branching_times = c(
                                  time,
                                  as.numeric(island_spec[1, "Colonisation time (BP)"])
                                ),
                                stac = 2,
                                missing_species = 0)
          }
          if (island_spec[1, "Species type"] == "C") {
            descendants <- list(stt_table = stt_table,
                                branching_times = c(
                                  time,
                                  sort(
                                    as.numeric(island_spec[, "branching time (BP)"]),
                                    decreasing = TRUE
                                  )
                                ),
                                stac = 2,
                                missing_species = 0)
          }
    }






    ### set ages as counting backwards from present
    island_spec[, "branching time (BP)"] <- totaltime -
      as.numeric(island_spec[, "branching time (BP)"])
    island_spec[, "Colonisation time (BP)"] <- totaltime -
      as.numeric(island_spec[, "Colonisation time (BP)"])
    if (mainland_n == 1) {
      ### number of independent colonisations
      uniquecolonisation <- as.numeric(unique(
        island_spec[, "Colonisation time (BP)"]))
      number_colonisations <- length(uniquecolonisation)
      ### if there is only one independent colonisation - anagenetic and
      ### cladogenetic species are classed as stac=2; immigrant classed as stac=4:
      if (number_colonisations == 1) {
        if (island_spec[1, "Species type"] == "I") {
          descendants <- list(stt_table = stt_table,
                              branching_times = c(
                                totaltime,
                                as.numeric(island_spec[1, "Colonisation time (BP)"])
                              ),
                              stac = 4,
                              missing_species = 0)
        }
        if (island_spec[1, "Species type"] == "A") {
          descendants <- list(stt_table = stt_table,
                              branching_times = c(
                                totaltime,
                                as.numeric(island_spec[1, "Colonisation time (BP)"])
                              ),
                              stac = 2,
                              missing_species = 0)
        }
        if (island_spec[1, "Species type"] == "C") {
          descendants <- list(stt_table = stt_table,
                              branching_times = c(
                                totaltime,
                                sort(
                                  as.numeric(island_spec[, "branching time (BP)"]),
                                  decreasing = TRUE
                                )
                              ),
                              stac = 2,
                              missing_species = 0)
        }
      }

      ### if there are two or more independent colonisations, all species are
      ### classed as stac=3 and put within same list item:
      if (number_colonisations > 1) {
        descendants <- list(stt_table = stt_table,
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

          descendants$branching_times <- c(totaltime, btimes_all_clado_desc)
          testit::assert(!(youngest_col_time %in% btimes_all_clado_desc))

          # If no cladogenetic species is present, remove the youngest col time
        } else if (length(btimes_all_clado_desc) == 0) {
          youngest_col_time <- min(col_times)
          i_youngest_col_time <- which(col_times == youngest_col_time)
          col_times <- col_times[-i_youngest_col_time]

          descendants$branching_times <- c(totaltime, col_times)
        }


        # all_colonisations section
        uniquecol <- sort(as.numeric(
          unique(island_spec[, "Colonisation time (BP)"])), decreasing = TRUE
        )
        for (i in seq_along(uniquecol)) {
          descendants$all_colonisations[[i]] <- list(
            event_times = NA,
            species_type = NA
          )

          samecolonisation <- which(as.numeric(
            island_spec[, "Colonisation time (BP)"]) == uniquecol[i]
          )

          if (island_spec[samecolonisation[1], "Species type"] == "I") {
            descendants$all_colonisations[[i]]$event_times <- as.numeric(
              c(totaltime,island_spec[samecolonisation, "Colonisation time (BP)"])
            )
            descendants$all_colonisations[[i]]$species_type <- "I"
          }

          if (island_spec[samecolonisation[1], "Species type"] == "A") {
            descendants$all_colonisations[[i]]$event_times <- as.numeric(
              c(totaltime, island_spec[samecolonisation, "Colonisation time (BP)"])
            )
            descendants$all_colonisations[[i]]$species_type <- "A"
          }

          if (island_spec[samecolonisation[1], "Species type"] == "C") {
            descendants$all_colonisations[[i]]$event_times <-
              sort(c(totaltime, as.numeric(
                island_spec[samecolonisation, "branching time (BP)"]
              )), decreasing = TRUE)
            descendants$all_colonisations[[i]]$species_type <- "C"
          }
        }
      }
    } else if (mainland_n > 1) {
      ### number of colonists present
      colonists_present <- sort(as.numeric(unique(
        island_spec[, "Mainland Ancestor"])))
      number_colonists_present <- length(colonists_present)
      island_clades_info <- list()
      for (i in 1:number_colonists_present) {
        subset_island <- island_spec[which(island_spec[, "Mainland Ancestor"] ==
                                             colonists_present[i]), ]
        if (!is.matrix(subset_island)) {
          subset_island <- rbind(subset_island[1:7])
          colnames(subset_island) <- cnames
        }
        island_clades_info[[i]] <- DAISIE_ONEcolonist(
          totaltime,
          island_spec = subset_island,
          stt_table = NULL)
        island_clades_info[[i]]$stt_table <- NULL
      }
      island <- list(stt_table = stt_table,
                     taxon_list = island_clades_info)
    }
  }
  return(island)
}
