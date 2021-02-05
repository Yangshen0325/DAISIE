#' Formats clade-specific simulation output into standard
#' DAISIE list output
#'
#' @inheritParams default_params_doc
#'
#' @return List with CS DAISIE simulation output
#' @keywords internal
DAISIE_format_mainland_ex_core <- function(island_replicates,
                                           time,
                                           M,
                                           verbose = TRUE) {

  totaltime <- time
  several_islands <- list()



  for (rep in seq_along(island_replicates)) {
    full_list <- island_replicates[[rep]]

### separate taxon_list lists from empty island lists

    print(length(full_list))

    new_full_list <- list()
    for (i in seq_along(full_list)) {
      if (is.null(full_list[[i]]$taxon_list)) {
        new_full_list[[i]] <- full_list[[i]]
      } else {
        for (j in seq_along(full_list[[i]]$taxon_list)) {
          new_full_list <- append(new_full_list, full_list[[i]]$taxon_list[[j]])
        }
      }
    }

    print(length(new_full_list))
    browser()

    stac_zero_vec <- unlist(full_list)[which(names(unlist(full_list)) == "stac")]
    number_not_present <- length(stac_zero_vec)
    stac_nonzero_vec <- unlist(full_list)[which(names(unlist(full_list)) == "taxon_list.stac")]
    present <- which(stac_vec != 0)
    number_present <- length(present)
    island_list <- list()
    for (i in 1:(number_present + 1)) {
      island_list[[i]] <- list()
    }

    island_list[[1]] <- list(island_age = totaltime,
                             not_present = number_not_present)

    if (number_present > 0) {
      for (i in 1:number_present) {
        island_list[[1 + i]] <- full_list[[present[i]]]
        island_list[[1 + i]]$stt_table <- NULL
      }
    }
    if (number_present == 0) {
      island_list <- list()
      island_list[[1]] <- list(island_age = totaltime,
                               not_present = M,
                               stt_all = stt_all)
    }
    several_islands[[rep]] <- island_list
    if (verbose == TRUE) {
      print(paste("Island being formatted: ",
                  rep,
                  "/",
                  length(island_replicates),
                  sep = ""))
    }
  }
  return(several_islands)
}

