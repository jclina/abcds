#' Add Study IDs from Past Studies
#'
#' @description
#' Joins harmonized participant data with study-specific participant IDs from
#' the U01 or U19 study. This function uses a crosswalk table to map between
#' common/harmonized participant identifiers and their corresponding IDs in
#' different historical studies.
#'
#' @param data A data frame containing harmonized participant data to be
#'   augmented with study-specific participant identifiers.
#' @inheritParams abcds_join
#' @param ... Variables to join by, passed as unquoted column names. These
#'   specify the common columns between `data` and the ID crosswalk table.
#'
#' @return A data frame with the original data augmented with study-specific
#'   participant IDs from the crosswalk table.
#'
#' @details
#' The function loads an internal crosswalk table (`id_crosswalk.RData`) from
#' the package's `extdata` directory and performs a join operation using the
#' `abcds_join()` function. The crosswalk table maps harmonized participant
#' identifiers to their equivalents across different studies.
#'
#' @examples
#' \dontrun{
#' # Add study IDs using a left join on 'participant_id'
#' data_with_ids <- add_study_ids(my_data, inner_join, subject_label)
#'
#' }
#'
#' @seealso \code{\link{add_event_ids}} for adding study-specific event identifiers
#' @export

add_study_ids <- function(data, join_type = NULL, ...) {
  id_crosswalk <- NULL
  join_type <- rlang::enexpr(join_type)
  by <- as.character(rlang::ensyms(...))
  load(system.file("extdata/id_crosswalk.RData", package = "abcds"))
  data <- abcds_join(id_crosswalk, data, by = by, !!join_type)
  return(data)
}


#' @title Add Event IDs from Past Studies
#'
#' @description
#' Joins harmonized event data with study-specific event names from
#' the U01 or U19 study. This function uses a crosswalk table to map between
#' common/harmonized event identifiers and their corresponding names
#' in different historical studies.
#'
#' @param data A data frame containing harmonized event data to be augmented
#'   with study-specific event identifiers.
#' @inheritParams abcds_join
#' @param ... Variables to join by, passed as unquoted column names. These
#'   specify the common columns between `data` and the event crosswalk table.
#'
#' @return A data frame with the original data augmented with study-specific
#'   event names or IDs from the crosswalk table.
#'
#' @details
#' The function loads an internal crosswalk table (`event_crosswalk.RData`)
#' from the package's `extdata` directory and performs a join operation using
#' the `abcds_join()` function. The crosswalk table maps harmonized event
#' identifiers to their equivalents across different studies.
#'
#' @examples
#' \dontrun{
#' # Add event IDs using a left join on 'event_name'
#' data_with_events <- add_event_ids(my_data, inner_join, event_sequence)
#' }
#'
#' @seealso \code{\link{add_study_ids}} for adding study-specific participant IDs
#' @export

add_event_ids <- function(data, join_type = NULL, ...) {
  event_crosswalk <- NULL
  join_type <- rlang::enexpr(join_type)
  by <- as.character(rlang::ensyms(...))
  load(system.file("extdata/event_crosswalk.RData", package = "abcds"))
  names(event_crosswalk)
  data <- abcds_join(event_crosswalk, data, by = by, !!join_type)
  return(data)
}
