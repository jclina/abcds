#' @title expand_lookup
#' @description Expands the look-up table in cases where there are ranges in the scores
#' @param data The data set containing the variable with the ranges
#' @param variable The variable containing the range as indicated by a dash between two numeric values
#' @return The expanded look-up table with the variable in descending order
#' @details Expands the look-up table in cases where there are ranges in the scores
#' @seealso
#'  \code{\link[rlang]{:=}}
#'  \code{\link[tibble]{tibble}}
#' @rdname expand_lookup
#' @importFrom rlang `:=`
#' @importFrom tibble tibble
#' @keywords internal

expand_lookup <- function(data, variable) {
  `:=` <- rlang::`:=`

  lookupTable <-
    do.call(
      rbind,
      lapply(seq_along(data[[variable]]), FUN = function(i) {
        if (grepl("-", data[[variable]][i])) {
          bounds <- as.numeric(strsplit(data[[variable]][i], "-")[[1]])
          tibble::tibble(
            !!variable := seq(bounds[1], bounds[2]),
            data[i, -which(colnames(data) == variable)]
          )
        } else {
          tibble::tibble(
            !!variable := as.numeric(data[[variable]][i]),
            data[i, -which(colnames(data) == variable)]
          )
        }
      })
    )

  lookupTable[order(lookupTable[[variable]], decreasing = TRUE), ]
}


#' @title deploy_kbit2_app
#' @description Deploys the KBIT-2 Calculator Shiny app on a local host server
#' @return Launches the KBIT-2 Calculator Shiny app
#' @details Deploys the KBIT-2 Calculator Shiny app on a local host server
#' @importFrom shiny runApp
#' @export

deploy_kbit2_app <- function() {
  shiny::runApp(system.file("app/kbit2/app.R", package = "abcds"))
}

#' @title abcds_join
#' @description A flexible join option takes a dplyr join function as an argument
#' @inheritParams dplyr::full_join
#' @param join_type The type of dplyr join to perform. Must be a string, symbol, or function (e.g.,
#'   "inner_join", `inner_join`, `dplyr::inner_join`)
#' @return A tibble containing the joined data
#' @details A flexible join option takes a dplyr join function as an argument
#' @seealso
#'  \code{\link[rlang]{sym}}, \code{\link[rlang]{eval_tidy}}, \code{\link[rlang]{is_symbol}}, \code{\link[rlang]{is_call}}, \code{\link[rlang]{abort}}
#' @rdname abcds_join
#' @export
#' @importFrom rlang sym eval_tidy is_symbol is_call abort

abcds_join <- function(x, y, by, join_type) {
  join_fn <- tryCatch(
    {
      rlang::eval_tidy(rlang::ensym(join_type), env = asNamespace("dplyr"))
    },
    error = function(e) {
      rlang::eval_tidy(join_type)
    }
  )

  join_fn(x, y, by = by)
}
