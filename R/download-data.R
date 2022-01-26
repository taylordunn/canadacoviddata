#' Register the pins board
#'
#' The `pins::board_register_github()` function requires a GitHub personal
#' access token be available through the environment variable `GITHUB_PAT`.
#'
#' @export
#' @importFrom pins board_register_github
register_github_board <- function() {
  pins::board_register_github(
    name = "github", repo = "taylordunn/canadacoviddata", path = "data-raw",
    token = Sys.getenv("GITHUB_PAT")
  )
}

#' Retrieve and save the provinces data
#'
#' Retrieves the `provinces` data from the Canadian COVID-19 tracker API
#' and pins it to the given board.
#' Returns the list of provinces which have been updated, by comparing the
#' `updated_at` timestamp to the previously pinned data.
#'
#' @param board The name of the `pins` board to write the data.
#'
#' @return A list of two-letter province/territory codes that have been updated.
#'
#' @export
#' @importFrom canadacovid get_provinces
#' @importFrom pins pin
#' @importFrom dplyr anti_join
download_provinces <- function(board = "github") {
  old_provinces <- pins::pin_get("provinces", board = board)
  new_provinces <- canadacovid::get_provinces()

  updated_provinces <- new_provinces %>%
    dplyr::anti_join(old_provinces, by = c("name", "updated_at"))

  if (nrow(updated_provinces) > 0) {
    print("Updating provinces.")
    pins::pin(new_provinces, name = "provinces", board = board)
  }
  return(updated_provinces$code)
}

#' Retrieve and save the reports data
#'
#' Retrieves the `reports` data from the Canadian COVID-19 tracker API
#' and pins it to the given board.
#' Also computes some extra variables, `change_active`, `total_active`,
#' and `positivity_rate`.
#'
#' @param provinces_codes A list of two-letter territory/provinces codes
#'   (e.g. "AB") to be updated.
#' @param board The name of the `pins` board to write the data.
#'
#' @export
#' @importFrom pins pin
#' @importFrom canadacovid get_reports
#' @importFrom rlang .data
#' @importFrom dplyr mutate
download_reports <- function(provinces_codes, board = "github") {
  for (prov in provinces_codes) {
    if (prov == "overall") {
      new_report <- canadacovid::get_reports("overall")
    } else {
      new_report <- canadacovid::get_reports(province = prov)
    }
    new_report <- new_report %>%
      dplyr::mutate(
        change_active = .data$change_cases - .data$change_recoveries -
          .data$change_fatalities,
        total_active = .data$total_cases - .data$total_recoveries -
          .data$total_fatalities,
        positivity_rate = .data$change_cases / .data$change_tests
      )
    print(paste0("Updating ", prov, " report (timestamp: ",
                 unique(new_report$last_updated), "."))
    pins::pin(new_report,
              name = paste0("reports_", tolower(prov)), board = board)
  }
}
