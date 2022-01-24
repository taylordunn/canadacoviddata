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
#' and downloads it to the given board.
#'
#' @param board The name of the `pins` board to write the data.
#'
#' @export
#' @importFrom canadacovid get_provinces
#' @importFrom pins pin
download_provinces <- function(board = "github") {
  canadacovid::get_provinces() %>%
    pins::pin(name = "provinces", board = board)
}
