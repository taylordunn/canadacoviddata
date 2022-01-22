#' Retrieve and save the provinces data
#'
#' @importFrom canadacovid get_provinces
download_provinces <- function() {
  canadacovid::get_provinces() %>%
    saveRDS(file = paste0("data-raw/provinces.rds"))
}
