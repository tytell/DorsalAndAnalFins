#' Get a project path from config.yml
#'
#' Looks up `key` in `config.yml` (using the active config profile) and
#' returns an absolute path. Relative paths are resolved from the project
#' root with [here::here()]; `~` is expanded.
#'
#' @param key Name of the entry in `config.yml`, e.g. `"raw_dir"`.
#' @param ... Further path components appended to the result.
#' @returns An `fs_path`.
#' @examples
#' \dontrun{
#' proj_path("raw_dir", "Pretreat")
#' }
proj_path <- function(key, ...) {
  p <- config::get(key, file = here::here("config.yml"))
  if (is.null(p)) {
    cli_abort("No entry {.val {key}} in {.file config.yml}.")
  }
  p <- fs::path_expand(p)
  if (!fs::is_absolute_path(p)) {
    p <- here::here(p)
  }
  fs::path(p, ...)
}
