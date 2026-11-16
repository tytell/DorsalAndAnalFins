#' Read force and torque data from an HDF5 file
#'
#' Reads the calibrated forces, torques, and encoder data, plus the
#' nominal stimulus time vectors (`t` and `tnorm`), from a force
#' transducer HDF5 file. The bias and all attributes in the file (except
#' those on `NominalStimulus/Position` and `NominalStimulus/Velocity`) are
#' added as constant columns. Attributes are named by their name in the
#' file; if the same name appears on more than one object, the object path
#' is added as a prefix (e.g., `Output_SampleFrequency`).
#'
#' @param filename Path to the `.h5` file.
#' @returns A tibble with one row per sample and columns `t`, `tnorm`,
#'   `fx`, `fy`, `fz`, `tx`, `ty`, `tz`, `encoder`, `bias_fx`, ...,
#'   `bias_tz`, then one column per attribute.
#' @examples
#' \dontrun{
#' read_flapper_data(proj_path("data_dir", "RawData", "ForceData",
#'                         "2026-11-03", "FlexFin", "test003.h5"))
#' }
read_flapper_data <- function(filename) {
  assertthat::assert_that(assertthat::is.readable(filename))

  h5 <- hdf5r::H5File$new(filename, mode = "r")
  on.exit(h5$close_all(), add = TRUE)

  # helper function to read a dataset by name
  read_ds <- function(name) h5[[name]]$read()

  data <- tibble::tibble(
    t.s = read_ds("NominalStimulus/t"),
    t.norm = read_ds("NominalStimulus/tnorm"),
    angle_cmd.deg = read_ds("NominalStimulus/Position"),
    fx.N = read_ds("Calibrated/xForce"),
    fy.N = read_ds("Calibrated/yForce"),
    fz.N = read_ds("Calibrated/zForce"),
    tx.Nm = read_ds("Calibrated/xTorque"),
    ty.Nm = read_ds("Calibrated/yTorque"),
    tz.Nm = read_ds("Calibrated/zTorque"),
    encoder.deg = read_ds("Calibrated/Encoder")
  )

  bias <- read_ds("Calibrated/Bias") |>
    rlang::set_names(c(
      "bias_fx.N",
      "bias_fy.N",
      "bias_fz.N",
      "bias_tx.Nm",
      "bias_ty.Nm",
      "bias_tz.Nm"
    )) |>
    as.list()

  attrs <- read_h5_attributes(
    h5,
    skip = c("NominalStimulus/Position", "NominalStimulus/Velocity")
  )

  data |>
    dplyr::mutate(!!!bias, !!!attrs)
}

#' Read all attributes in an HDF5 file
#'
#' @param h5 An open `hdf5r::H5File`.
#' @param skip Paths of objects whose attributes should be skipped.
#' @returns A named list of attribute values. Character values are
#'   trimmed of whitespace.
#' @noRd
read_h5_attributes <- function(h5, skip = character()) {
  objs <- c("/", h5$ls(recursive = TRUE)$name) |>
    setdiff(skip)

  attrs <- objs |>
    purrr::map(\(nm) {
      obj <- if (nm == "/") h5 else h5[[nm]]
      hdf5r::h5attributes(obj) |>
        purrr::map(\(a) if (is.character(a)) stringr::str_trim(a) else a)
    }) |>
    rlang::set_names(objs) |>
    purrr::compact()

  # add the object path as a prefix to attribute names that appear more
  # than once
  allnames <- purrr::list_c(purrr::map(attrs, names))
  dups <- unique(allnames[duplicated(allnames)])

  attrs |>
    purrr::imap(\(a, nm) {
      prefix <- stringr::str_replace_all(nm, "/", "_")
      names(a) <- dplyr::if_else(
        names(a) %in% dups,
        paste(prefix, names(a), sep = "_"),
        names(a)
      )
      a
    }) |>
    purrr::list_flatten(name_spec = "{inner}")
}

read_flapper_dataset <- function(
  dirname,
  fin,
  trialtype,
  trials = NULL,
  cutoff = 10
) {
  datadir <- proj_path("data_dir", dirname)

  files <- tibble(
    filename = fs::dir_ls(
      path = datadir,
      type = "file",
      regexp = "test.*\\.h5"
    ),
    trial = str_extract(filename, '(?<=test)(\\d+)', group = 1) |>
      as.numeric()
  )

  if (!is.null(trials)) {
    files <- files |>
      mutate(good = trial %in% trials)
  } else {
    files <- files |>
      mutate(good = TRUE)
  }

  if (nrow(files) == 0) {
    cli_alert_danger("No files found in {datadir}")
  } else {
    cli_alert_success(
      "{nrow(files)} files found in {datadir}: {sum(files$good)} good"
    )
  }

  data <-
    with(
      files |>
        filter(good),
      map2(
        filename,
        trial,
        \(n, t)
          read_flapper_data(n) |>
            mutate(filename = n, trial = t),
        .progress = TRUE
      )
    )

  data <-
    data |>
    bind_rows() |>
    rename(foillen.mm = Foillen_mm, flowspeed.mm = FlowSpeed_mm) |>
    mutate(fin = fin, trial_type = trialtype)

  data <-
    data |>
    group_by(trial) |>
    mutate(
      across(
        c(
          matches('^[tf][xyz].Nm?')
        ),
        \(x) smooth(x, t = t.s, cutoff = cutoff),
        .names = '{.col}_s'
      )
    ) |>
    rename_with(\(n) str_replace(n, '\\.N(m?)_s$', '_s.N\\1')) |>
    mutate(across(
      c(matches('^[tf][xyz].Nm?')),
      \(x) mean(x[between(t.s, -0.8, -0.2)]),
      .names = '{.col}_init'
    ))

  return(data)
}
