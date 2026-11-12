#' Smooth a signal with a low-pass Butterworth filter
#'
#' Designs a Butterworth filter in second-order sections form and applies
#' it forwards and backwards with [gsignal::filtfilt()], so the result has
#' no phase shift. Because the filter is applied twice, the effective
#' order is twice `order`.
#'
#' @param x Numeric vector to smooth. Must not contain `NA`s.
#' @param cutoff Cutoff frequency in Hz.
#' @param t Time vector, in seconds, the same length as `x`. Used to
#'   estimate the sampling frequency if `sampfreq` is not given.
#' @param sampfreq Sampling frequency in Hz. Overrides `t`.
#' @param order Filter order.
#' @param type Filter type, passed to [gsignal::butter()].
#' @returns The smoothed vector, the same length as `x`.
#' @examples
#' \dontrun{
#' data |>
#'   mutate(across(fx.N:tz.Nm, \(x) smooth(x, cutoff = 10, t = t.s)))
#' }
smooth <- function(
  x,
  cutoff,
  t = NULL,
  sampfreq = NULL,
  order = 9,
  type = "low"
) {
  if (is.null(sampfreq)) {
    if (is.null(t)) {
      cli::cli_abort("Either {.arg t} or {.arg sampfreq} must be given.")
    }
    sampfreq <- 1 / stats::median(diff(t))
  }
  if (any(cutoff >= sampfreq / 2)) {
    cli::cli_abort(
      "{.arg cutoff} must be below the Nyquist frequency ({sampfreq / 2} Hz)."
    )
  }
  if (anyNA(x)) {
    cli::cli_abort("{.arg x} contains {.val NA} values.")
  }

  filt <- gsignal::butter(
    order,
    cutoff / (sampfreq / 2),
    type = type,
    output = "Sos"
  )
  gsignal::filtfilt(filt, x)
}
