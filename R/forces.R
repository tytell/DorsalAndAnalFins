thrust_lateral <- function(ang, fx, fy, units = 'degrees') {
  if (units == 'degrees') {
    unitscale = pi / 180
  } else {
    unitscale = 1
  }

  tibble(
    thrust.N = -fy * cos(ang * unitscale) + fx * sin(ang * unitscale),
    lateral.N = -fy * sin(ang * unitscale) - fx * cos(ang * unitscale),
    .keep = 'none'
  )
}

signed_max <- function(x, pctile = 1) {
  if (pctile != 1) {
    maxfn <- \(x) quantile(x, pctile)
    minfn <- \(x) quantile(x, 1 - pctile)
  } else {
    maxfn <- \(x) max(x)
    minfn <- \(x) min(x)
  }

  hi <- maxfn(x)
  lo <- minfn(x)

  if_else(abs(hi) > abs(lo), hi, lo)
}
