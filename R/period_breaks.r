period_breaks <- function(x, period)
{
  UseMethod("period_breaks")
}

period_breaks.interval_data <-
function(x, period = c("year", "month", "week", "day"))
{
  period <- match.arg(period)
  .iv <- get_iv.interval_data(x)

  first_break <- lubridate::floor_date(min(x[[.iv$start]]), period)
  last_break <- lubridate::ceiling_date(max(x[[.iv$end]]), period)

  n_periods <- (last_break - first_break) / lubridate::duration(1L, period)
  first_break + 0:n_periods * lubridate::period(1L, period)
}
