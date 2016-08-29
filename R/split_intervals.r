split_intervals <- function(x, ...)
{
  UseMethod("split_intervals")
}

split_intervals.interval_data <-
function(x, breaks = "year")
{
  .iv <- get_iv.interval_data(x)
  .keyx <- key(x)

  # data.table containing intervals to break into
  break_dt <- .make_break_table(x, breaks)

  # foverlaps() requires data to be keyed by interval start and end
  setkeyv(x, unlist(.iv, use.names = FALSE))

  # overlap join - keep rows where intervals overlap
  ol <- foverlaps(break_dt, x)

  # calculate new intervals and their duration
  set(ol, j = .iv$start, value = pmax(ol[[.iv$start]], ol[[".lbr"]]))
  set(ol, j = .iv$end, value = pmin(ol[[.iv$end]], ol[[".rbr"]]))
  set(ol, j = ".dur",
    value = as.integer(ol[[.iv$end]]) - as.integer(ol[[.iv$start]]) + 1L
  )

  # restore old key
  setkeyv(x, .keyx)
  setkeyv(ol, .keyx)

  # add interval_data class and drop break variables
  interval_data(ol, start = .iv$start, end = .iv$end)
  ol[, c(".lbr", ".rbr") := NULL][]

  return(ol)
}

.make_break_table <-
function(x, breaks = c("year", "month", "week", "day"))
{
  if (is.data.table(breaks))
    return(breaks)
  if (is.character(breaks))
    breaks <- period_breaks(x, match.arg(breaks))

  breaks <- data.table(
    .lbr = shift(breaks),
    .rbr = breaks - 1L,
    .per_id = seq_along(breaks) - 1L
  )[-1L]

  setkeyv(breaks, c(".lbr", ".rbr"))
  return(breaks)
}
