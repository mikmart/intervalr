group_overlapping <- function(x, ...)
{
  UseMethod("group_overlapping")
}

group_overlapping.interval_data <-
function(x, .spell_id = "spell", .by = key(x), .max_gap = 0L)
{
  if (is.null(.by))
    .raise_missing_by_error()
  .iv <- get_iv.interval_data(x)

  setkeyv(x, c(.by, .iv$start))

  # when intervals are ordered by starts, a spell ends if the start of a new
  # interval is later than the end of any previous interval (+ max gap)
  x[, .tmp_high := prev_cummax(get(.iv$end)), by = .by
  ][, (.spell_id) :=
      1L + cumsum(get(.iv$start) > .tmp_high + .max_gap), by = .by
  ][, .tmp_high := NULL]

  setkeyv(x, .by)

  return(x)
}
