merge_overlapping <- function(x, ...)
{
  UseMethod("merge_overlapping")
}

merge_overlapping.data.table <- function(x, start, end, ...)
{
  interval_data(x, start, end)
  merge_overlapping.interval_data(x, ...)
}

merge_overlapping.interval_data <-
function(x, .spell_id = NULL, .by = key(x), .max_gap = 0L)
{
  if (is.null(.by))
    .raise_missing_by_error()

  drop_spell <- is.null(.spell_id)
  if (drop_spell)
    .spell_id <- ".tmp_.spell_id"

  .iv <- get_iv.interval_data(x)
  x <- copy(x)

  # identify overlapping intervals within .by groups
  group_overlapping.interval_data(x, .spell_id, .by, .max_gap)

  # collapse intervals in overlap groups to a single row
  merged <- x[, .(
    start = min(get(.iv$start)),  # earliest start
    end   = max(get(.iv$end))     # latest end
  ), by = c(.by, .spell_id)]

  # rename interval start and end to match original data
  setnames(merged, names(.iv), unlist(.iv, use.names = FALSE))
  set_iv.interval_data(merged, .iv)
  setkeyv(merged, .by)

  if (drop_spell)
    merged[, (.spell_id) := NULL][]

  return(merged)
}
