prev_cummax <- function(x)
{
  shift(cummax(x), fill = x[1L])
}

cummax.Date <- function(x)
{
  (setattr(cummax(unclass(x)), "class", c("Date", "IDate")))
}

.raise_missing_by_error <- function()
{
  stop("you must supply '.by' for merging if 'x' is not keyed", call. = FALSE)
}
