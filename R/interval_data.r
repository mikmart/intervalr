
interval_data <- function(x, start, end, ...)
{
  UseMethod("interval_data")
}

interval_data.data.frame <- function(x, ...)
{
  x <- as.data.table(x)
  interval_data.data.table(x, ...) 
}

interval_data.data.table <- function(x, start, end, key = NULL)
{
  if (!is.null(key))
    setkeyv(x, key)
  
  int_vars <- list("start" = start, "end" = end)
  setattr(x, ".int_vars", int_vars)
  setattr(x, "class", base::union("interval_data", class(x)))
  
  return(invisible(x))
}

is.interval_data <- function(x) inherits(x, "interval_data")

get_iv <- function(...)
{
  UseMethod("get_iv")
}

get_iv.interval_data <- function(x)
{
  attr(x, ".int_vars")
}

set_iv <- function(...)
{
  UseMethod("set_iv")
}

set_iv.interval_data <- function(x, iv)
{
  setattr(x, ".int_vars", iv)
}
