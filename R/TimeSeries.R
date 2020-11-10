#' @title Subset time-series to business day only
#' @param x xts object
#' @param year_start beginning year to create business days
#' @param year_end ending year to create business days
#' @details In order to find business days this function creates a calendar
#' from timeDate's holidayNYSE calendar. The \code{year_start} and \code{year_end}
#' determine the length of the calendar which needs to be greater or equal to
#' the \code{x} time-series.
#' @return \code{x} with dates and corresponding values for business days
#' @importFrom bizdays create.calendar is.bizday
#' @export
#' @examples
#' busday(x, 1900, 2020)
#' busday(x, year_end = 2050)
busday <- function(x, year_start = 1970, year_end = 2030) {

  bizdays::create.calendar('cal',
                           holidays = timeDate::holidayNYSE(year_start:year_end),
                           weekdays = c('saturday', 'sunday'))
  is_busday <- bizdays::is.bizday(zoo::index(x), 'cal')
  x[is_busday, ]
}


#' @title Return xts with index of dates
#' @param x xts object
#' @note When using functions such as apply on xts objects an xts object with
#' a numeric index is returned, however the character dates are still in the
#' objects attributes. This function reconstructs the xts with a date index.
.return_xts <- function(x) {

  dt <- attr(x, 'dimnames')
  xts(x, as.Date(dt[[1]]))
}


#' @title Fill missing values for a return time-series
#' @param x xts with returns
#' @param rpl value to replace missing values
#' @return \code{x} with NAs replaced by \code{rpl}
#' @details Missing values at the beginning of the time-series are persevered.
#' Only the missing values after a non-missing value are replaced. E.g., the
#' series NA, NA, 1, 5, NA, 7 would be replaced with NA, NA, 1, 5, 0, 7.
#' @export
#' @examples 
#' fill_na_ret(x)
#' fill_na_ret(x, median(x, TRUE))
fill_na_ret <- function(x, rpl = 0) {
  ret <- apply(x, 2, .na_ret, rpl = rpl)
  .return_xts(ret)
}


#' @title Fill missing price values with previous non-missing value
#' @param x xts with prices
#' @return \code{x} with missing values replaced by the previous non-missing value
#' @note see https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
#' @export
#' @examples 
#' fill_na_price(x)
fill_na_price <- function(x) {
  price <- apply(x, 2, .na_price)
  .return_xts(price)
}


#' @title Fill missing returns in a vector
#' @param x vector of returns
#' @param rpl value to replace NA values
#' @examples 
#' x <- 1:10
#' x[3] <- NA
#' fill_na(x, 0)
.na_ret <- function(x, rpl) {

  first_ret <- min(which(!is.na(x)))
  x[is.na(x)] <- rpl
  if (first_ret > 1) {
    x[1:first_ret] <- NA
  }
  return(x)
}


#' @title Fill missing prices in a vector
#' @param x vector of prices
.na_price <- function(x) {
  ind <- which(!is.na(x))
  if (is.na(x[1])) {
    ind <- c(1, ind)
  }
  rep(x[ind], times = diff(c(ind, length(x) + 1)))
}


#' @title Convert returns to a price index
#' @param x xts of returns
#' @return a price index calculating from \code{x} and a initial value of 1
#' @export
ret_to_price <- function(x) {

  price <- apply(x + 1, 2, cumprod)
  first_row <- xts(matrix(1, ncol = ncol(x)), zoo::index(x)[1] - 1)
  price_out <- rbind(first_row, price)
  colnames(price_out) <- colnames(x)
  return(price_out)
}


#' @title Convert prices to returns (percent change)
#' @param x xts of prices
#' @return returns calculated from \code{x}
#' @export
price_to_ret <- function(x) {

  ret <- x / lag.xts(x, 1) - 1
  ret[2:nrow(ret), ]
}


#' @title Change time-series frequency
#' @param x xts object
#' @param period character string of the desired time-series periods
#' @param dtype character string of "return" or "price" to represent the data type
#' @return xts object with new frequency
#' @importFrom lubridate ceiling_date
#' @export
change_freq <- function(x, period = 'months', dtype = c('return', 'price')) {

  dtype <- tolower(dtype[1])
  if (dtype == 'return') {
    price <- ret_to_price(x)
  } else {
    price <- x
  }
  eo <- endpoints(price, on = period)
  price_new_freq <- price[eo, ]
  if (dtype == 'return') {
    data_out <- price_to_ret(price_new_freq)
  } else {
    data_out <- price_new_freq
  }
  if (tolower(period) %in% c('months', 'quarters', 'years')) {
    zoo::index(data_out) <- lubridate::ceiling_date(zoo::index(data_out),
                                                    'months') - 1
  }
  return(data_out)
}


#' @title Subtract risk-free from returns
#' @param x xts object of returns
#' @param rf xts object with the risk-free return time-series
#' @param period optional period to change the frequency, default value of NULL
#' will not change the frequency and leave the time-series as is
#' @return xts object containing \code{x} less \code{rf}
#' @export
excess_ret <- function(x, rf, period = NULL) {

  if (ncol(rf) > 1) {
    rf <- rf[, 1]
    warning('rf was more than one column, taking the first column')
  }
  if (!is.null(period)) {
    x <- change_freq(x, period)
    rf <- change_freq(rf, period)
  }
  join <- merge.xts(x, rf, fill = 0, join = 'left')
  rf_join <- join[, ncol(join), drop = TRUE]
  exc_ret <- apply(x, 2, function(x, y) {x - y}, y = rf_join)
  xts(exc_ret, zoo::index(x))
}


#' @title Change frequency in time-series with missing values
#' @param x xts object
#' @param period character to represent desired frequency
#' @param dtype character to specify if \code{x} is a 'return' or 'price' 
#' time-series
#' @note The missing values are filled with either \code{0} for returns and the
#' most recent (i.e., lagged) non-missing value for prices.
#' @return xts object with new \code{period} 
#' @export
change_freq_na <- function(x, period, dtype) {

  x_new_freq <- .change_vec_na(x[, 1], period, dtype)
  if (ncol(x) > 1) {
    for (i in 2:ncol(x)) {
      x_new_freq <- cbind(x_new_freq, .change_vec_na(x[, i], period, dtype))
    }
  }
  return(x_new_freq)
}


.change_vec_na <- function(x, period, dtype) {

  first_obs <- min(which(!is.na(x)))
  if (first_obs == 1) {
    return(change_freq(x, period, dtype))
  }
  if (dtype == 'return') {
    x_fill <- x
    x_fill[is.na(x)] <- 0
  } else {
    x_fill <- .na_price(x)
  }
  x_new_freq <- change_freq(x_fill, period, dtype)
  last_na_date <- zoo::index(x)[first_obs - 1]
  x_new_freq[paste0('/', last_na_date) ] <- NA
  return(x_new_freq)
}


#' @title Convert string frequency to numeric scale
#' @param period string to represent frequency
#' @return numeric scale
#' @export
freq_to_scaler <- function(period) {

  switch(tolower(period),
       days = 252,
       weeks = 52,
       months = 12,
       quarters = 4,
       years = 1
  )
}


#' @title Combine multiple xts objects into one xts object
#' @param period optional string to specify a desired common frequency
#' @param dtype string to specify if we are combining 'return' or 'price' 
#' time-series
#' @param use_busday boolean to strip time-series to business day only
#' (NYSE calendar)
#' @param comm_start boolean to truncate the combined time-series to the 
#' latest inception of the individual xts objects
#' @param comm_end boolean to truncate the combined time-series to the earliest
#' ending date of the individual xts objects
#' @param na_rpl boolean to replace missing values to \code{0} for returns and
#' the lagged non-missing value for prices
#' @param input_list boolean to specify if a list of xts objects are being 
#' past, see vignette
#' @return the combined xts object
#' @export
combine_xts <- function(..., period = NULL, dtype = c('return', 'price'),
                        use_busday = TRUE, comm_start = TRUE, comm_end = TRUE,
                        na_rpl = TRUE, input_list = FALSE) {

  dtype <- tolower(dtype)[1]
  xts_list <- list(...)
  if (input_list) {
    xts_list <- xts_list[[1]]
  }
  if (!is.null(period)) {
    xts_list <- lapply(xts_list, change_freq_na, period = period, dtype = dtype)
  }
  xts_combo <- do.call(cbind, xts_list)
  if (use_busday) {
    xts_combo <- busday(xts_combo)
  }
  if (comm_start) {
    dt_raw <- sapply(xts_list, function(x) min(zoo::index(na.omit(x))))
    xts_combo <- xts_combo[paste0(max(as.Date(dt_raw, origin = '1970-01-01')), '/')]
  }
  if (comm_end) {
    dt_raw <- sapply(xts_list, function(x) max(zoo::index(na.omit(x))))
    xts_combo <- xts_combo[paste0('/', min(as.Date(dt_raw, origin = '1970-01-01')))]
  }
  if (na_rpl) {
    if (dtype == 'return') {
      xts_combo <- fill_na_ret(xts_combo)
    } else {
      xts_combo <- fill_na_price(xts_combo)
    }
  }
  return(xts_combo)
}


#' @title Truncate xts object
#' @param x xts object
#' @param date_start first date to truncate
#' @param date_end last date to truncate
#' @export
trunc_xts <- function(x, date_start = NULL, date_end = NULL) {

  if (!is.null(date_start)) {
    x <- x[paste0(date_start, '/')]
  }
  if (!is.null(date_end)) {
    x <- x[paste0('/', date_end)]
  }
  return(x)
}


#' @export
xts_to_dataframe <- function(x) {

  date_vec <- zoo::index(x, origin = '1970-01-01')
  df <- data.frame(Date = as.Date(date_vec), x, row.names = NULL)
  if (!is.null(colnames(x))) {
    colnames(df)[2:ncol(df)] <- colnames(x)
  }
  return(df)
}


#' @title Calendar time helper
#' @param xwin string to represent calendar window
#' @param asof ending date of window
#' @return beginning date of window
#' @export
cal_time <- function(xwin = c('dtd', 'wtd', 'mtd', 'qtd', 'ytd', 'ttm',
                              '3 yr', '5 yr', '10 yr', '20 yr', '30 yr'),
                     asof = Sys.Date()) {
  
  xwin <- tolower(xwin[1])
  if (!xwin %in% c('dtd', 'wtd', 'mtd', 'qtd', 'ytd', 'ttm',
                   '3 yr', '5 yr', '10 yr', '20 yr', '30 yr')) {
    stop('wxin must be one of: dtd, wtd, mtd, qtd, ytd, ttm, 3 yr, 5 yr, 10 yr, 
         20 yr, 30 yr')
  }
  xyear <- lubridate::year(asof)
  xmon <- lubridate::month(asof)
  xday <- lubridate::day(asof)
  switch (xwin,
          dtd = asof,
          wtd = lubridate::floor_date(asof, 'week') + 1,
          mtd = lubridate::floor_date(asof, 'month'),
          qtd = lubridate::floor_date(asof, 'quarter'),
          ytd = lubridate::floor_date(asof, 'year'),
          ttm = as.Date(paste0(xyear - 1, '-', xmon, '-', xday)),
          '3 yr' = as.Date(paste0(xyear - 3, '-', xmon, '-', xday)),
          '5 yr' = as.Date(paste0(xyear - 5, '-', xmon, '-', xday)),
          '10 yr' = as.Date(paste0(xyear - 10, '-', xmon, '-', xday)),
          '20 yr' = as.Date(paste0(xyear - 20, '-', xmon, '-', xday)),
          '30 yr' = as.Date(paste0(xyear - 30, '-', xmon, '-', xday))
  )
}
