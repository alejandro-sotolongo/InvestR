#' @importFrom bizdays create.calendar is.bizday
busday <- function(x, year_start = 1970, year_end = 2030) {

  bizdays::create.calendar('cal',
                           holidays = timeDate::holidayNYSE(year_start:year_end),
                           weekdays = c('saturday', 'sunday'))
  is_busday <- bizdays::is.bizday(zoo::index(x), 'cal')
  x[is_busday, ]
}


fill_na_ret <- function(x, rpl = 0) {
  apply(x, 2, .na_ret, rpl = rpl)
}


#' @title Fill missing price values with previous non-missing value
#' @note https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
fill_na_price <- function(x) {
  apply(x, 2, .na_price)
}


.na_ret <- function(x, rpl) {

  first_ret <- min(which(!is.na(x)))
  x[is.na(x)] <- rpl
  if (first_ret > 1) {
    x[1:first_ret] <- NA
  }
  return(x)
}


.na_price <- function(x) {
  ind <- which(!is.na(x))
  if (is.na(x[1])) {
    ind <- c(1, ind)
  }
  rep(x[ind], times = diff(c(ind, length(x) + 1)))
}


ret_to_price <- function(x) {

  price <- apply(x + 1, 2, cumprod)
  first_row <- xts(matrix(1, ncol = ncol(x)), zoo::index(x)[1] - 1)
  price_out <- rbind(first_row, price)
  colnames(price_out) <- colnames(x)
  return(price_out)
}


price_to_ret <- function(x) {

  ret <- x / lag.xts(x, 1) - 1
  ret[2:nrow(ret), ]
}


#' @importFrom lubridate ceiling_date
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


excess_ret <- function(x, rf, period = NULL) {
  
  if (!is.null(period)) {
    x <- change_freq(x, period)
    rf <- change_freq(rf, period)
  }
  join <- merge.xts(x, rf, fill = 0, join = 'left')
  rf_join <- join[, ncol(join), drop = TRUE]
  exc_ret <- apply(x, 2, function(a, b) {a - b}, b = rf_join)
  xts(exc_ret, zoo::index(x))
}


change_freq_na <- function(x, period, dtype) {
  
  apply(x, 2, .change_vec_na, period = period, dtype = dtype)
}

.change_vec_na <- function(x, period, dtype) {
  
  first_obs <- min(which(!is.na(x)))
  if (first_obs == 1) {
    return(change_freq(x, period, dtype))
  }
  if (dtype == 'return') {
    x_fill <- .na_ret(x)
  } else {
    x_fill <- .na_price(x)
  }
  change_freq(x_fill, period, dtype)
  x_out <- x_fill
  x_out[1:(first_obs - 1), ] <- NA
  return(x_out)
}
