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


change_freq <- function(x, period = 'months', dtype = c('return', 'price')) {

}
