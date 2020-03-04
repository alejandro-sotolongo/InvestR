#' @export
download_tiingo <- function(ticker, t_api, try_mode = TRUE) {

  t_url <- paste0('https://api.tiingo.com/tiingo/daily/',
                  ticker,
                  '/prices?startDate=1970-01-01',
                  '&endDate=', Sys.Date(),
                  '&token=', t_api)
  if (try_mode) {
    dat <- try(jsonlite::read_json(t_url))
    if ('try-error' %in% class(dat))
      return(NULL)
  } else {
    dat <- jsonlite::read_json(t_url)
  }
  date_raw <- sapply(dat, '[[', 'date')
  date_vec <- as.Date(date_raw)
  price_vec <- sapply(dat, '[[', 'adjClose')
  price <- xts(price_vec, date_vec)
  colnames(price) <- ticker
  ret <- price_to_ret(price)
  return(ret)
}
