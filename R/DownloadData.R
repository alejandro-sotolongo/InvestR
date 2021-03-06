#' @title Download data from Tiingo
#' @param ticker string to represent security ticker
#' @param t_api string to represent your Tiingo API, see details
#' @param try_mode boolean to use try / catch expression
#' @return xts of historical returns
#' @details Tiingo provies end of day prices for stocks, ETFs, and mutual funds.
#' You can set up a free account to get an API. www.tiingo.com
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


#' @title Download time-series from FRED database
#' @param ticker string of time-series to download
#' @param try_mode boolean to use try-error handling
#' @return xts containing dates and economic data
#' @export
#' @details see https://fred.stlouisfed.org for more info
#' @examples
#' indust_prod <- download_fred('INDPRO')
download_fred <- function(ticker, try_mode = TRUE) {
  
  tmp <- tempfile()
  f_url <- paste0('https://fred.stlouisfed.org/series/',
                  ticker,
                  '/downloaddata/',
                  ticker,
                  '.csv')
  if (try_mode) {
    getdat <- try(download.file(f_url, destfile = tmp))
    if ('try-error' %in% class(getdat)) {
      warning(paste0('could not download ', ticker))
      return(NULL)
    } else {
      download.file(f_url, destfile = tmp)
    }
  }
  dat <- read.csv(tmp, na.string = '.')
  res <- xts(dat[, 2], as.Date(dat[, 1]))
  colnames(res) <- ticker
  return(res)
}


#' @title Download data with daily frequency from Ken French datalibrary
#' @param data_url subset of url specific to data, see details
#' @param skip how many header rows in csv file to skip, see details
#' @return xts with time-series
#' @export
#' @details the base url for all downloads is
#' 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/', each
#' csv download has a unique url to add after the base, e.g., the \code{data_url}
#' for momentum is 'F-F_Momentum_Factor_daily_CSV.zip'. The amount of rows to 
#' skip is different for each dataset. You can check by downloading the csv in
#' the datalibray https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
#' and seeing how many header rows above the data there are. As of 11/1/2020 some
#' common skips are: daily 3 factor = 4, daily momentum = 12, daily 10 sector = 9.
download_french <- function(data_url, skip) {
  
  tmp <- tempfile()
  base_url <- 'http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/'
  full_url <- paste0(base_url, data_url)
  download.file(full_url, destfile = tmp)
  uzfile <- unzip(tmp)
  clean_french_ret <- function(x) {
    x <- gsub('  ', '', x)
    x <- gsub(' ', '', x)
    x <- as.numeric(x)
    x[x >= 99.99] <- NA
    x[x <= -99.99] <- NA
    x / 100
  }
  dat <- read.csv(uzfile, skip = skip)
  dat$X <- as.Date(as.character(dat$X), format = '%Y%m%d')
  dat[, 2:ncol(dat)] <- apply(dat[, 2:ncol(dat), drop = FALSE], 2, clean_french_ret)
  dat <- na.omit(dat)
  res <- xts(dat[, 2:ncol(dat)], as.Date(dat[, 1]))
  return(res)
}


#' @export
download_french_rf <- function(skip = 4) {
  
  tmp <- tempfile()
  url <- 'https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip'
  download.file(url, destfile = tmp)
  uzfile <- unzip(tmp)
  clean_french_ret <- function(x) {
    x <- gsub('  ', '', x)
    x <- gsub(' ', '', x)
    x <- as.numeric(x)
    x[x >= 99.99] <- NA
    x[x <= -99.99] <- NA
    x / 100
  }
  dat <- read.csv(uzfile, skip = skip)
  dat$X <- as.Date(as.character(dat$X), format = '%Y%m%d')
  dat[, 2:ncol(dat)] <- apply(dat[, 2:ncol(dat), drop = FALSE], 2, clean_french_ret)
  dat <- na.omit(dat)
  dat <- dat[, c(1, 5)]
  dat_ext <- data.frame(X = seq.Date(dat$X[nrow(dat)] + 1, Sys.Date(), 'days'), 
                        RF = dat$RF[nrow(dat)])
  dat <- rbind(dat, dat_ext)
  xts(dat[, 2], as.Date(dat[, 1]))
}


download_shiller <- function() {
  
  tmp <- tempfile()
  s_url <- 'http://www.econ.yale.edu/~shiller/data/ie_data.xls'
  curl::curl_download(s_url, tmp)
  dat <- readxl::read_xls(tmp, sheet = 'Data', skip = 7)
  colnames(dat)[7] <- 'Interest Rate GS10'
  colnames(dat)[8] <- 'Real Price'
  colnames(dat)[9] <- 'Real Dividend'
  colnames(dat)[10] <- 'Real TR Price'
  colnames(dat)[11] <- 'Real Earnings'
  colnames(dat)[12] <- 'Real TR Earnings'
  colnames(dat)[13] <- 'CAPE'
  colnames(dat)[15] <- 'TR CAPE'
  dt <- formatC(dat$Date, digits = 2, format = 'f')
  yr <- substr(dt, 1, 4)
  mo <- substr(dt, 6, 7)
  dy <- rep('01', length(mo))
  dt <- as.Date(paste(yr, mo, dy, sep = '-'))
  eom <-  lubridate::ceiling_date(dt, unit = 'month') - 1
  dat$Date <- as.Date(eom, origin = '1970-01-01')
  valid_row <- !is.na(dat$Date)
  dat <- dat[valid_row, c(1:13, 15)]
  dat$CAPE <- as.numeric(dat$CAPE)
  dat$`TR CAPE` <- as.numeric(dat$`TR CAPE`)
  res <- xts(dat[, 2:ncol(dat)], dat[[1]])
}


download_russell <- function(key_link) {
  
  data_url <- paste0('https://research.ftserussell.com/products/russell-index-values/home/getfile?id=valueshist_',
                     key_link, '.csv')
  tmp <- tempfile()
  curl::curl_download(data_url, tmp)  
  dat <- read.csv(tmp)
  price <- xts(dat[, 4], as.Date(dat[, 2], format = '%m/%d/%Y'))
  ret <- price_to_ret(price)
  colnames(ret) <- iconv(dat[1, 1], from = 'UTF-8', to = 'ASCII//TRANSLIT')
  return(ret)
}


read_research_affiliates <- function(workbook) {
  
  exp_ret <- readxl::read_excel(workbook, 2, range = 'B4:L48')
  exp_ret_all <- readxl::read_excel(workbook, 3, range = 'B4:L121')
  hist_ret <- readxl::read_excel(workbook, 4, range = 'B5:L48')
  exp_corr <- readxl::read_excel(workbook, 5, range = 'B4:AC31')
  exp_cov <- readxl::read_excel(workbook, 5, range = 'B35:AC62')
  port_wgt <- readxl::read_excel(workbook, 6, range = 'B4:AC21')
  cape <- readxl::read_excel(workbook, 7, range = 'B4:M36')
  yield <- readxl::read_excel(workbook, 8, range = 'B4:R25')
  growth <- readxl::read_excel(workbook, 9, range = 'B4:E26')
  res <- list()
  res$exp_ret <- exp_ret
  res$exp_ret_all <- exp_ret_all
  res$hist_ret <- hist_ret
  res$exp_corr <- exp_corr
  res$exp_cov <- exp_cov
  res$port_wgt <- port_wgt
  res$cape <- cape
  res$yield <- yield
  res$growth <- growth
  return(res)
}
