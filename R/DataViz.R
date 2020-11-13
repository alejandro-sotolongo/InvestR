#' @title Plot Performance Summary
#' @param x xts object
#' @param period optional string to change the frequency of the xts
#' @import ggplot2
#' @importFrom cowplot plot_grid
#' @export
viz_perf <- function(x, period = NULL) {

  x <- na.omit(x)
  if (!is.null(period)) {
    x <- change_freq(x, period)
  }
  wealth <- ret_to_price(x)
  df <- xts_to_dataframe(wealth)
  plot_wealth <- tidyr::pivot_longer(df, -Date)
  g_wealth <- ggplot(plot_wealth, aes(x = Date, y = value, color = name)) +
    geom_line() +
    xlab('') + ylab('') + labs(color = '', title = 'Cumulative Return') +
    theme_light()
  vol <- roll_vol(x)
  df <- xts_to_dataframe(vol)
  plot_vol <- tidyr::pivot_longer(df, -Date) 
  g_roll_vol <- ggplot(plot_vol, aes(x = Date, y = value, color = name)) +
    geom_line() +
    xlab('') + ylab('') + labs(color = '', title = 'Rolling Volatility') +
    scale_y_continuous(labels = scales::percent) +
    theme_light()
  dd <- drawdown(x)
  df <- xts_to_dataframe(dd)
  plot_dd <- tidyr::pivot_longer(df, -Date)
  g_drawdown <- ggplot(plot_dd, aes(x = Date, y = value, color = name)) +
    geom_line() +
    xlab('') + ylab('') + labs(color = '', title = 'Drawdowns') +
    scale_y_continuous(labels = scales::percent) +
    theme_light()
  cowplot::plot_grid(g_wealth, g_roll_vol, g_drawdown, ncol = 1,
                     rel_heights = c(2, 1, 1), align = 'v')
}


#' @title Plot Drawdowns
#' @param x xts object
#' @param period optional string to change the frequency of the xts
#' @import ggplot2
#' @export
viz_drawdown <- function(x, period = NULL) {
  
  x <- na.omit(x)
  if (!is.null(period)) {
    x <- change_freq(x, period)
  }
  dd <- drawdown(x)
  df <- xts_to_dataframe(dd)
  plot_dd <- tidyr::pivot_longer(df, -Date)
  ggplot(plot_dd, aes(x = Date, y = value, color = name)) +
    geom_line() +
    xlab('') + ylab('') + labs(color = '', title = 'Drawdowns') +
    scale_y_continuous(labels = scales::percent) +
    theme_light()
}


#' @export
tbl_cal_perf <- function(x, asof = NULL) {
  
  date_end <- zoo::index(x)[nrow(x)]
  date_start <- zoo::index(x)[1]
  if (is.null(asof)) {
    asof <- date_end
  }
  if (asof > date_end) {
    stop('as of date is more recent than the last date of the time-series x')
  }
  xwin <- c('dtd', 'wtd', 'mtd', 'qtd', 'ytd', 'ttm',
           '3 yr', '5 yr', '10 yr', '20 yr', '30 yr')
  date_vec <- lapply(xwin, cal_time, asof = asof)
  is_before_date_start <- as.Date(as.numeric(date_vec), origin = '1970-01-01') < date_start
  xts_list <- mapply(trunc_xts, x = list(x), date_start = date_vec, 
                     date_end = asof)
  xts_list_fill <- lapply(xts_list, fill_na_ret)
  perf <- sapply(xts_list_fill, cum_ret, use_busday = FALSE, remove_na = FALSE)
  perf[, 7] <- (1 + perf[, 7])^(1/3) - 1
  perf[, 8] <- (1 + perf[, 8])^(1/5) - 1
  perf[, 9] <- (1 + perf[, 9])^(1/10) - 1
  perf[, 10] <- (1 + perf[, 10])^(1/20) - 1
  perf[, 11] <- (1 + perf[, 11])^(1/30) - 1
  perf[, is_before_date_start] <- NA
  perf_out <- data.frame(perf)
  perf_out$Asset <- rownames(perf)  
  colnames(perf_out)[1:11] <- toupper(xwin)
  perf_out_sort <- perf_out[, c('Asset', toupper(xwin))]
  rownames(perf_out_sort) <- NULL
  perf_out_sort_fmt <- perf_out_sort
  perf_out_sort_fmt[, 2:12] <- apply(perf_out_sort_fmt[, 2:12], 2, f_percent)
  res <- list()
  res$fmt <- perf_out_sort_fmt
  res$num <- perf_out_sort
  return(res)
}

#' @export
f_percent <- function(x, digits = 2) {
  x_fmt <- formatC(x * 100, digits = digits, format = 'f')
  x_fmt_abs <- formatC(abs(x) * 100, digits = digits, format = 'f')
  x_per <- paste0(x_fmt, '%')
  less_0 <- x < 0
  less_0[is.na(less_0)] <- FALSE
  x_per[less_0] <- paste0('(', x_fmt_abs[less_0], '%)')
  x_per[x_per == ' NA%'] <- '-'
  return(x_per)
}

#' @export
f_num <- function(x, digits = 2) {
  x <- formatC(x, digits = 2, format = 'f', big.mark = ',')
  x[x == ' NA'] <- '-'
  return(x)
}
