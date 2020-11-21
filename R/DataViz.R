#' @title Plot Performance Summary
#' @param x xts object
#' @param period optional string to change the frequency of the xts
#' @import ggplot2
#' @importFrom cowplot plot_grid
#' @export
viz_perf <- function(x, period = NULL, plot_col = NULL) {

  x <- na.omit(x)
  if (!is.null(period)) {
    x <- change_freq(x, period)
  }
  wealth <- ret_to_price(x)
  df <- xts_to_dataframe(wealth)
  plot_wealth <- tidyr::pivot_longer(df, -Date)
  plot_wealth$name <- factor(plot_wealth$name, unique(plot_wealth$name))
  g_wealth <- ggplot(plot_wealth, aes(x = Date, y = value, color = name)) +
    geom_line() +
    xlab('') + ylab('') + labs(color = '', title = 'Cumulative Return') +
    theme_light()
  vol <- roll_vol(x)
  df <- xts_to_dataframe(vol)
  plot_vol <- tidyr::pivot_longer(df, -Date) 
  plot_vol$name <- factor(plot_vol$name, unique(plot_vol$name))
  g_roll_vol <- ggplot(plot_vol, aes(x = Date, y = value, color = name)) +
    geom_line() +
    xlab('') + ylab('') + labs(color = '', title = 'Rolling Volatility') +
    scale_y_continuous(labels = scales::percent) +
    theme_light()
  dd <- drawdown(x)
  df <- xts_to_dataframe(dd)
  plot_dd <- tidyr::pivot_longer(df, -Date)
  plot_dd$name <- factor(plot_dd$name, unique(plot_dd$name))
  g_drawdown <- ggplot(plot_dd, aes(x = Date, y = value, color = name)) +
    geom_line() +
    xlab('') + ylab('') + labs(color = '', title = 'Drawdowns') +
    scale_y_continuous(labels = scales::percent) +
    theme_light()
  if (!is.null(plot_col)) {
    g_wealth <- g_wealth + scale_color_manual(values = plot_col)
    g_roll_vol <- g_roll_vol + scale_color_manual(values = plot_col)
    g_drawdown <- g_drawdown + scale_color_manual(values = plot_col)
  }
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


viz_roll_style <- function(fund, fact, roll_period = 504) {
  
  dat <- roll_style_analysis(fund, fact, roll_period)
  df <- xts_to_dataframe(dat)
  plot_dat <- tidyr::pivot_longer(df, -Date)
  ggplot(plot_dat, aes(x = Date, y = value, fill = name)) +
    geom_area()
  
}


#' @export
viz_style_drift <- function(fund, fact, period = 'week', roll_period = 156,
                            .step = 1L) {
  
  roll_wgt <- roll_style_analysis(fund, fact, period, roll_period, .step)
  y_point <- roll_wgt[, 1] + roll_wgt[, 3] - roll_wgt[, 2] - roll_wgt[, 4]
  x_point <- roll_wgt[, 1] + roll_wgt[, 2] - roll_wgt[, 3] - roll_wgt[, 4]
  plot_xts <- cbind(x_point, y_point)
  first_date <- zoo::index(plot_xts)[1]
  last_date <- zoo::index(plot_xts)[nrow(plot_xts)]
  plot_df <- xts_to_dataframe(plot_xts)
  colnames(plot_df) <- c('Date', 'X', 'Y')
  ref_df <- data.frame(Label = c('Large Value', 'Small Value', 'Large Growth', 'Small Growth'),
                       X = c(-1, -1, 1, 1),
                       Y = c(1, -1, 1, -1),
                       Date = rep(last_date), 4)
  ggplot(plot_df, aes(x = X, y = Y, color = Date)) +
    geom_point() +
    scale_x_continuous(minor_breaks = seq(-1, 1, 1),
                       limits = c(-1.5, 1.5)) +
    scale_y_continuous(minor_breaks = seq(-1, 1, 1),
                       limits = c(-1.5, 1.5)) +
    geom_point(data = ref_df, aes(x = X, y = Y), color = 'darkgrey', size = 2) +
    geom_text(data = ref_df, mapping = aes(x = X, y = Y, label = Label),
              size = 3, nudge_y = 0.15, color = 'black') +
    xlab('') + ylab('') +
    theme_light() +
    theme(panel.grid.minor = element_line(color = 'grey', size = 0.5))
    
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
  perf <- sapply(xts_list_fill, cum_ret, remove_na = FALSE)
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
tbl_month_ret <- function(x, dig = 2) {
  
  ret <- change_freq(x, 'months')
  dt <- zoo::index(ret)
  month <- lubridate::month(dt)
  year <- lubridate::year(dt)
  year_adj <- year - year[1] + 1
  ret_vec <- as.numeric(ret)
  tbl <- matrix(nrow = max(year_adj), ncol = 13)
  for (i in 1:length(ret_vec)) {
    tbl[year_adj[i], month[i]] <- ret_vec[i]
  }
  year_ret <- change_freq(x, 'years')
  first_year <- year[1]
  date_start <- as.Date(paste0(first_year, '-01-01'))
  date_end <- as.Date(paste0(first_year, '-12-31'))
  first_year_ret <- prod(1 + x[paste0(date_start, '/', date_end)]) - 1
  tbl[, 13] <- c(first_year_ret, as.numeric(year_ret))
  tbl_fmt <- apply(tbl, 2, f_num_per, digits = dig)
  tbl_fmt <- data.frame(tbl_fmt)
  colnames(tbl_fmt) <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug',
                         'Sep', 'Oct', 'Nov', 'Dec', 'Yr')
  tbl_fmt$Year <- unique(year)
  tbl_fmt <- tbl_fmt[, c('Year', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
                         'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Yr')]
  res <- list()
  res$fmt <- tbl_fmt
  res$num <- tbl
  return(res)
}

#' @export
tbl_perf_stat <- function(fund, comp, rf, period = NULL) {
  
  combo <- combine_xts(fund, comp, rf, period = period, use_busday = FALSE)
  fund <- combo[, 1]
  comp <- combo[, 2:(ncol(combo) - 1)]
  rf <- combo[, ncol(combo)]
  rf_mu <- geo_ret(rf)
  x <- combo[, -ncol(combo)]
  ret <- geo_ret(x)
  xvol <- vol(x)
  xvol_down <- down_vol(x)
  dd <- drawdown(x)
  worst_dd <- apply(dd, 2, min)
  sharpe <- sharpe_ratio(x, rf)
  sortino <- sortino_ratio(x, rf_mu)
  ret_worst_dd <- geo_ret(x) / -(worst_dd)
  ecov <- excess_cov(x, rf)
  xbeta <- ecov[1, ] / diag(ecov)
  omega <- omega_ratio(x, method = 'standard')
  capt <- list()
  for (i in 1:ncol(comp)) {
    capt[[i]] <- up_down_capt(fund, comp[, i])
  }
  up_capt <- sapply(capt, '[[', 'up')[1, ]
  down_capt <- sapply(capt, '[[', 'down')[1, ]
  num <- rbind(ret, xvol, xvol_down, worst_dd, sharpe, sortino, ret_worst_dd,
               omega, xbeta, c(1, up_capt), c(1, down_capt))
  fmt <- num
  percent_rows <- c(1:4, 10:11)
  fmt[percent_rows, ] <- f_percent(num[percent_rows, ])
  num_rows <- 5:9
  fmt[num_rows, ] <- f_num(num[num_rows, ])
  fmt <- data.frame(fmt, row.names = NULL)
  fmt$Estimate <- c('Geometric Return', 'Volatility', 'Downside Volatility',
                    'Worst Drawdown', 'Sharpe Ratio', 'Sortino Ratio',
                    'Geo. Return / Worst DD', 'Omega Ratio', 'Beta', 
                    'Upside Capture', 'Downside Capture')
  fmt <- fmt[, c('Estimate', colnames(x))]
  res <- list()
  res$fmt <- fmt
  res$num <- num
  return(res)
} 


#' @export
tbl_quantile <- function(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  
  q <- apply(x, 2, quantile, na.rm = TRUE, probs = probs)
  df <- data.frame(apply(q, 2, f_percent))
  df$Percentile <- f_num(probs * 100, 0)
  df[, c('Percentile', colnames(x))]
}

#' @export
tbl_mv_reg <- function(fund, fact, rf, period = NULL) {
  
  if (is.null(period)) {
    period <- periodicity(fund)$units
  }
  a <- freq_to_scaler(period)
  fit <- mv_reg(fund, fact, rf, period)
  tbl_num <- matrix(nrow = ncol(fact) + 3, ncol = ncol(fund))
  coeff <- sapply(fit, '[[', 'coefficients')
  fit_summ <- lapply(fit, function(x) {summary(x)})
  r2 <- sapply(fit_summ, '[[', 'r.squared')
  adj_r2 <- sapply(fit_summ, '[[', 'adj.r.squared')
  tbl_num[1:nrow(coeff), ] <- coeff
  tbl_num[1, ] <- tbl_num[1, ] * a
  tbl_num[nrow(coeff) + 1, ] <- r2
  tbl_num[nrow(coeff) + 2, ] <- adj_r2
  tbl_fmt <- data.frame(tbl_num)
  tbl_fmt[1, ] <- f_percent(tbl_num[1, ])
  tbl_fmt[2:nrow(coeff), ] <- f_num(tbl_num[2:nrow(coeff), ])
  last_2_rows <- (nrow(coeff) + 1):nrow(tbl_fmt)
  tbl_fmt[last_2_rows, ] <- f_percent(tbl_num[last_2_rows, ])
  colnames(tbl_fmt) <- colnames(fund)
  tbl_fmt$Estimate <- c('Intercept (ann.)', colnames(fact), 'R-squared', 
                        'Adj. R-squared')
  tbl_fmt <- tbl_fmt[, c('Estimate', colnames(fund))]
  res <- list()
  res$fmt <- tbl_fmt
  res$num <- tbl_num
  res$fit <- fit
  res$fit_summ <- fit_summ
  return(res)
}


#' @export
kbl_mv_reg <- function(fund, fact, rf, period = NULL, t_stat_crit = 2) {
  
  tbl_list <- tbl_mv_reg(fund, fact, rf, period)
  t_stat <- sapply(tbl_list$fit_summ, function(x){x$coefficients[, 3]})
  tbl <- tbl_list$fmt
  for (i in 2:ncol(tbl)) {
    tbl[, i] <- cell_spec(tbl[, i], 
                          bold = c(ifelse(abs(t_stat[, i - 1]) > t_stat_crit, 
                                          TRUE, FALSE), FALSE, FALSE))
  }     
  kbl <- kable(tbl, escape = FALSE)
  kable_styling(kbl, latex_options = 'striped')
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
  x <- formatC(x, digits = digits, format = 'f', big.mark = ',')
  x[x == ' NA'] <- '-'
  return(x)
}

#' @export
f_num_per <- function(x, digits = 2) {
  x <- formatC(x * 100, digits = digits, format = 'f', big.mark = ',')
  x[x == ' NA'] <- '-'
  return(x)
}