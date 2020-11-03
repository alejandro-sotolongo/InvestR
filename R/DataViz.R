#' @export
viz_perf <- function(x, period = NULL) {

  if (!is.null(period)) {
    x <- change_freq(x, period)
  }
  wealth <- ret_to_price(x)
  df <- xts_to_dataframe(wealth)
  plot_wealth <- tidyr::pivot_longer(df, -Date)
  g_wealth <- ggplot(plot_wealth, aes(x = Date, y = value, color = name)) +
    geom_line() +
    xlab('') + ylab('') + labs(color = '', title = 'Cumulative Return')
  df <- xts_to_dataframe(x)
  plot_returns <- tidyr::pivot_longer(df, -Date)
  g_return <- ggplot(plot_returns, aes(x = Date, y = value, fill = name)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    xlab('') + ylab('') + labs(fill = '', title = paste0('Period Returns')) +
    scale_y_continuous(labels = scales::percent) +
    theme(axis.text.y = element_text(size = 7))
  dd <- drawdown(x)
  df <- xts_to_dataframe(dd)
  plot_dd <- tidyr::pivot_longer(df, -Date)
  g_drawdown <- ggplot(plot_dd, aes(x = Date, y = value, color = name)) +
    geom_line() +
    xlab('') + ylab('') + labs(color = '', title = 'Drawdowns') +
    scale_y_continuous(labels = scales::percent)
  cowplot::plot_grid(g_wealth, g_return, g_drawdown, ncol = 1,
                     rel_heights = c(2, 1, 1.25), align = 'v')
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
