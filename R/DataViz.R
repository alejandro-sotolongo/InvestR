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
  g_return <- ggplot(plot_returns, aes(x = Date, y = value, color = name)) +
    geom_line() +
    xlab('') + ylab('') + labs(color = '', title = paste0('Period Returns')) +
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

viz_wgt <- function(..., input_list = FALSE) {

  port_list <- list(...)
  if (input_list) {
    port_list <- port_list[[1]]
  }
  for (i in 1:length(port_list)) {
    if (i == 1) {
      wgt <- data.frame(
        Ticker = port_list[[i]]$sec_tick,
        Weight = port_list[[i]]$rebal_wgt
      )
    } else {
      wgt <- merge(x = wgt,
                   y = data.frame(Ticker = port_list[[i]]$sec_tick,
                                  Weight = port_list[[i]]$rebal_wgt),
                   all = TRUE,
                   by = 'Ticker')
    }
  }
  name_vec <- sapply(port_list, function(x) x$name)
  colnames(wgt) <- c('Ticker', make.unique(name_vec))

}
