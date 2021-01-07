#' @export
viz_comp_2_ret <- function(x, period = NULL, plot_col = NULL, 
                           plot_delta = c('line', 'bar'), title = '') {
  
  if (ncol(x) != 2) {
    stop('x must be two time-series organized by columns')
  }
  plot_delta <- tolower(plot_delta[1])
  x <- na.omit(x)
  if (!is.null(period)) {
    x <- change_freq(x, period)
  }
  wealth <- ret_to_price(x)
  df <- xts_to_dataframe(wealth)
  delta <- x[, 1] - x[, 2]
  plot_wealth <- tidyr::pivot_longer(df, -Date)
  plot_wealth$name <- factor(plot_wealth$name, unique(plot_wealth$name))
  g1 <- ggplot(plot_wealth, aes(x = Date, y = value, color = name)) +
    geom_line() +
    xlab('') + ylab('') + labs(color = '', title = title) +
    theme_light()
  if (!is.null(plot_col)) {
    g1 <- g1 + scale_color_manual(values = plot_col)
  } else {
    g1 <- g1 + scale_color_manual(values = c('dodgerblue', 'darkgrey'))
  }
  df$Delta <- c(0, as.numeric(delta))
  df$CumDelta <- cumprod(1 + df$Delta) - 1
  df$UpDown <- ifelse(df$Delta >= 0, 'Up', 'Down')
  if (plot_delta == 'bar') {
    g2 <- ggplot(df[, c(1, 4:5)], aes(x = Date, y = Delta, fill = UpDown)) +
      geom_bar(stat = 'identity', width = 8) +
      scale_fill_manual(values = c('darkred', 'darkgreen')) +
      scale_y_continuous(labels = scales::percent) +
      xlab('') + labs(fill = '') + ylab('') + 
      theme_light()
  } else {
    g2 <- ggplot(df, aes(x = Date, y = CumDelta, fill = 'Delta')) +
      geom_area() +
      xlab('') + labs(fill = '') + ylab('') + 
      theme_light()
  }
  cowplot::plot_grid(g1, g2, ncol = 1, rel_heights = c(2, 1), align = 'v')
}


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


#' @export
viz_roll_style <- function(fund, fact, period = 'months', roll_period = 36, 
                           na_eps = 0) {
  
  dat <- roll_style_analysis_with_na(fund, fact, period, roll_period, na_eps)
  df <- xts_to_dataframe(dat)
  plot_dat <- tidyr::pivot_longer(df, -Date)
  ggplot(plot_dat, aes(x = Date, y = value, fill = name)) +
    geom_area() +
    scale_y_continuous(labels = scales::percent) +
    labs(fill = '') + xlab('') + ylab('') +
    theme_light()
}


#' @export
viz_style_drift <- function(fund, fact, period = 'week', roll_period = 156,
                            .step = 1L, comp_plot = FALSE) {
  
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
  g1 <- ggplot(plot_df, aes(x = X, y = Y, color = Date)) +
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
  if (comp_plot) {
    n_fact <- ncol(fact)
    dat <- combine_xts(fund, fact, roll_wgt, period = period, use_busday = FALSE)
    style_port <- rowSums(dat[, 2:(n_fact + 1)] * dat[, (n_fact + 2):ncol(dat)])
    x <- cbind(dat[, 1], style_port)
    colnames(x)[2] <- 'Style Portfolio'
    g2 <- viz_comp_2_ret(x)
    res <- list()
    res$style_plot <- g1
    res$comp_plot <- g2
    return(res)
  } else {
    return(g1)
  }
}


#' @export
viz_style <- function(fund, fact) {
  
  x <- combine_xts(fund, fact, use_busday = FALSE)
  wgt <- style_analysis(x[, 1], x[, 2:ncol(x)])
  df <- data.frame(Style = colnames(fact), Weight = wgt)
  df$Style <- as.factor(df$Style)
  ggplot(df, aes(x = Style, y = Weight)) +
    geom_col() +
    scale_x_discrete(limits = rev(df$Style)) +
    coord_flip() +
    xlab('') + 
    scale_y_continuous(labels = scales::percent) +
    theme_light()
}


#' @export
viz_capm <- function(x) {
  
  ret <- geo_ret(x)
  xvol <- vol(x)
  df <- data.frame(Asset = colnames(x), Mu = ret, Sigma = xvol)
  ggplot(df, aes(x = Sigma, y = Mu, color = Asset)) +
    geom_point(size = 3) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    xlab('Volatility') + ylab('Geometric Return') +
    theme_light()
  
}


#' @export
viz_capm_period <- function(x, scenario = NULL, period = NULL) {
  
  x <- na.omit(x)
  if (is.null(scenario)) {
    scenario <- default_equity_scenario()
  }
  df <- data.frame(asset = NA, mu = NA, sigma = NA, date = NA)
  j <- 1
  for (i in 1:nrow(scenario)) {
    period_ret <- trunc_xts(x, date_start = scenario[i, 1], 
                            date_end = scenario[i, 2])
    if (length(period_ret) > 0) {
      df[j:(j + ncol(x) - 1), 'asset'] = colnames(x)
      if (scenario[i, 2] - scenario[i, 1] > 364) {
        mu <- geo_ret(period_ret) 
      } else {
        mu <- apply(period_ret + 1, 2, prod, na.rm = TRUE) - 1
      }
      df[j:(j + ncol(x) - 1), 'mu'] <- mu
      df[j:(j + ncol(x) - 1), 'sigma'] <- vol(x)
      df[j:(j + ncol(x) - 1), 'date'] <- rep(paste0(format(scenario[i, 1], '%b %Y'), ' to ', 
                            format(scenario[i, 2], '%b %Y')), ncol(x))
      j <- j + ncol(x)
    }
  }
  df$date <- factor(df$date, unique(df$date))
  df$asset <- factor(df$asset, unique(df$asset))
  ggplot(df, aes(x = sigma, y = mu, col = asset)) +
    geom_point() +
    facet_wrap(.~ date, scales = 'free') +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = 'Risk / Return', 
         subtitle = 'Returns for periods less than one year are not annualized')
}


#' @export
default_equity_scenario <- function() {
  
  start_dt <- c('1998-08-31',
                '2000-03-27',
                '2002-10-10',
                '2007-10-10',
                '2009-03-10',
                '2015-07-21',
                '2016-02-12',
                '2018-09-21',
                '2018-12-25',
                '2020-02-20',
                '2020-03-24')
  end_dt <- c('2000-03-26',
              '2002-10-09',
              '2007-10-09',
              '2009-03-09',
              '2015-07-20',
              '2016-02-11',
              '2018-09-20',
              '2018-12-24',
              '2020-02-19',
              '2020-03-23')
  res <- data.frame(start = as.Date(start_dt),
                    end = c(as.Date(end_dt), Sys.Date()))
  return(res)
}


#' @export
viz_ret_worst_dd <- function(x, text_size = 4) {
  
  ret <- geo_ret(x)
  dd <- drawdown(x)
  worst_dd <- -apply(dd, 2, min)
  df <- data.frame(Asset = colnames(x), Return = ret, Risk = worst_dd)
  ggplot(df, aes(x = Risk, y = Return, color = Asset, label = Asset)) +
    geom_point(size = 3) +
    geom_smooth() +
    ggrepel::geom_text_repel(size = text_size) +
    ylab('Geo. Return') + xlab('Worst Drawdown') +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    theme_light() +
    theme(legend.position = 'none')
}


#' @export
viz_pdf <- function(x, last_n = 5) {
  
  if (ncol(x) > 1) {
    stop('x must be univariate')
  }
  x <- na.omit(x)
  den <- density(as.numeric(x))
  den_df <- data.frame(X = den$x, Y = den$y, Date = NA)
  y_inter <- findInterval(tail(x, last_n), den$x)
  last_n_df <- data.frame(X = tail(x, last_n), Y = den$y[y_inter],
                          Date = zoo::index(x)[(nrow(x) - last_n + 1):nrow(x)])
  colnames(last_n_df) <- c('X', 'Y', 'Date')
  ggplot(den_df, aes(x = X, y = Y)) + 
    geom_line() +
    geom_point(aes(x = X, y = Y, color = Date), data = last_n_df, size = 3) +
    scale_x_continuous(labels = scales::percent) +
    xlab('Return') + ylab('Density') +
    labs(color = 'Recent Returns') +
    theme_light() 
}


#' @export
viz_pdf_risk <- function(x) {
  
  if (ncol(x) > 1) {
    stop('x must be univariate')
  }
  x <- na.omit(x)
  den <- density(as.numeric(x))
  den_df <- data.frame(X = den$x[den$x < 0], Y = den$y[den$x < 0])
  q5 <- quantile(x, 0.05)
  cvar <- mean(quantile(x, seq(0, 0.05, 0.01)))
  y_1 <- max(den$y)
  y_2 <- (max(den$y)) / 2
  ggplot(den_df, aes(x = X, y = Y)) +
    geom_line() +
    scale_x_continuous(labels = scales::percent) +
    xlab('Return') + ylab('Density') +
    geom_vline(xintercept = q5, col = 'red') +
    geom_vline(xintercept = cvar, col = 'brown') +
    annotate('text', label = paste0('95 VaR ', f_percent(q5)),
             x = q5, y = y_1, color = 'red', size = 3) +
    annotate('text', label = paste0('95 cVaR: ', f_percent(cvar)),
             x = cvar, y = y_2, color = 'brown', size = 3) +
    theme_minimal()
}


#' @export
viz_corr <- function(x, text_size = 3) {
  
  n_assets <- ncol(x)
  n_rep_letter <- n_assets %/% 26 + 1
  letter_lbl <- rep(LETTERS, n_rep_letter)
  letter_lbl <- letter_lbl[1:n_assets]
  xcorr <- cor(x, use = 'pairwise')
  rownames(xcorr) <- letter_lbl
  colnames(xcorr) <- paste0(colnames(xcorr), ' - ', letter_lbl)
  df <- reshape2::melt(xcorr)
  df$Var2 <- as.factor(df$Var2)
  df$lab <- f_num(df$value)
  ggplot(df, aes(x = Var1, y = Var2, fill = value, label = lab)) +
    geom_tile() +
    scale_y_discrete(limits = unique(rev(df$Var2))) +
    scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red') +
    geom_text(size = text_size) +
    xlab('') + ylab('') +
    scale_x_discrete(position = 'top') +
    theme_minimal() +
    theme(legend.position = 'none')
}


#' @export
viz_dendro <- function(x) {
  
  xcorr <- cor(x, use = 'pairwise')
  hc <- pca_hclust(xcorr)
  plot(hc, hang = -1)
}


#' @export
viz_pca <- function(x, n_pc = 4) {
  
  p <- psych::pca(x, n_pc)
  p_loadings <- data.frame(p$loadings[, 1:n_pc])
  p_loadings$Asset <- factor(rownames(p_loadings), rev(rownames(p_loadings)))
  tidy_dat <- tidyr::pivot_longer(p_loadings, -Asset, values_to = 'values',
                                  names_to = 'series')
  chart_loadings <- ggplot(tidy_dat, aes(x = Asset, y = values, fill = Asset)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    coord_flip() +
    facet_wrap(. ~ series) +
    scale_fill_manual(values = c(rep('darkgrey', ncol(x) - 1), 'indianred3')) +
    ylab('') + xlab('') +
    theme_light() +
    theme(legend.position = 'none')
  var_expl <- p$values[1:n_pc] / sum(p$values)
  cum_var_expl <- data.frame(PC = c(paste0('PC ', 1:n_pc), 'Remaining Components'),
                             var = c(var_expl, 1 - sum(var_expl)))
  cum_var_expl$lbl <- f_percent(cum_var_expl$var)
  cum_var_expl$PC <- as.factor(cum_var_expl$PC)
  chart_var_expl <- ggplot(cum_var_expl, aes(x = PC, y = var, fill = PC, label = lbl)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    scale_x_discrete(limits = rev(cum_var_expl$PC)) +
    coord_flip() +
    ylab('Variance Explained') +
    scale_y_continuous(labels = scales::percent) +
    xlab('') +
    geom_text() +
    theme_light() +
    theme(legend.position = 'none')
  res <- list()
  res$loadings <- chart_loadings
  res$var_expl <- chart_var_expl
  return(res)
}


#' @export
viz_factor_ret <- function(fund, fact, rf, period = NULL, net_rf_y = TRUE, 
                           net_rf_x = TRUE) {
  
  if (is.null(period)) {
    period <- periodicity(fund)$units
  }
  fit <- mv_reg(fund, fact, rf, period, net_rf_y, net_rf_x)
  model <- fit[[1]]$model
  ret <- geo_ret(model, period = period)
  coeff <- c(NA, fit[[1]]$coefficients)
  coeff[2] <- coeff[2] * freq_to_scaler(period)
  contr <- c(ret[1], coeff[2], coeff[3:length(coeff)] * ret[2:length(ret)])
  df <- data.frame(Name = c(colnames(fund), 'Resid.', colnames(fact)),
                   Coeff = coeff,
                   Return = c(ret[1], coeff[2], ret[2:length(ret)]),
                   Contr.to.Ret = contr)
  tidy_df <- tidyr::pivot_longer(df, -Name, names_to = 'Series', 
                                 values_to = 'Estimate')
  tidy_df$Series <- factor(tidy_df$Series, unique(tidy_df$Series))
  tidy_df$Name <- factor(tidy_df$Name, unique(tidy_df$Name)) 
  ggplot(tidy_df, aes(x = Name, y = Estimate, fill = Name)) + 
    geom_bar(stat = 'identity') +
    facet_wrap(.~ Series, scales = 'free') +
    scale_x_discrete(limits = rev(unique(tidy_df$Name))) + 
    scale_fill_manual(values = c('gainsboro', rep('grey36', nrow(df) - 1))) +
    coord_flip() +
    labs(Name = '') + xlab('') + ylab('') +
    theme_light() +
    theme(legend.position = 'none')
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
  # TO-DO error catch if returns don't end on same day
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
  perf_out_sort_fmt[, 2:12] <- apply(perf_out_sort_fmt[, 2:12], 2, f_num_per)
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
tbl_risk_quantile <- function(x, probs = seq(0.01, 0.05, by = 0.01),
                              period = NULL, annualize = TRUE) {
  
  if (annualize) {
    if (is.null(period)) {
      period <- periodicity(x)$units
    }
    a <- freq_to_scaler(period)
  } else {
    a <- 1
  }
  q <- apply(x, 2, quantile, na.rm = TRUE, probs = probs)
  avg <- colMeans(q)
  num <- rbind(q, avg)
  num <- apply(num, 2, function(x, a) {x * sqrt(a)}, a = a)
  num_fmt <- apply(num, 2, f_percent)
  fmt <- data.frame(Percentile = c(f_num(probs * 100, 0), 'Average'), num_fmt)
  res <- list()
  res$fmt <- fmt
  res$num <- num
  return(res)
}


#' @export
tbl_mv_reg <- function(fund, fact, rf, period = NULL, net_rf_y = TRUE,
                       net_rf_x = TRUE) {
  
  if (is.null(period)) {
    period <- periodicity(fund)$units
  }
  a <- freq_to_scaler(period)
  fit <- mv_reg(fund, fact, rf, period, net_rf_y, net_rf_x)
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
kbl_mv_reg <- function(fund, fact, rf, period = NULL, net_rf_y = TRUE,
                       net_rf_x = TRUE, t_stat_crit = 2) {
  
  tbl_list <- tbl_mv_reg(fund, fact, rf, period, net_rf_y, net_rf_x)
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
tbl_drawdowns <- function(fund, n_worst = 10) {
  
  tbl <- find_drawdowns(fund)
  tbl <- tbl[order(tbl$Drawdown), ]
  tbl <- tbl[1:n_worst, ]
  num <- tbl
  fmt <- tbl
  fmt$Drawdown <- f_percent(fmt$Drawdown)
  res <- list()
  res$fmt <- fmt
  res$num <- num
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