#' @export
excess_cov <- function(x, rf) {

  exc_ret <- excess_ret(x, rf)
  cov(exc_ret, use = 'complete.obs')
}


#' @export
mrf <- function(x, rf, period = NULL) {
  
  x_start <- zoo::index(x)[1]
  x_end <- zoo::index(x)[nrow(x)]
  if (x_start < zoo::index(rf)[1]) {
    stop('x begins before rf')
  }
  if (x_end > zoo::index(rf)[nrow(rf)]) {
    stop('x ends after rf')
  }
  rf_cut <- trunc_xts(rf, x_start, x_end)
  x_mu <- geo_ret(x, period)
  rf_mu <- geo_ret(rf_cut, period)
  x_mu - rf_mu
}


#' @export
geo_ret <- function(x, period = NULL, remove_na = TRUE) {

  if (is.null(period)) {
    period <- periodicity(x)$units
  }
  a <- freq_to_scaler(period)
  if (is.null(a)) {
  }
  wealth <- apply(x + 1, 2, prod, na.rm = remove_na)
  wealth^(a / nrow(x)) - 1
}


#' @export
cum_ret <- function(x, remove_na = TRUE) {

  wealth <- apply(x + 1, 2, prod, na.rm = remove_na)
  wealth - 1
}


#' @export
vol <- function(x, period = NULL, remove_na = TRUE, annualize = TRUE) {

  if (is.null(period)) {
    period <- periodicity(x)$units
  }
  a <- freq_to_scaler(period)
  if (is.null(a)) {
    stop(paste0('invalid period: ', period))
  }
  period_sd <- apply(x, 2, sd, na.rm = remove_na)
  if (annualize) {
    return(period_sd * sqrt(a))
  } else {
    return(period_sd)
  }
}


#' @export
down_vol <- function(x, mar = 0, period = NULL, remove_na = TRUE, 
                     annualize = TRUE) {
  
  if (is.null(period)) {
    period <- periodicity(x)$units
  }
  a <- freq_to_scaler(period)
  if (is.null(a)) {
    stop(paste0('invalid period: ', period))
  }
  down_sd <- apply(x, 2, .down_vol_wrap, mar = mar, remove_na = remove_na)
  if (annualize) {
    return(down_sd * sqrt(a))
  } else {
    return(down_sd)
  }
}


.down_vol_wrap <- function(x, mar, remove_na) {
  is_down <- x < mar
  sd(x[is_down], na.rm = remove_na)
}

#' @export
sharpe_ratio <- function(x, rf, period = NULL) {
  
  mrf(x, rf, period) / vol(x, period)
}


#' @export
sortino_ratio <- function(x, mar = 0, period = NULL) {
  
  (geo_ret(x, period = period) - mar) / down_vol(x, mar = mar, period = period)
}


#' @export
up_down_capt <- function(x, y) {
  
  if (ncol(y) > 1) {
    stop('y needs to be a univariate xts')
  }
  combo <- combine_xts(x, y, use_busday = FALSE)
  is_y_up <- combo[, ncol(combo)] > 0
  combo_split <- split(combo, is_y_up)
  up <- geo_ret(combo_split[[2]])
  down <- geo_ret(combo_split[[1]])
  up_capture <- up / up[length(up)]
  down_capture <- down / down[length(down)]
  res <- list()
  res$up <- up_capture
  res$down <- down_capture
  return(res)
}


#' @export
omega_ratio <- function(x, mar = 0, method = c('weight', 'standard'), 
                        remove_na = TRUE) {
  
  if (remove_na) {
    x <- na.omit(x)
  }
  apply(x, 2, .omega_ratio_wrap, mar = mar, method = method)  
}


.omega_ratio_wrap <- function(x, mar, method = c('weight', 'standard')) {
  
  d <- density(x)
  above_mar <- d$x > mar
  method <- tolower(method)[1]
  if (method == 'weight') {
    d$x <- d$x - mar
    up_area <- t(d$y[above_mar]) %*% d$x[above_mar]
    down_area <- t(d$y[!above_mar]) %*% d$x[!above_mar]
    res <- up_area / -down_area
    return(res)[1]
  } else {
    cum_y_scale <- cumsum(d$y) / sum(d$y)
    down_area <- min(cum_y_scale[above_mar])
    up_area <- 1 - down_area
    return(up_area / down_area)
  }
}


#' @export
mv_reg <- function(fund, fact, rf, period = NULL) {
  
  n_funds <- ncol(fund)
  n_facts <- ncol(fact)
  if (ncol(rf) > 1) {
    stop('rf must be 1 time-series')
  }
  x <- combine_xts(fund, fact, rf, period = period)
  fund_exc <- excess_ret(x[, 1:n_funds], x[, ncol(x)])
  fact_exc <- excess_ret(x[, (n_funds + 1):(ncol(x) - 1)], x[, ncol(x)])
  fit <- list()
  for (i in 1:n_funds) {
    fit[[i]] <- lm(fund_exc[, i] ~ fact_exc)
  }
  return(fit)
}


#' @export
pca_hclust <- function(cor_mat) {

  p <- princomp(covmat = cor_mat)
  meas <- diag(sqrt(p$sdev^2)) %*% t(p$loadings[,])
  dist_res <- dist(t(meas), method = 'euclidean')
  hclust(dist_res)
}


#' @export
risk_cluster_wgt <- function(hc, vol, k = 2) {

  n_assets <- max(hc$order)
  memb <- cutree(hc, k)
  xcor <- diag(1, n_assets, n_assets)
  for (i in 1:k) {
    xcor[memb == i, memb == i] <- 1
  }
  vol <- matrix(vol, ncol = 1)
  xcov <- vol %*% t(vol) * xcor
  mu_vec <- vol * 0.25
  cov_inv <- MASS::ginv(xcov)
  (cov_inv %*% mu_vec) /
    (matrix(1, ncol = length(hc$order), nrow = 1) %*% cov_inv %*% mu_vec)[1]
}


#' @export
drawdown <- function(x) {

  x <- na.omit(x)
  dd <- apply(x, 2, .drawdown_calc)
  .return_xts(dd)
}

.drawdown_calc <- function(x) {

  wi <- cumprod(x + 1)
  wi_peak <- cummax(wi)
  wi / wi_peak - 1
}


#' @export
find_drawdowns <- function(x) {
  
  if (ncol(x) > 1) {
    warning('x needs to be univariate, taking the first column')
    x <- x[, 1]
  }
  dd <- drawdown(x)
  dd <- xts_to_dataframe(dd)
  colnames(dd) <- c('Date', 'Drawdown')
  dd$isDown <- dd[, 2] < 0
  dd$isDownLag <- c(NA, dd[1:(nrow(dd) - 1), 'isDown'])
  dd$start <- dd$isDown & !dd$isDownLag # change from 0 to negative signals drawdown start
  dd$end <- !dd$isDown & dd$isDownLag # change from negative back to 0 signals recovery
  start_date <- dd[dd$start, 1]
  end_date <- dd[dd$end, 1]
  # if lengths of start and end dates are the same the time-series ends on
  # a drawdown and we need to adjust last end date to last start date
  if (length(end_date) == length(start_date)) {
    dd_date <- data.frame(StartDate = start_date,
                          EndDate = c(end_date[2:length(end_date)],
                                      start_date[length(start_date)]))
  } else {
    dd_date <- data.frame(StartDate = start_date,
                          EndDate = end_date[2:length(end_date)])
  }
  # create unique list of drawdowns
  dd_list <- mapply(trunc_df,
                    df = list(dd[, 1:2]),
                    date_start = dd_date$StartDate,
                    date_end = dd_date$EndDate,
                    SIMPLIFY = FALSE)
  .get_min_dd <- function(x) {
    # find trough by finding which observation equals the minimum
    indx <- which(x[, 2] == min(x[, 2]))
    # if there are more than one observations equal to the minimum take the first one
    if (length(indx) > 1) {
      indx <- indx[1]
    }
    x[indx, ]
  }
  trough_list <- lapply(dd_list, .get_min_dd)
  trough_df <- do.call(rbind, trough_list)
  res <- data.frame(StartDate = dd_date$StartDate,
                    TroughDate = trough_df$Date,
                    EndDate = dd_date$EndDate,
                    Trough = trough_df$Drawdown,
                    DaysToTrough = trough_df$Date - dd_date$StartDate,
                    DaysToRecover = dd_date$EndDate - trough_df$Date,
                    TotalDays = dd_date$EndDate - dd_date$StartDate)
  colnames(res)[4] <- 'Drawdown'
  return(res)
}



#' @export
roll_vol <- function(x, roll_win = 63, period = NULL, remove_na = TRUE) {
  
  if (is.null(period)) {
    period <- periodicity(x)$units
  }
  a <- freq_to_scaler(period)
  if (is.null(a)) {
    stop(paste0('invalid period: ', period))
  }
  df <- data.frame(x)
  vol_list <- slider::slide(df, ~.calc_vol_wrap(.x), .complete = TRUE, .before = roll_win)
  vol <- do.call('rbind', vol_list)
  vol_ann <- apply(vol, 2, function(x, a) {x * sqrt(a)}, a = a)
  xts(vol_ann, as.Date(rownames(vol_ann)))
}

.calc_vol_wrap <- function(x) {
  apply(x, 2, sd)
}


#' @export
roll_turb <- function(ret, roll_win = 156, z_win_short = 5, z_win_long = 156) {

  roll_mahal_list <- slider::slide(ret, ~.calc_turb_wrap(.x),
                                  .complete = TRUE, .before = roll_win)
  roll_mahal <- unlist(roll_mahal_list)
  delta_mahal_list <- slider::slide(roll_mahal, ~zscore_win(.x, z_win_short),
                                   .complete = TRUE, .before = z_win_long)
  delta_mahal <- unlist(delta_mahal_list)
  perc_mahal <- rank(roll_mahal) / (1 + length(roll_mahal))
  res <- cbind(roll_mahal, c(rep(NA, z_win_long), delta_mahal), perc_mahal)
  colnames(res) <- c('Turb', 'DeltaTurb', 'PercentileTurb')
  xts(res, as.Date(zoo::index(ret)[(roll_win + 1):nrow(ret)]))
}

.calc_turb_wrap <- function(ret) {

  xret <- filter_na_col(ret, eps = 5)
  res <- .mahal(xret, colMeans(xret, na.rm = TRUE),
                cov(xret, use = 'complete.obs'))
  return(res[length(res)])
}

.mahal <- function(x, center, cov) {

  x <- sweep(x, 2L, center)
  rowSums(x %*% cov * x)
}

roll_forward_ret <- function(ret, roll_win) {

}

#' @export
roll_absorp <- function(ret, n_pc = 2, roll_win = 504, lambda = NULL,
                        z_win_short = 15, z_win_long = 252) {
  if (is.null(lambda)) {
    freq <- xts::periodicity(ret)
    lambda <- 1 - 2 / (roll_win + 1)
  }
  roll_ar_list <- slider::slide(ret, ~.calc_absorp_wrap(.x, lambda = lambda),
                               .complete = TRUE, .before = roll_win)
  roll_ar <- unlist(roll_ar_list)
  delta_ar_list <- slider::slide(roll_ar, ~zscore_win(.x, z_win_short),
                                .complete = TRUE, .before = z_win_long)
  delta_ar <- unlist(delta_ar_list)
  perc_ar <- rank(roll_ar) / (length(roll_ar) + 1)
  res <- cbind(roll_ar, c(rep(NA, z_win_long), delta_ar), perc_ar)
  colnames(res) <- c('AR', 'DeltaAR', 'PercentileAR')
  xts(res, as.Date(zoo::index(ret)[(roll_win + 1):nrow(ret)]))
}

#' @export
.calc_absorp_wrap <- function(ret, n_pc = 2, lambda = NULL) {
  xret <- filter_na_col(ret, eps = 5)
  xret[is.na(xret)] <- 0
  cov_mat <- ewma_cov(xret, lambda)
  calc_absorp(cov_mat, n_pc)
}

#' @export
calc_absorp <- function(cov_mat, n_pc = NULL) {
  if (is.null(n_pc)) {
    n_pc <- ceiling(0.2 * nrow(cov_mat))
  }
  eig <- svd(cov_mat)
  sum(eig$d[1:2]) / sum(eig$d)
}

#' @export
filter_na_col <- function(ret, eps = 10) {
  na_count <- colSums(is.na(ret))
  ret[, na_count < eps]
}

#' @export
ewma_cov <- function(ret, lambda = NULL) {
  if (is.null(lambda)) {
    freq <- xts::periodicity(ret)
    lambda <- 1 - 2 / (freq_to_scaler(freq$units))
  }
  n_obs <- nrow(ret)
  cov_mat <- cov(ret)
  mu <- colMeans(ret)
  ret_centered <- sweep(ret, 2, mu, '-')
  for (obs in 1:n_obs) {
    r <- ret_centered[obs, ]
    rr <- t(r) %*% r
    cov_mat <- (1 - lambda) / (1 + lambda^n_obs) * rr + lambda * cov_mat
  }
  return(cov_mat)
}


#' @export
zscore_win <- function(x, short_win) {
  t0 <- length(x)
  t_short <- t0 - short_win - 1
  (mean(x[t0:t_short]) - mean(x)) / sd(x)
}


#' @export
pca_cov <- function(cov_mat) {
  s <- svd(cov_mat)
  latent <- s$d
  raw_coeff <- s$v
  p <- dim(raw_coeff)[1]
  d <- dim(raw_coeff)[2]
  max_index <- max.col(t(abs(raw_coeff)))
  col_sign <- sign(raw_coeff[max_index + seq(from = 0, to = (d - 1) * p, by = p)])
  coeff <- matrix(col_sign, nrow = p, ncol = d, byrow = TRUE) * raw_coeff
  res <- list()
  res$latent <- latent
  res$coeff <- coeff
  return(res)
}


#' @export
roll_style_analysis <- function(fund, fact, period = 'week', roll_period = 156, 
                                .step = 1L) {
  
  x <- combine_xts(fund, fact, period = period, use_busday = FALSE)
  roll_list <- slider::slide(x, ~style_analysis(.x[, 1], .x[, 2:ncol(x)]),
                             .before = roll_period, .complete = TRUE, 
                             .step = .step)
  roll_mat <- do.call('rbind', roll_list) 
  roll_xts <- xts(roll_mat, zoo::index(x)[(roll_period + 1):nrow(x)])
  colnames(roll_xts) <- colnames(fact)
  return(roll_xts)
}


#' @export
roll_style_analysis_with_na <- function(fund, fact, period = 'week',
                                        roll_period = 156, na_eps = 0) {
  
  x <- cbind(fund, fact)
  date_start <- max(zoo::index(fund)[1], zoo::index(fact)[1])
  date_end <- min(zoo::index(fund)[nrow(fund)], zoo::index(fund)[nrow(fund)])
  x_trunc <- trunc_xts(x, date_start, date_end)
  ret <- change_freq_na(x_trunc, period, 'return')
  res <- matrix(nrow = nrow(ret), ncol = ncol(fact))
  for (i in roll_period:nrow(ret)) {
    roll_ret <- ret[(i - roll_period + 1):i, ]
    na_per_col <- apply(roll_ret, 2, is.na)
    is_na_col <- colSums(na_per_col) > na_eps
    roll_ret_clean <- roll_ret[, !is_na_col]
    roll_ret_clean[is.na(roll_ret_clean)] <- 0
    wgt <- style_analysis(roll_ret_clean[, 1], roll_ret_clean[, 2:ncol(roll_ret_clean)])
    res[i, !is_na_col[2:ncol(roll_ret)]] <- wgt
  }
  wgt_xts <- xts(res, as.Date(zoo::index(ret)))
  colnames(wgt_xts) <- colnames(fact)
  return(wgt_xts)
}


#' @export
style_analysis <- function(fund, fact) {
  
  res <- track_error_min_qp(fund, fact)
  res$solution
}


#' @export
track_error_min_qp <- function(fund, fact) {
  
  n_fact <- ncol(fact)
  cov_fact <- cov(fact)
  cov_vec <- matrix(nrow = n_fact, ncol = 1)
  for (i in 1:n_fact) {
    cov_vec[i, 1] <- cov(fact[, i], fund)
  }
  a_mat_t <- rbind(rep(1, n_fact), diag(-1, n_fact), diag(1, n_fact))
  a_mat <- t(a_mat_t)
  b_0 <- c(1, rep(-1, n_fact), rep(0, n_fact))
  res <- quadprog::solve.QP(cov_fact, cov_vec, a_mat, bvec = b_0)
  return(res)
}


test_track_error <- function(fund, fact, res) {
  
  c_part <- 0
  for (i in 1:ncol(fact)) {
    c_part <- c_part + res$recons[i] * fund
  }
  t_ind <- fund - c_part 
  return(t_ind)
}


