#' @export
excess_cov <- function(x, rf) {

  exc_ret <- excess_ret(x, rf)
  cov(exc_ret, use = 'complete.obs')
}


#' @export
geo_ret <- function(x, use_busday = TRUE, period = NULL) {

  if (use_busday) {
    x <- busday(x)
  }
  if (is.null(period)) {
    period <- periodicity(ret)$units
  }
  a <- freq_to_scaler(period)
  if (is.null(a)) {
    stop(paset0('invalid period: ', period))
  }
  wealth <- apply(x + 1, 2, prod, na.rm = TRUE)
  wealth^(a / nrow(x)) - 1
}


#' @export
vol <- function(x, use_busday = TRUE, period = NULL) {

  if (use_busday) {
    x <- busday(x)
  }
  if (is.null(period)) {
    period <- periodicity(ret)$units
  }
  a <- freq_to_scaler(period)
  if (is.null(a)) {
    stop(paset0('invalid period: ', period))
  }
  period_sd <- apply(x, 2, sd, na.rm = TRUE)
  period_sd * sqrt(a)
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
  mu_vec <- vol * .25
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


#' @export
.drawdown_calc <- function(x) {

  wi <- cumprod(x + 1)
  wi_peak <- cummax(wi)
  wi / wi_peak - 1
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

