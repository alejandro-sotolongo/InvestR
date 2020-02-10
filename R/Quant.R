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
  wgt <- (cov_inv %*% mu_vec) / (matrix(1, ncol = 16, nrow = 1) %*% cov_inv %*% mu_vec)[1] 
  return(data.frame(wgt, memb))
}
