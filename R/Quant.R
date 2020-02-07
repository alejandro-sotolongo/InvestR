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
