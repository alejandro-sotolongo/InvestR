library(CriticalLineAlgo)

tick <- c('VV', 'VB', 'VTV', 'VUG', 'VEA', 'VWO',
          'TLT', 'IEF', 'IEI', 'BNDX', 'EMB', 'VCIT', 'SCHP',
          'VNQ', 'IQDNX')
dat_list <- lapply(tick, download_tiingo, 
                   t_api = '9a562223a2edd4f842fefdb0c894abf795fb4342')
dat <- do.call(cbind, dat_list)
dat <- na.omit(dat)
xcov <- ewma_cov(dat['2018-01-01/2020-01-01'])
xcor <- cov2cor(xcov)
hc <- pca_hclust(xcor)
res <- risk_cluster_wgt(hc, sqrt(diag(xcov)), 2)

xcov <- xcov * 252
vol <- sqrt(diag(xcov))
mu <- vol * 1/3
names(mu) <- tick
store <- run_cla(mu, xcov, up_bound = rep(0.25, length(tick)), low_bound = rep(0, length(tick)))
corner_wgt <- do.call(cbind, store$wgt_list)
exp_ret <- apply(corner_wgt, 2, function(m, w) {t(w) %*% m}, m = mu)
exp_vol <- apply(corner_wgt, 2, function(covar, w) {sqrt(t(w) %*% covar %*% w)},
                 covar = xcov)
plot(vol, mu, col = 'darkgrey',
     ylab = 'Return', xlab = 'Volatility', xlim = c(0, .2), ylim = c(0, .1))
points(exp_vol, exp_ret, col = 'darkgreen', lwd = 2, type = 'b')
res_vol <- sqrt(t(res) %*% xcov %*% res)
res_mu <- t(res) %*% mu
points(res_vol, res_mu, col = 'red', lwd = 2)

out_mu <- apply(dat['2020-01-02/'] + 1, 2, function(x) {prod(x) - 1})
res_out_mu <- t(res) %*% out_mu
res_out_vol <- sqrt(t(res) %*% cov(dat['2020-01-02/']) %*% res) * sqrt(252)

adj_corner_wgt <- apply(corner_wgt, 2, function(x, y) {0.67 * x + 0.33 * y}, y = as.numeric(res))
exp_ret <- apply(adj_corner_wgt, 2, function(m, w) {t(w) %*% m}, m = mu)
exp_vol <- apply(adj_corner_wgt, 2, function(covar, w) {sqrt(t(w) %*% covar %*% w)},
                 covar = xcov)
points(exp_vol, exp_ret, col = 'darkblue', lwd = 2, type = 'b')
exp_ret <- apply(adj_corner_wgt, 2, function(m, w) {t(w) %*% m}, m = out_mu)
exp_vol <- apply(adj_corner_wgt, 2, function(covar, w) {sqrt(t(w) %*% cov(dat['2020-01-02/']) %*% w) * sqrt(252)},
                 covar = xcov)
points(exp_vol, exp_ret, col = 'orange', lwd = 2, type = 'b')
points(res_out_vol, res_out_mu, col = 'brown', lwd = 2, type = 'b')
