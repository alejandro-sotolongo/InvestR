#' @export
Portfolio <- R6::R6Class(
  classname = 'Portfolio',
  public = list(
    sec_xts = NULL,
    sec_tick = NULL,
    sec_comm_period = 'days',
    sec_meta = NULL,
    rebal_period = 'quarters',
    rebal_cost = 0,
    rebal_wgt = NULL,
    date_start = NULL,
    date_end = NULL,
    time_series = NULL,
    name = 'Port',
    port_index = NULL,
    asset_index = NULL,
    hist_wgt = NULL,
    last_wgt = NULL,
    bench = NULL,

    initialize = function(sec_xts = ...,
                          sec_tick = NULL,
                          sec_comm_period = NULL,
                          sec_meta = NULL,
                          use_blank_meta = TRUE,
                          run_load_meta = FALSE,
                          rebal_period = 'quarters',
                          rebal_cost = 0,
                          rebal_wgt = NULL,
                          date_start = NULL,
                          date_end = NULL,
                          name = 'Port') {

      if (length(list(sec_xts)) == 1 & is.null(sec_comm_period)) {
        ret <- sec_xts
      } else {
        ret <- combine_xts(sec_xts, period = sec_comm_period)
      }
      self$sec_xts <- ret
      if (is.null(sec_tick)) {
        self$sec_tick <- colnames(ret)
      }
      self$name <- name
      self$sec_comm_period <- sec_comm_period
      self$rebal_period <- rebal_period
      self$rebal_cost <- rebal_cost
      if (is.null(rebal_wgt)) {
        rebal_wgt <- rep(1 / ncol(ret), ncol(ret))
      }
      self$rebal_wgt <- rebal_wgt
      if (run_load_meta) {
        self$load_meta()
      } else if (use_blank_meta) {
        self$blank_meta()
      } else {
        self$sec_meta <- sec_meta
      }
      if (is.null(date_start)) {
        date_start <- as.Date('1970-01-01')
      }
      if (is.null(date_end)) {
        date_end <- Sys.Date()
      }
      self$date_start <- date_start
      self$date_end <- date_end
      self$time_series <- NULL
    },

    load_meta = function() {
      ss <- googlesheets::gs_key('1FwDPphSSDnEWKHFYnhEcThnKS0a7zufaVG0q_gFuLXo')
      gs <- googlesheets::gs_read(ss)
      meta <- merge(data.frame(Ticker = toupper(self$sec_tick)), gs, all.x = TRUE)
      if (any(is.na(meta$Name))) {
        warning('There are tickers not found in the meta-data file.')
      }
      self$sec_meta <- as.data.frame(meta)
    },

    blank_meta = function() {
      meta <- data.frame(Ticker = self$sec_tick, Name = NA, AssetClass = NA,
                         Strategy = NA, Geography = NA, Country = NA)
      self$sec_meta <- meta
    },

    auto_reb_wgt = function() {

      day_vec <- seq.Date(from = self$date_start, to = self$date_end, by = 'days')
      reb_wgt_mat <- matrix(self$rebal_wgt, nrow = length(day_vec),
                            ncol = length(self$rebal_wgt), byrow = TRUE)
      reb_wgt <- xts(reb_wgt_mat, day_vec)
      colnames(reb_wgt) <- colnames(self$sec_xts)
      self$rebal_wgt <- change_freq(reb_wgt, period = self$rebal_period,
                                  dtype = 'price')
    },

    rebal = function() {

      init_cap <- 100
      n_obs <- nrow(self$sec_xts)
      n_assets <- ncol(self$sec_xts)
      if (is.null(nrow(self$rebal_wgt))) {
        self$auto_reb_wgt()
      }
      if (length(self$rebal_cost) == 1) {
        self$rebal_cost <- rep(self$rebal_cost, n_assets)
      }
      asset_index <- matrix(0, nrow = n_obs + 1, ncol = n_assets)
      asset_index[1, ] <- init_cap * self$rebal_wgt[1, ]
      port_index <- matrix(0, nrow = n_obs + 1, ncol = 1)
      port_index[1, 1] <- init_cap
      rebal_dt <- zoo::index(self$rebal_wgt)
      sec_dt <- zoo::index(self$sec_xts)
      comm_start <- max(c(min(rebal_dt), min(sec_dt)))
      rebal_dt <- rebal_dt[rebal_dt >= comm_start]
      self$rebal_wgt <- self$rebal_wgt[paste0(comm_start, '/')]
      rebal_counter <- 1
      for (i in 1:n_obs) {
        is_rebal_dt <- sec_dt[i] >= rebal_dt[rebal_counter]
        if (is_rebal_dt) {
          adj_w <- self$rebal_wgt[rebal_counter, ] - self$rebal_cost
          asset_index[i, ] <- adj_w * port_index[i, 1]
          rebal_counter <- rebal_counter + 1
        }
        asset_index[i + 1, ] <- asset_index[i, ] * (1 + self$sec_xts[i, ])
        pnl <- sum(asset_index[i + 1, ]) - sum(asset_index[i, ])
        port_index[i + 1, 1] <- port_index[i, 1] + pnl
      }
      self$port_index <- xts(port_index, c(sec_dt[1] - 1, sec_dt))
      self$time_series <- price_to_ret(self$port_index)
      hist_wgt <- asset_index / rowSums(asset_index)
      self$hist_wgt <- xts(hist_wgt, c(sec_dt[1] - 1, sec_dt))
      self$asset_index <- xts(asset_index, c(sec_dt[1] - 1, sec_dt))
      is_alloc <- hist_wgt[nrow(hist_wgt), ] != 0
      last_wgt <- data.frame(
        Asset = self$sec_tick[is_alloc],
        Weight = as.numeric(hist_wgt[nrow(hist_wgt), is_alloc]))
      self$last_wgt <- last_wgt
    },

    contr_to_ret = function(date_start = NULL, date_end = NULL) {

      if (is.null(self$port_index)) {
        warning('the rebal function needs to be run first')
        return()
      }

      if (!is.null(date_start)) {
        index_dt <- zoo::index(self$asset_index)
        dt_before_start <- which(index_dt < date_start)
        index_dt_start <- index_dt[max(dt_before_start)]
      } else {
        index_dt_start <- NULL
      }
      asset_index <- zoo::coredata(trunc_xts(self$asset_index, index_dt_start,
                                             date_end))
      sec_mat <- zoo::coredata(trunc_xts(self$sec_xts, date_start, date_end))
      contr_mat <- asset_index[1:(nrow(asset_index) - 1), ] * sec_mat
      port_index_trunc <- trunc_xts(self$port_index, date_start, date_end)
      beg_val <- as.numeric(port_index_trunc[1, 1])
      last_val <- as.numeric(port_index_trunc[nrow(port_index_trunc), 1])
      contr <- colSums(contr_mat) / beg_val
      resid <- last_val / beg_val - 1 - sum(contr)
      res <- c(contr, resid)
      names(res) <- c(self$sec_tick, 'Resid.')
      return(res)
    },

    contr_to_risk = function(date_start = NULL, date_end = NULL, cov_mat = NULL) {

      if (is.null(cov_mat)) {
        cov_mat <- cov(trunc_xts(self$sec_xts), use = 'complete.obs')
      }
      x <- self$last_wgt$Weight
      if (is.null(x)) {
        x <- self$rebal_wgt
        if (!is.null(nrow(x))) {
          x <- x[nrow(x), ]
        }
      }
      (x * (cov_mat %*% x)) / (t(x) %*% cov_mat %*% x)[1]
    },

    pca_hclust = function(shrink = TRUE) {

      cor_mat <- cor(self$sec_xts, use = 'pairwise.complete.obs')
      if (shrink) {
        cor_mat <- corpcor::cor.shrink(cor_mat)
      }
      p <- princomp(covmat = cor_mat)
      meas <- diag(sqrt(p$sdev^2)) %*% t(p$loadings[,])
      dist_res <- dist(t(meas), method = 'euclidean')
      hclust(dist_res)
    }
  )
)
