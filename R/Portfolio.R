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

    initialize = function(sec_xts = ...,
                          sec_tick = NULL,
                          sec_comm_period = 'days',
                          sec_meta = NULL,
                          use_blank_meta = TRUE,
                          run_load_meta = FALSE,
                          rebal_period = 'quarters',
                          rebal_cost = 0,
                          rebal_wgt = NULL,
                          date_start = NULL,
                          date_end = NULL,
                          name = 'Port') {

      ret <- combine_xts(sec_xts, period = sec_comm_period)
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
    }
  )
)
